#pragma once

#include "classInfo.h"
#include "environment.h"
#include "lexer.h"
#include "llvm-13/llvm/IR/IRBuilder.h"
#include "llvm-13/llvm/IR/LLVMContext.h"
#include "llvm-13/llvm/IR/Module.h"
#include "llvm-13/llvm/IR/Verifier.h"
#include "parser.h"
#include "llvm/IR/Instructions.h"
#include "llvm/Support/Alignment.h"
#include <algorithm>
#include <iterator>
#include <memory>
#include <string>
#include <vector>

#define todo() assert(false && "Unimplemented")

using Env_t = std::shared_ptr<Environment>;

class SchemeLLVM {
public:
  SchemeLLVM() {
    moduleInit();
    setupExternFunctions();
    setupGlobalEnvironment();
  }

  void exec(const std::string &program) {
    auto lexer = std::make_unique<Lexer>();
    auto tokens = lexer->lex(program);
    auto parser = std::make_unique<Parser>(tokens);
    auto programNode = parser->parse();
    compile(programNode, globalEnv);
    saveModuleToFile("./out.ll");
  }

private:
  std::unique_ptr<llvm::LLVMContext> ctx;
  std::unique_ptr<llvm::Module> module;
  std::unique_ptr<llvm::IRBuilder<>> builder;
  std::shared_ptr<Environment> globalEnv;
  std::unique_ptr<llvm::IRBuilder<>>
      varsBuilder; // Additional builder for start of function
  llvm::Function *fn;
  llvm::StructType *currentClass =
      nullptr; // Adding it here so as to not pass it into each function
  std::map<std::string, std::shared_ptr<ClassInfo>> classTable;

  void moduleInit() {
    ctx = std::make_unique<llvm::LLVMContext>();
    module = std::make_unique<llvm::Module>("schemeLLVM", *ctx);
    builder = std::make_unique<llvm::IRBuilder<>>(*ctx);
    varsBuilder = std::make_unique<llvm::IRBuilder<>>(*ctx);
  }

  void saveModuleToFile(const std::string &fileName) {
    std::error_code errorCode;
    llvm::raw_fd_ostream outLL(fileName, errorCode);
    module->print(outLL, nullptr);
  }

  void compile(std::shared_ptr<ProgramNode> programNode, Env_t env) {
    fn = createFunction("main",
                        llvm::FunctionType::get(builder->getInt32Ty(), false),
                        globalEnv);
    gen(programNode, env, fn);
    builder->CreateRet(builder->getInt32(0));
  }

  void gen(std::shared_ptr<ProgramNode> programNode, Env_t env,
           llvm::Function *oldFunc) {
    auto statements = programNode.get()->statements;
    for (auto it = statements.begin(); it < statements.end(); it++) {
      genStatement(*it, env, oldFunc, &fn->getEntryBlock());
      fn = oldFunc; // in case statement is a function, you need to
                    // return the insert point to the older function
    }
  }
  void allocVarInBlock(std::string name, llvm::Type *type, Env_t env,
                       llvm::Value *exprValue, llvm::BasicBlock *block) {
    varsBuilder->SetInsertPoint(block);
    auto varAlloc = varsBuilder->CreateAlloca(type, 0, name.c_str());
    builder->CreateStore(exprValue, varAlloc);
    env->set(name, varAlloc);
  }
  void allocVar(std::string name, llvm::Type *type, Env_t env,
                llvm::Value *exprValue) {
    varsBuilder->SetInsertPoint(&fn->getEntryBlock());
    auto varAlloc = varsBuilder->CreateAlloca(type, 0, name.c_str());
    builder->CreateStore(exprValue, varAlloc);
    env->set(name, varAlloc);
  }

  llvm::Type *getLLVMTypeFromLangType(TokenType langType) {
    switch (langType) {
    case TokenType::IntType: {
      return dynamic_cast<llvm::Type *>(builder->getInt32Ty());
    }
    case TokenType::StringType: {
      return dynamic_cast<llvm::Type *>(builder->getInt8Ty()->getPointerTo());
    }
    case TokenType::VoidType: {
      return builder->getVoidTy();
    }
    case TokenType::Class: {
      return dynamic_cast<llvm::Type *>(currentClass->getPointerTo());
    }
    default: {
      assert(false && "Invalid type");
    }
    }
  }

  void genStatement(std::shared_ptr<StatementNode> stmt, Env_t env,
                    llvm::Function *oldFunc, llvm::BasicBlock *block) {
    switch (stmt->type) {
    case StatementType::Let: {
      auto letStatement = stmt->letStatement;
      auto exprValue = genExpr(letStatement->value, env);
      allocVarInBlock(*(letStatement->identifier), exprValue->getType(), env,
                      exprValue, block);
      // llvm::Value* value = genExpr(letStatement->value);
      // module->getOrInsertGlobal(letStatement->identifier,
      //                           value->getType());
      // auto variable =
      //     module->getNamedGlobal(letStatement->identifier);
      // variable->setAlignment(llvm::MaybeAlign(4));
      // variable->setConstant(false);
      // variable->setInitializer((llvm::Constant*)value);
      break;
    }
    case StatementType::Expression: {
      genExpr(stmt->expressionStatement->expression, env);
      break;
    }
    case StatementType::If: {
      // INFO: Should if return expressions? If yes, need to add phi
      // functions
      auto condition = genExpr(stmt->ifStatement->condition, env);
      auto ifTrueBlock = createBB("ifTrue", fn);
      auto ifFalseBlock = createBB("ifFalse", fn);
      auto ifEndBlock = createBB("ifEnd", fn);
      builder->CreateCondBr(condition, ifTrueBlock, ifFalseBlock);
      builder->SetInsertPoint(ifTrueBlock);
      auto trueBlock = stmt->ifStatement->trueBlock;
      for (auto statementIt = trueBlock->statements.begin();
           statementIt < trueBlock->statements.end(); statementIt++) {
        genStatement(*statementIt, env, oldFunc, block);
      }
      builder->CreateBr(ifEndBlock);
      builder->SetInsertPoint(ifFalseBlock);
      auto falseBlock = stmt->ifStatement->falseBlock;
      for (auto statementIt = falseBlock->statements.begin();
           statementIt < falseBlock->statements.end(); statementIt++) {
        genStatement(*statementIt, env, oldFunc, block);
      }
      builder->CreateBr(ifEndBlock);
      builder->SetInsertPoint(ifEndBlock);
      break;
    }
    case StatementType::For: {
      auto forEnv = std::make_shared<Environment>(
          std::map<std::string, llvm::AllocaInst *>(), env);
      genStatement(stmt->forStatement->initialCondition, forEnv, oldFunc,
                   block);
      auto forConditionBlock = createBB("forCondition", fn);
      builder->CreateBr(forConditionBlock);
      auto forBody = createBB("forBody", fn);
      auto forEnd = createBB("forEnd", fn);
      auto forUpdateBlock = createBB("forUpdateBlock", fn);
      builder->SetInsertPoint(forConditionBlock);
      auto condition = genExpr(stmt->forStatement->continueCondition, forEnv);
      builder->CreateCondBr(condition, forBody, forEnd);
      builder->SetInsertPoint(forBody);
      for (auto statement : stmt->forStatement->body->statements) {
        genStatement(statement, forEnv, oldFunc, forBody);
      }
      builder->CreateBr(forUpdateBlock);
      builder->SetInsertPoint(forUpdateBlock);
      genStatement(stmt->forStatement->updateCondition, forEnv, oldFunc, block);
      builder->CreateBr(forConditionBlock);
      builder->SetInsertPoint(forEnd);
      break;
    }
    case StatementType::While: {
      auto whileEnv = std::make_shared<Environment>(
          std::map<std::string, llvm::AllocaInst *>(), env);
      auto whileConditionBlock = createBB("whileCondition", fn);
      builder->CreateBr(whileConditionBlock);
      builder->SetInsertPoint(whileConditionBlock);
      auto condition = genExpr(stmt->whileStatement->condition, whileEnv);
      auto whileBody = createBB("whileBody", fn);
      auto whileEnd = createBB("whileEnd", fn);
      builder->CreateCondBr(condition, whileBody, whileEnd);
      builder->SetInsertPoint(whileBody);
      for (auto statement : stmt->whileStatement->body->statements) {
        genStatement(statement, whileEnv, oldFunc, whileBody);
      }
      builder->CreateBr(whileConditionBlock);
      ;
      builder->SetInsertPoint(whileEnd);
      break;
    }
    case StatementType::Function: {
      std::vector<llvm::Type *> paramTypes{};
      for (auto arg : stmt->funcStatement->arguments) {
        llvm::Type *type = getLLVMTypeFromLangType(arg->type);
        paramTypes.emplace_back(type);
      }

      auto newFuncEnv = std::make_shared<Environment>(
          std::map<std::string, llvm::AllocaInst *>(), env);
      auto functionReturnType =
          getLLVMTypeFromLangType(stmt->funcStatement->returnType);
      auto newFunc = createFunction(
          stmt->funcStatement->name,
          llvm::FunctionType::get(functionReturnType, paramTypes, false),
          newFuncEnv);
      fn = newFunc;
      auto statements = stmt->funcStatement->body->statements;
      int i = 0; // INFO: fn->args() gives an iterator thus this
                 // shabby loop
      for (auto &arg : fn->args()) {
        auto argFromAST = stmt->funcStatement->arguments[i++];
        arg.setName(*(argFromAST->name));
        allocVar(*(argFromAST->name), arg.getType(), newFuncEnv, &arg);
      }
      for (auto statementIt = statements.begin();
           statementIt < statements.end(); statementIt++) {
        genStatement(*statementIt, newFuncEnv, oldFunc,
                     &newFunc->getEntryBlock());
      }
      std::string constructorSuffix = "constructor";
      auto functionName = stmt->funcStatement->name;
      if (functionName.size() >= constructorSuffix.size()) {
        if (functionName.compare(functionName.size() - constructorSuffix.size(),
                                 constructorSuffix.size(),
                                 constructorSuffix) == 0) {

          auto identifierExpression = std::make_shared<IdentifierExpression>(
              std::make_shared<std::string>("this"));
          auto returnExpression =
              std::make_shared<ExpressionNode>(identifierExpression);
          auto returnStatementConstructor =
              std::make_shared<ReturnStatement>(returnExpression);
          auto statementNodeForReturn =
              std::make_shared<StatementNode>(returnStatementConstructor);
          genStatement(statementNodeForReturn, newFuncEnv, oldFunc,
                       &newFunc->getEntryBlock());
        }
      }
      builder->SetInsertPoint(&oldFunc->getEntryBlock());
      break;
    }
    case StatementType::Block: {
      assert(false && "Should'nt hit this");
    }
    case StatementType::Return: {
      auto returnVal = stmt->returnStatement->returnValue;
      auto returnExp = genExpr(returnVal, env);
      builder->CreateRet(returnExp);
      break;
    }
    case StatementType::Class: {
      auto classStatement = stmt->classStatement;
      auto className = *(classStatement->name);
      auto classGenerated = llvm::StructType::create(*ctx, className);
      currentClass = classGenerated;
      auto classEnv = std::make_shared<Environment>(
          std::map<std::string, llvm::AllocaInst *>{}, env);

      std::vector<llvm::Type *> classAttributeTypes;

      classTable[className] =
          std::make_shared<ClassInfo>(classGenerated, classEnv);

      for (auto attribute : classStatement->attributes) {
        (*(classTable[className]->attributes))[*(attribute->name)] =
            getLLVMTypeFromLangType(attribute->type);
        classAttributeTypes.push_back(getLLVMTypeFromLangType(attribute->type));
      }

      for (auto method : classStatement->methods) {
        assert(method->type == StatementType::Function &&
               "Class method should be a function");
        auto functionStmt = method->funcStatement;
        method->funcStatement->name =
            className + "_" + method->funcStatement->name;

        method->funcStatement->arguments.push_back(
            std::make_shared<FunctionArg>(
                TokenType::Class, std::make_shared<std::string>("this")));

        std::vector<llvm::Type *> paramTypes;
        std::transform(
            functionStmt->arguments.begin(), functionStmt->arguments.end(),
            std::back_inserter(paramTypes),
            [this](std::shared_ptr<FunctionArg> arg) -> llvm::Type * {
              return getLLVMTypeFromLangType((*arg).type);
            });

        (*(classTable[className]->methods))[functionStmt->name] =
            createFunctionProto(
                method->funcStatement->name,
                llvm::FunctionType::get(
                    getLLVMTypeFromLangType(functionStmt->returnType),
                    paramTypes, false),
                env);
      }

      currentClass->setBody(classAttributeTypes, false);

      for (auto method : classStatement->methods) {
        genStatement(method, classEnv, oldFunc, &fn->getEntryBlock());
      }

      break;
    }
    }
  }

  std::string getClassFromMethodPrefix() {
    std::string fnName = builder->GetInsertBlock()->getName().str();
    size_t underscorePos = fnName.find('_');
    if (underscorePos != std::string::npos) {
      return fnName.substr(0, underscorePos);
    } else {
      return fnName;
    }
  }

  llvm::Value *genExpr(std::shared_ptr<ExpressionNode> expressionNode,
                       Env_t env) {
    llvm::Value *dummy;
    switch (expressionNode->type) {
    case ExpressionType::String: {
      auto str =
          builder->CreateGlobalStringPtr(expressionNode->stringExp->value);
      return str;
    }
    case ExpressionType::Number: {
      auto num = builder->getInt32(expressionNode->integerExp->intValue);
      return num;
    }
    case ExpressionType::Infix: {
      auto rhs = genExpr(expressionNode->infixExpr->rhs, env);
      switch (expressionNode->infixExpr->op) {
      case TokenType::Plus: {
        auto lhs = genExpr(expressionNode->infixExpr->lhs, env);
        return builder->CreateAdd(lhs, rhs, "tmpAdd");
      }
      case TokenType::Minus: {
        auto lhs = genExpr(expressionNode->infixExpr->lhs, env);
        return builder->CreateSub(lhs, rhs, "tmpMinus");
      }
      case TokenType::LessThan: {
        auto lhs = genExpr(expressionNode->infixExpr->lhs, env);
        return builder->CreateICmpULT(lhs, rhs, "tmpLessThan");
      }
      case TokenType::GreaterThan: {
        auto lhs = genExpr(expressionNode->infixExpr->lhs, env);
        return builder->CreateICmpUGT(lhs, rhs, "tmpGreaterThan");
      }
      default: {
        assert(false && "Invalid operand for infix");
      }
      }
      break;
    }
    case ExpressionType::Member: {
      assert(expressionNode->memberExpression->lhs->type ==
             ExpressionType::Identifier);
      llvm::Value *lhs = genExpr(expressionNode->memberExpression->lhs, env);
      auto loadInst = llvm::dyn_cast<llvm::LoadInst>(lhs);
      auto className = (llvm::dyn_cast<llvm::PointerType>(loadInst->getType()))
                           ->getElementType()
                           ->getStructName()
                           .str();
      ExpressionType rhsType = expressionNode->memberExpression->rhs->type;

      assert(rhsType == ExpressionType::Identifier &&
             "Should be an identifier on the rhs of a member expression");
      std::string fieldName =
          *(expressionNode->memberExpression->rhs->identifierExpression->name);
      if (rhsType == ExpressionType::Call) {
        todo();
      } else if (rhsType == ExpressionType::Identifier) {
        // assert(classInfo->attributes != nullptr && "Should not be null");
        llvm::StructType *structInstance =
            (llvm::StructType *)(lhs->getType()->getContainedType(0));
        auto indexOfField = getFieldIndex(structInstance, fieldName);
        auto address = builder->CreateStructGEP(structInstance, lhs,
                                                indexOfField, "p" + fieldName);

        std::shared_ptr<ClassInfo> classInformation;
        assert(classTable.find(className) != classTable.end());
        classInformation = (classTable.find(className)->second);
        auto load = builder->CreateLoad(
            structInstance->getElementType(indexOfField), address, fieldName);
        classInformation->classEnv->set(fieldName, (llvm::AllocaInst *)address);
        return load;
      } else {
        assert(false && "Please don't do this, please");
      }
    }
    case ExpressionType::Assignment: {
      auto lhsType = expressionNode->assignmentExpression->lhs->type;
      assert(lhsType == ExpressionType::Identifier ||
             lhsType == ExpressionType::Member && "Invalid lvalue");
      auto rhs = genExpr(expressionNode->assignmentExpression->rhs, env);
      llvm::AllocaInst *pointerToLhs;
      if (expressionNode->assignmentExpression->lhs->type ==
          ExpressionType::Identifier) {
        genExpr(expressionNode->assignmentExpression->lhs, env);
        pointerToLhs = env->get(*(expressionNode->assignmentExpression->lhs
                                      ->identifierExpression->name));

      } else if (expressionNode->assignmentExpression->lhs->type ==
                 ExpressionType::Member) {
        auto className =
            (llvm::dyn_cast<llvm::GetElementPtrInst>(
                 (llvm::dyn_cast<llvm::LoadInst>(
                      genExpr(expressionNode->assignmentExpression->lhs, env))
                      ->getPointerOperand())))
                ->getSourceElementType()
                ->getStructName()
                .str();
        auto classEnv = classTable.find(className)->second->classEnv;
        pointerToLhs = classEnv->get(
            *(expressionNode->assignmentExpression->lhs->memberExpression->rhs
                  ->identifierExpression->name));
        assert(pointerToLhs != nullptr && "Variable not an attribute of class");
      }
      return builder->CreateStore(rhs, pointerToLhs);
    }
    case ExpressionType::New: {
      auto className = *(expressionNode->newExpression->className);
      auto classInfo = classTable[className];
      if (classInfo == nullptr) {
        assert(false && "Class is undefined");
      }
      auto typeSize = builder->getInt64(getTypeSize(classInfo->classGenerated));
      auto mallocPtr = builder->CreateCall(module->getFunction("GC_malloc"),
                                           typeSize, className);
      auto instance = builder->CreatePointerCast(
          mallocPtr, classInfo->classGenerated->getPointerTo());
      // get the constructor function
      auto constructor = module->getFunction(className + "_constructor");
      std::vector<llvm::Value *> evaluatedArguments;
      for (auto arg : (expressionNode->newExpression->arguments)) {
        evaluatedArguments.push_back(genExpr(arg, env));
      }
      evaluatedArguments.push_back(instance);
      builder->CreateCall(constructor, evaluatedArguments);
      return instance;
    }
    case ExpressionType::Identifier: {
      auto identifier = env->get(*(expressionNode->identifierExpression->name));
      auto identifierCastForLoading =
          llvm::dyn_cast<llvm::AllocaInst>(identifier);
      if (llvm::GetElementPtrInst *gepInst =
              llvm::dyn_cast<llvm::GetElementPtrInst>(identifier)) {
        // REFACTOR: HACK for getting gep arguments in environment working
        return builder->CreateLoad(
            gepInst->getType()->getPointerElementType(),
            identifierCastForLoading,
            (expressionNode->identifierExpression->name)->c_str());
      }
      return builder->CreateLoad(
          identifierCastForLoading->getAllocatedType(),
          identifierCastForLoading,
          (expressionNode->identifierExpression->name)->c_str());
      break;
    }
    case ExpressionType::Call: {
      llvm::Function *function;
      function =
          (module->getFunction(*(expressionNode->callExpression->fnName)));
      auto evaledExpressions = std::vector<llvm::Value *>();
      for (auto arg : expressionNode->callExpression->arguments) {
        auto evaledArg = genExpr(arg, env);
        evaledExpressions.emplace_back(evaledArg);
      }
      auto call =
          builder->CreateCall((llvm::Function *)(function), evaledExpressions);
      return call;
    }
    default:
      assert(false && "Should'nt hit here");
    }
    return dummy;
  }

  size_t getTypeSize(llvm::Type *type) {
    return module->getDataLayout().getTypeAllocSize(type);
  }

  int getFieldIndex(llvm::StructType *structInstance, std::string fieldName) {
    auto attributes = *classTable[structInstance->getName().data()]->attributes;
    auto attribute = attributes.find(fieldName);
    return std::distance(attributes.begin(), attribute);
  }

  llvm::Function *createFunction(const std::string &fnName,
                                 llvm::FunctionType *fnType, Env_t env) {
    auto fn = module->getFunction(fnName);
    if (fn == nullptr) {
      fn = createFunctionProto(fnName, fnType, env);
    }
    auto currentBlock = createFunctionBlock(fn, fnName + "Entry");
    return fn;
  }

  llvm::Function *createFunctionProto(const std::string &fnName,
                                      llvm::FunctionType *fnType, Env_t env) {
    auto fn = llvm::Function::Create(fnType, llvm::Function::ExternalLinkage,
                                     fnName, *module);
    verifyFunction(*fn);
    std::vector<llvm::Type *> functionParamTypes;
    module->getOrInsertFunction(fnName, fnType);
    return fn;
  }

  llvm::BasicBlock *createFunctionBlock(llvm::Function *fn,
                                        std::string entryName) {
    auto entry = createBB(entryName, fn);
    builder->SetInsertPoint(entry);
    return entry;
  }

  llvm::BasicBlock *createBB(std::string name, llvm::Function *fn = nullptr) {
    return llvm::BasicBlock::Create(*ctx, name, fn);
  }

  void setupExternFunctions() {
    auto bytePtrArr = builder->getInt8Ty()->getPointerTo();
    module->getOrInsertFunction(
        "printf", llvm::FunctionType::get(builder->getInt32Ty(), bytePtrArr,
                                          true /*vararg*/));
    module->getOrInsertFunction(
        "GC_malloc",
        llvm::FunctionType::get(builder->getInt8Ty()->getPointerTo(),
                                builder->getInt64Ty(), false));
  }

  void setupGlobalEnvironment() {
    std::map<std::string, llvm::AllocaInst *> globalObject{};
    globalEnv = std::make_shared<Environment>(globalObject, nullptr);
  }
};
