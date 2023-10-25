#pragma once

#include <memory>
#include <string>
#include <vector>
#include "environment.h"
#include "lexer.h"
#include "llvm-13/llvm/IR/IRBuilder.h"
#include "llvm-13/llvm/IR/LLVMContext.h"
#include "llvm-13/llvm/IR/Module.h"
#include "llvm-13/llvm/IR/Verifier.h"
#include "llvm/Support/Alignment.h"
#include "parser.h"

#define todo() assert(false && "Unimplemented")

using Env_t = std::shared_ptr<Environment>;

class SchemeLLVM {
   public:
    SchemeLLVM() {
        moduleInit();
        setupExternFunctions();
        setupGlobalEnvironment();
    }

    void exec(const std::string& program) {
        module->print(llvm::outs(), nullptr);
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
        varsBuilder;  // Additional builder for start of function
    llvm::Function* fn;

    void moduleInit() {
        ctx = std::make_unique<llvm::LLVMContext>();
        module = std::make_unique<llvm::Module>("schemeLLVM", *ctx);
        builder = std::make_unique<llvm::IRBuilder<>>(*ctx);
        varsBuilder = std::make_unique<llvm::IRBuilder<>>(*ctx);
    }

    void saveModuleToFile(const std::string& fileName) {
        std::error_code errorCode;
        llvm::raw_fd_ostream outLL(fileName, errorCode);
        module->print(outLL, nullptr);
    }

    void compile(std::shared_ptr<ProgramNode> programNode, Env_t env) {
        fn = createFunction(
            "main", llvm::FunctionType::get(builder->getInt32Ty(), false),
            globalEnv);
        gen(programNode, env, fn);
        builder->CreateRet(builder->getInt32(0));
    }

    void gen(std::shared_ptr<ProgramNode> programNode,
             Env_t env,
             llvm::Function* oldFunc) {
        auto statements = programNode.get()->statements;
        for (auto it = statements.begin(); it < statements.end(); it++) {
            genStatement(*it, env, oldFunc);
            fn = oldFunc;  // in case statement is a function, you need to
                           // return the insert point to the older function
        }
    }
    void allocVar(std::string name,
                  llvm::Type* type,
                  Env_t env,
                  llvm::Value* exprValue) {
        varsBuilder->SetInsertPoint(&fn->getEntryBlock());
        auto varAlloc = varsBuilder->CreateAlloca(type, 0, name.c_str());
        builder->CreateStore(exprValue, varAlloc);
        env->set(name, varAlloc);
    }

    void genStatement(std::shared_ptr<StatementNode> stmt,
                      Env_t env,
                      llvm::Function* oldFunc) {
        switch (stmt->type) {
            case StatementType::Let: {
                auto letStatement = stmt->letStatement;
                auto exprValue = genExpr(letStatement->value, env);
                allocVar(*(letStatement->identifier), exprValue->getType(), env,
                         exprValue);
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
                    genStatement(*statementIt, env, oldFunc);
                }
                builder->CreateBr(ifEndBlock);
                builder->SetInsertPoint(ifFalseBlock);
                auto falseBlock = stmt->ifStatement->falseBlock;
                for (auto statementIt = falseBlock->statements.begin();
                     statementIt < falseBlock->statements.end();
                     statementIt++) {
                    genStatement(*statementIt, env, oldFunc);
                }
                builder->CreateBr(ifEndBlock);
                builder->SetInsertPoint(ifEndBlock);
                break;
            }
            case StatementType::For: {
                auto forEnv = std::make_shared<Environment>(
                    std::map<std::string, llvm::Value*>(), env);
                genStatement(stmt->forStatement->initialCondition, forEnv,
                             oldFunc);
                auto forConditionBlock = createBB("forCondition", fn);
                builder->CreateBr(forConditionBlock);
                auto forBody = createBB("forBody", fn);
                auto forEnd = createBB("forEnd", fn);
                auto forUpdateBlock = createBB("forUpdateBlock", fn);
                builder->SetInsertPoint(forConditionBlock);
                auto condition =
                    genExpr(stmt->forStatement->continueCondition, forEnv);
                builder->CreateCondBr(condition, forBody, forEnd);
                builder->SetInsertPoint(forBody);
                for (auto statement : stmt->forStatement->body->statements) {
                    genStatement(statement, forEnv, oldFunc);
                }
                builder->CreateBr(forUpdateBlock);
                builder->SetInsertPoint(forUpdateBlock);
                genStatement(stmt->forStatement->updateCondition, forEnv,
                             oldFunc);
                builder->CreateBr(forConditionBlock);
                builder->SetInsertPoint(forEnd);
                break;
            }
            case StatementType::Function: {
                std::vector<llvm::Type*> paramTypes{};
                for (auto arg : stmt->funcStatement->arguments) {
                    llvm::Type* type =
                        arg->type == TokenType::IntType
                            ? dynamic_cast<llvm::Type*>(builder->getInt32Ty())
                            : builder->getInt8Ty()->getPointerTo();
                    paramTypes.emplace_back(type);
                }
                auto newFuncEnv = std::make_shared<Environment>(
                    std::map<std::string, llvm::Value*>(), globalEnv);
                auto functionReturnType =
                    stmt->funcStatement->returnType == TokenType::IntType
                        ? dynamic_cast<llvm::Type*>(builder->getInt32Ty())
                        : builder->getInt8Ty()->getPointerTo();
                auto newFunc =
                    createFunction(stmt->funcStatement->name,
                                   llvm::FunctionType::get(functionReturnType,
                                                           paramTypes, false),
                                   newFuncEnv);
                fn = newFunc;
                auto statements = stmt->funcStatement->body->statements;
                int i = 0;  // INFO: fn->args() gives an iterator thus this
                            // shabby loop
                for (auto& arg : fn->args()) {
                    auto argFromAST = stmt->funcStatement->arguments[i++];
                    arg.setName(*(argFromAST->name));
                    allocVar(*(argFromAST->name), arg.getType(), newFuncEnv,
                             &arg);
                }
                for (auto statementIt = statements.begin();
                     statementIt < statements.end(); statementIt++) {
                    genStatement(*statementIt, newFuncEnv, oldFunc);
                }
                env->set(stmt->funcStatement->name, newFunc);
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
        }
    }

    llvm::Value* genExpr(std::shared_ptr<ExpressionNode> expressionNode,
                         Env_t env) {
        llvm::Value* dummy;
        switch (expressionNode->type) {
            case ExpressionType::String: {
                auto str = builder->CreateGlobalStringPtr(
                    expressionNode->stringExp->value);
                return str;
            }
            case ExpressionType::Number: {
                auto num =
                    builder->getInt32(expressionNode->integerExp->intValue);
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
                        return builder->CreateICmpUGT(lhs, rhs,
                                                      "tmpGreaterThan");
                    }
                    case TokenType::Equal: {
                        assert(expressionNode->infixExpr->lhs->type ==
                                   ExpressionType::Identifier &&
                               "Invalid lvalue");
                        auto pointerToLhs =
                            env->get(*(expressionNode->infixExpr->lhs
                                           ->identifierExpression->name));
                        return builder->CreateStore(rhs, pointerToLhs);
                    }
                    default: {
                        assert(false && "Invalid operand for infix");
                    }
                }
                break;
            }
            case ExpressionType::Identifier: {
                auto identifier =
                    env->get(*(expressionNode->identifierExpression->name));
                auto identifierCastForLoading =
                    llvm::dyn_cast<llvm::AllocaInst>(identifier);
                return builder->CreateLoad(
                    identifierCastForLoading->getAllocatedType(),
                    identifierCastForLoading,
                    (expressionNode->identifierExpression->name)->c_str());
                break;
            }
            case ExpressionType::Call: {
                llvm::Function* function;
                if (*(expressionNode->callExpression->fnName) == "printf") {
                    function = module->getFunction("printf");
                } else {
                    function = static_cast<llvm::Function*>(
                        env->get(*(expressionNode->callExpression->fnName)));
                }
                auto evaledExpressions = std::vector<llvm::Value*>();
                for (auto arg : expressionNode->callExpression->arguments) {
                    evaledExpressions.emplace_back(genExpr(arg, env));
                }
                auto call = builder->CreateCall((llvm::Function*)(function),
                                                evaledExpressions);
                return call;
            }
            default:
                assert(false && "Should'nt hit here");
        }
        return dummy;
    }

    llvm::Function* createFunction(const std::string& fnName,
                                   llvm::FunctionType* fnType,
                                   Env_t env) {
        auto fn = module->getFunction(fnName);
        if (fn == nullptr) {
            fn = createFunctionProto(fnName, fnType, env);
        }
        auto currentBlock = createFunctionBlock(fn, fnName + "Entry");
        return fn;
    }

    llvm::Function* createFunctionProto(const std::string& fnName,
                                        llvm::FunctionType* fnType,
                                        Env_t env) {
        auto fn = llvm::Function::Create(
            fnType, llvm::Function::ExternalLinkage, fnName, *module);
        verifyFunction(*fn);
        env->set(fnName, fn);
        return fn;
    }

    llvm::BasicBlock* createFunctionBlock(llvm::Function* fn,
                                          std::string entryName) {
        auto entry = createBB(entryName, fn);
        builder->SetInsertPoint(entry);
        return entry;
    }

    llvm::BasicBlock* createBB(std::string name, llvm::Function* fn = nullptr) {
        return llvm::BasicBlock::Create(*ctx, name, fn);
    }

    void setupExternFunctions() {
        auto bytePtrArr = builder->getInt8Ty()->getPointerTo();
        module->getOrInsertFunction(
            "printf", llvm::FunctionType::get(builder->getInt32Ty(), bytePtrArr,
                                              true /*vararg*/));
    }

    void setupGlobalEnvironment() {
        std::map<std::string, llvm::Value*> globalObject{};
        globalEnv = std::make_shared<Environment>(globalObject, nullptr);
    }
};
