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
            genStatement(it, env);
            fn = oldFunc;  // in case statement is a function, you need to
                           // return the insert point to the older function
            builder->SetInsertPoint(&fn->getEntryBlock());
        }
    }
    void allocVar(std::string name,
                  llvm::Type* type,
                  Env_t env,
                  llvm::Value* exprValue) {
        varsBuilder->SetInsertPoint(&fn->getEntryBlock());
        auto varAlloc = varsBuilder->CreateAlloca(type, 0, name.c_str());
        builder->CreateStore(exprValue, varAlloc);
        env->set(name, exprValue);
    }

    void genStatement(std::vector<std::shared_ptr<StatementNode>>::iterator it,
                      Env_t env) {
        switch ((*it)->type) {
            case StatementType::Let: {
                auto letStatement = (*it)->letStatement;
                auto exprValue = genExpr(letStatement->value);
                switch (letStatement->value->type) {
                    case ExpressionType::String: {
                        allocVar(letStatement->identifier,
                                 builder->getInt8Ty()->getPointerTo(), env,
                                 exprValue);
                        break;
                    }
                    case ExpressionType::Infix: {
                        allocVar(letStatement->identifier,
                                 builder->getInt32Ty(), env, exprValue);
                        break;
                    }
                    case ExpressionType::Number: {
                        allocVar(letStatement->identifier,
                                 builder->getInt32Ty(), env, exprValue);
                        break;
                    }
                    default: {
                        assert(false && "Should not hit this");
                    }
                }
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
                genExpr((*it)->expressionStatement->expression);
                break;
            }
            case StatementType::Function: {
                auto newFuncEnv = std::make_shared<Environment>(
                    std::map<std::string, llvm::Value*>(), globalEnv);
                auto newFunc = createFunction(
                    (*it)->funcStatement->name,
                    llvm::FunctionType::get(builder->getInt32Ty(), false),
                    newFuncEnv);
                fn = newFunc;
                auto statements = (*it)->funcStatement->body->statements;
                for (auto statementIt = statements.begin();
                     statementIt < statements.end(); statementIt++) {
                    genStatement(statementIt, env);
                }
                builder->CreateRet(builder->getInt32(0));
                break;
            }
            case StatementType::Block: {
                assert(false && "Should'nt hit this");
            }
            case StatementType::Return: {
                todo();
            }
        }
    }

    llvm::Value* genExpr(std::shared_ptr<ExpressionNode> expressionNode) {
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
                auto lhs = genExpr(expressionNode->infixExpr->lhs);
                auto rhs = genExpr(expressionNode->infixExpr->rhs);
                switch (expressionNode->infixExpr->op) {
                    case TokenType::Plus: {
                        return builder->CreateAdd(lhs, rhs);
                    }
                    case TokenType::Minus: {
                        return builder->CreateSub(lhs, rhs);
                    }
                    default: {
                        assert(false && "Invalid operand for infix");
                    }
                }
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
