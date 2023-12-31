#pragma once
#include "environment.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Type.h"
#include <map>
class ClassInfo {
public:
  llvm::StructType *classGenerated;
  llvm::StructType *parent;
  std::shared_ptr<std::map<std::string, llvm::Type *>> attributes;
  std::shared_ptr<std::map<std::string, llvm::Function *>> methods;
  std::shared_ptr<Environment> classEnv;

  ClassInfo(llvm::StructType *classGenerated,
            std::shared_ptr<std::map<std::string, llvm::Type *>> attributes,
            std::shared_ptr<std::map<std::string, llvm::Function *>> methods)
      : classGenerated(classGenerated), attributes(attributes),
        methods(methods) {}
  ClassInfo(llvm::StructType *classGenerated,
            std::shared_ptr<Environment> classEnv)
      : classGenerated(classGenerated),
        attributes(std::make_shared<std::map<std::string, llvm::Type *>>()),
        methods(std::make_shared<std::map<std::string, llvm::Function *>>()),
        classEnv(classEnv) {}
};
