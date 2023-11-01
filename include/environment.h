#include "llvm/IR/Instructions.h"
#include "llvm/IR/Value.h"
#include <iostream>
#include <map>
#include <memory>
class Environment {
public:
  Environment(std::map<std::string, llvm::AllocaInst *> inner,
              std::shared_ptr<Environment> outer)
      : inner(inner), outer(outer) {}

  llvm::Value *set(const std::string &name, llvm::AllocaInst *value) {
    inner[name] = value;
    return value;
  }

  llvm::Value *get(const std::string &name) {
    auto valueInMap = inner.find(name);
    if (valueInMap == inner.end()) {
      // Value not found in inner scope, search in outer scope
      if (outer != nullptr) {
        return outer->get(name);
      }
      std::cout << "Variable " << name << " not defined"
                << "\n";
    }
    return valueInMap->second;
  }

private:
  std::map<std::string, llvm::AllocaInst *> inner;
  std::shared_ptr<Environment> outer;
};
