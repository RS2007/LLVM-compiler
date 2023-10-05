#include "llvm/IR/Value.h"
#include <map>
#include <memory>
class Environment {
public:
  Environment(std::map<std::string, llvm::Value *> inner,
              std::shared_ptr<Environment> outer)
      : inner(inner), outer(outer) {}

private:
  std::map<std::string, llvm::Value *> inner;
  std::shared_ptr<Environment> outer;
};
