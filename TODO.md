- [x] Finish the environment API
- [x] Remove as many news and deletes and use smart pointers
- [o] Add support for comparison operators  
  - Support for `>=`,`<=`,`==`,`!`
- [ ] Support for boolean data type
- [ ] Cleaning up the parser
  - [x] Bug with parsing expression statements
  - [ ] Cleaning `parseExpression`
- [o] Finish the parser for if,function and call expressions(subsequently for the return,block and expression statements)
  - [x] Function
  - [x] If
  - [x] Call
  - [x] For
  - [x] While
- [o] Add support for classes and inheritance
  - [x] Classes(stack allocated) and property access working
  - [x] Heap allocation
  - [ ] Member calls
- [ ] Higher order function support
- [ ] Implementing a type checker
  - [ ] Adding support for proper types
- [x] Map access error: fix this
- [o] Test codegen for member functions
  - [x] Constructor working
- [ ] Class Methods & V tables
- [x] Based on the js parser, we will have an assignment expression and a member expression
- [x] While codegenning for the if-else condition, if the true block has a return statement do not put a branch at the end, similarly for the false block


## Language Spec
```
defun add(x: number,y: number): number -> {
  return x+y;
}
```

- let statements take in functions
- all functions defacto in main
- create a separate function in case of expression function

### For 
- The initial condition must be a statement  
- For in llvm assembly looks something like this:
  - set i as the value given in the initial condition in a forInitial basic block.
  - jump to the for condition basic block(unconditional jump). 
  - generate a forCondition basic block and put the condition branch there
  - go to forBody basic block if the condition is satisfied(conditional branch)
  - at the end of the body, update the condition and then jump back to the forCondition basic block

### Classes
```ll
  %Point = type {
    i32,
    i32
  } ; aggregate types

  define void @Point_constructor(%Point* %self,i32 %x,i32 %y){

  }
```
- `getelementptr` or `gep` instruction
  - `address = getelementptr <ty>,<ty>* <base>,<idx1>,<idx2>`
  - The first ty is the type of the object we are reading
  - The second ty* is the pointer to the base object


### The weird parser error
- why can the lhs of the expression be the member expression 


- [ ] Clean up parser, there are too many conditional checks in the while loop in `parseExpression`
