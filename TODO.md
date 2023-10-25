- [ ] Finish the environment API
- [x] Remove as many news and deletes and use smart pointers
- [o] Add support for comparison operators  
  - Support for `>=`,`<=`,`==`,`!`
- [ ] Support for boolean data type
- [ ] Cleaning up the parser
  - [ ] Bug with parsing expression statements
- [o] Finish the parser for if,function and call expressions(subsequently for the return,block and expression statements)
  - [x] Function
  - [x] If
  - [x] Call
  - [ ] For
  - [ ] While
- [ ] Add support for classes and inheritance
- [ ] Higher order function support

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
