- [ ] Finish the environment API
- [x] Remove as many news and deletes and use smart pointers
- [ ] Add support for comparison operators  
- [ ] Cleaning up the parser
- [o] Finish the parser for if,function and call expressions(subsequently for the return,block and expression statements)
  - [x] Function
  - [ ] If
  - [ ] Call
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
