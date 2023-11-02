### Current Progress
  - can compile the following program
  ```
    defun add(a: int, b: int): int{
         return a + b;
    }
    let five = add(3,2);
    let ans = printf("%d",five);
  ```
### Note
- Currently supports type signatures with no typechecking(will be implemented in the future)

### Build
- requires the libgc package for your distribution
  - for ubuntu `sudo apt-get install -y libgc-dev`
- requires the llvm toolchain for your machine
  - change the llvm locations in `CMakeLists.txt`
- Change the program string in `main.cpp`

```console
  mkdir build && cd $_
  mv ../compileLLVM.sh ./
  cmake ..
  ninja
  ./exec
  ./compileLLVM.sh
```

### Demo

https://github.com/RS2007/llvm-cpp-tut/assets/83483297/e362faaf-9bb3-4fb1-ad60-03ba5fa0d53a

