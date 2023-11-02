llc out.ll
clang-13 -I/usr/include/gc -o  out out.s /usr/lib/x86_64-linux-gnu/libgc.a -pthread
./out
