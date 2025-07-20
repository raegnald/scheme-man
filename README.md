
# Scheme-Man

## Build

1. Install CMake and GNU Guile. On MacOS: `brew install cmake guile`
2. Build with CMake:

```shell
mkdir -p build && cd build
cmake ..
cmake --build .
# Now test the program:
./src/scman ../assets/Levels/001.tmx
```
