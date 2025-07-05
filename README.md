
# Scheme-Man

## Build

1. Install CMake and SFML3. On MacOS: `brew install cmake sfml@3`
2. Build with CMake:

```shell
mkdir -p build && cd build
cmake ..
cmake --build .
# Now test the program:
./scman ../assets/Levels/001.tmx
```
