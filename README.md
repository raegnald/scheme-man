
# Scheme-Man

## Build

1. Install CMake and GNU Guile. On MacOS: `brew install cmake guile`
2. Build with CMake:

```shell
mkdir -p build && cd build
cmake -DCMAKE_BUILD_TYPE=Debug ..     # or -DCMAKE_BUILD_TYPE=Release
cmake --build . -j8

# Now test the program:
./src/scman ../assets/Levels/001.tmx
```

## Documentation

Go to the [docs directory](./docs/).
