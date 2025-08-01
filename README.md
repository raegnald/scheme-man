
# Scheme-Man

## Build

1. Initialise submodules:

```shell
git submodule update --init --recursive --rebase --force --depth=1
```

2. Install CMake.
  - On MacOS: `brew install cmake`
  - On Debian systems: `apt install cmake`

3. Fetch submodules:

```shell
git submodule update --init --recursive --rebase --force --depth=1
```

4. Build with CMake:

```shell
mkdir -p build && cd build
cmake -DCMAKE_BUILD_TYPE=Debug ..     # or -DCMAKE_BUILD_TYPE=Release (default)
cmake --build . -j8

# Now test the program:
./src/scman ../assets/Levels/001.tmx
```

## Documentation

Go to the [docs directory](./docs/).
