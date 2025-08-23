# Scheme-Man

## Build

1. Install CMake (and optionally Ninja).
  - On MacOS: `brew install cmake ninja`
  - On Debian systems: `apt install cmake ninja-build`

2. Install [vcpkg](https://vcpkg.io).

3. Fetch submodules:

```shell
git submodule update --init --recursive --rebase --force --depth=1
```

4. Build with CMake:

```shell
mkdir -p build && cd build
time cmake -S .. -B . -G Ninja --preset=vcpkg -DCMAKE_BUILD_TYPE=Debug ..     # or -DCMAKE_BUILD_TYPE=Release (default)
cmake --build .

# Now test the program:
./src/scman
```

## Documentation

Go to the [docs directory](./docs/).
