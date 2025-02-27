# Tic-Tac-Toe Haskell Project Guide

## Build Commands
- `make build` - Build the project
- `make run` - Run the application
- `make test` - Run all tests
- `stack test --test-arguments="-p \"<test_name>\""` - Run a single test (replace <test_name> with the test function name)
- `make lint` - Run HLint on source files
- `make format` - Format code with Fourmolu
- `make clean` - Purge build artifacts

## Code Style Guidelines
- **Formatting**: 2-space indentation, trailing function arrows, multi-line Haddock
- **Imports**: Use diff-friendly style, qualified imports for containers (e.g., `Data.Sequence as S`)
- **Types**: Export type signatures, use explicit imports/exports
- **Naming**: Use camelCase for functions, PascalCase for types
- **Testing**: Group tests with `spec_` prefix, use descriptive test names
- **Error Handling**: Return `Either Error a` for functions that can fail
- **GHC Options**: Follow all warnings enabled in package.yaml (-Wall, etc.)