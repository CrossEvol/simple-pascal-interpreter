# Simple Pascal Interpreter

This project is a simple interpreter for the Pascal programming language, written in Python. It includes a lexer, parser, semantic analyzer, and interpreter.

## Docs
see [mini-pascal docs](pascal.md).

## Target

This project want to run the [fibonacci-recursion program](pas/fibonacci.pas) and [hand-written json parser](pas/json.pas).

## Usage

This project uses a `Makefile` to simplify common tasks.

### Running the Interpreter

To run the interpreter on a Pascal source file:

```bash
make run file=<path/to/your/pascal_file.pas>
```

### Debugging

To run the interpreter with debug logging for the call stack and symbol table scopes:

```bash
make main file=<path/to/your/pascal_file.pas>
```

### Running Tests

To run the main unit test suite:

```bash
make test
```

#### Integration Tests

There are also integration tests that run against a collection of Pascal examples:

-   **`test_spi`**: Runs the examples using this interpreter (`spi`).
    ```bash
    make test_spi
    ```
-   **`test_fpc`**: Compares the output of the examples with the output of the Free Pascal Compiler (`fpc`) to verify correctness.
    ```bash
    make test_fpc
    ```

### Generating an AST Visualization

To generate a visualization of the Abstract Syntax Tree (AST) for a Pascal file:

```bash
make dot file=<path/to/your/pascal_file.pas>
```

This will generate `ast.dot` and, if you have Graphviz installed, an `ast.png` image.

### Building an Executable

To build a standalone executable of the interpreter:

```bash
make build
```

The executable will be located in the `dist/` directory.
