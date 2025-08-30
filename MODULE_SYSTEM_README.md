# Pascal Module System

A complete module system implementation for the Pascal interpreter, supporting standard Pascal unit syntax with interface/implementation separation.

## Quick Start

### Using Standard Library Modules

```pascal
program Example;
uses Math, Map, ArrayUtils;

var
  result: integer;
  myMap: TMap;
  numbers: array[1..3] of integer;

begin
  // Use Math module
  result := ADD(10, 5);
  writeln('10 + 5 = ', result);
  
  // Use Map module
  myMap := TMap.Create;
  myMap.Put('score', 95);
  writeln('Score: ', myMap.Get('score'));
  
  // Use ArrayUtils module
  numbers[1] := 3;
  numbers[2] := 1; 
  numbers[3] := 2;
  Sort(numbers);
  writeln('Sorted array: ', numbers[1], ', ', numbers[2], ', ', numbers[3]);
end.
```

### Creating Custom Units

```pascal
unit Calculator;

interface
  function Add(a, b: integer): integer;
  function Multiply(a, b: integer): integer;

implementation
  function Add(a, b: integer): integer;
  begin
    Add := a + b;
  end;
  
  function Multiply(a, b: integer): integer;
  begin
    Multiply := a * b;
  end;

end.
```

## Documentation

- **[Module System Guide](docs/module-system.md)** - Complete documentation of syntax and features
- **[Troubleshooting Guide](docs/troubleshooting.md)** - Solutions for common issues
- **[Examples](examples/)** - Working example programs

## Standard Library Modules

### Math Module (`stdlib/Math.pas`)
Mathematical operations:
- `ADD(a, b: integer): integer`
- `SUB(a, b: integer): integer`
- `MUL(a, b: integer): integer`
- `DIV(a, b: integer): integer`
- `MOD(a, b: integer): integer`

### Map Module (`stdlib/Map.pas`)
Key-value data structure:
- `TMap` class
- `Put(key: string; value: integer)`
- `Get(key: string): integer`
- `Remove(key: string)`
- `Keys(): array of string`
- `Values(): array of integer`

### ArrayUtils Module (`stdlib/ArrayUtils.pas`)
Array manipulation utilities:
- `Sort(var arr: array of integer)`
- `Find(arr: array of integer; value: integer): integer`
- `Copy(source: array of integer; var dest: array of integer)`
- `Size(arr: array of integer): integer`

## Key Features

- **Standard Pascal Syntax**: Uses `unit`, `interface`, `implementation`, and `uses` keywords
- **Symbol Visibility**: Interface symbols are public, implementation symbols are private
- **Dependency Resolution**: Automatic loading of dependent modules
- **Circular Dependency Detection**: Prevents infinite dependency loops
- **Search Paths**: Searches current directory and `./stdlib` for modules
- **Error Handling**: Clear error messages for common module issues

## File Structure

```
├── docs/
│   ├── module-system.md      # Complete documentation
│   └── troubleshooting.md    # Error resolution guide
├── examples/
│   ├── basic-module-usage.pas
│   ├── map-module-demo.pas
│   ├── array-utils-demo.pas
│   ├── multiple-modules.pas
│   ├── custom-unit-example/
│   │   ├── StringUtils.pas
│   │   └── string-demo.pas
│   └── README.md
├── stdlib/
│   ├── Math.pas
│   ├── Map.pas
│   └── ArrayUtils.pas
└── MODULE_SYSTEM_README.md   # This file
```

## Running Examples

```bash
# Basic module usage
python main.py examples/basic-module-usage.pas

# Map module demonstration
python main.py examples/map-module-demo.pas

# ArrayUtils demonstration  
python main.py examples/array-utils-demo.pas

# Multiple modules working together
python main.py examples/multiple-modules.pas

# Custom unit example
cd examples/custom-unit-example
python ../../main.py string-demo.pas
```

## Module Search Paths

The interpreter searches for unit files in:
1. Current directory (`.`)
2. Standard library directory (`./stdlib`)

## Best Practices

1. **Unit Organization**: One logical concept per unit
2. **Minimal Interfaces**: Only expose necessary symbols
3. **Clear Dependencies**: Avoid circular dependencies
4. **Consistent Naming**: Use descriptive unit and symbol names
5. **Documentation**: Comment interface declarations

## Error Handling

The module system provides clear error messages for:
- Missing module files
- Circular dependencies
- Symbol not found in modules
- Interface/implementation mismatches
- Syntax errors in unit files

See the [troubleshooting guide](docs/troubleshooting.md) for detailed solutions.

## Implementation Status

✅ **Complete Features**:
- Unit syntax parsing (`unit`, `interface`, `implementation`)
- Uses clause parsing and processing
- Module file discovery and loading
- Symbol table integration with module support
- Cross-module symbol resolution
- Dependency resolution and circular dependency detection
- Standard library modules (Math, Map, ArrayUtils)
- Comprehensive error handling
- Documentation and examples

## Getting Started

1. **Read the documentation**: Start with [docs/module-system.md](docs/module-system.md)
2. **Try the examples**: Run the example programs in [examples/](examples/)
3. **Create your own units**: Follow the patterns shown in the custom unit example
4. **Use standard library**: Import Math, Map, or ArrayUtils modules in your programs

For issues or questions, refer to the [troubleshooting guide](docs/troubleshooting.md).