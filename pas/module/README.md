# Pascal Module System Examples

This directory contains example programs demonstrating the Pascal module system features.

## Running the Examples

To run any example program:

```bash
python main.py examples/[example-name].pas
```

## Example Programs

### 1. Basic Module Usage (`basic-module-usage.pas`)
- **Purpose**: Demonstrates importing and using the Math standard library module
- **Features**: Basic function calls from imported module
- **Run**: `python main.py examples/basic-module-usage.pas`

### 2. Map Module Demo (`map-module-demo.pas`)
- **Purpose**: Shows complete usage of the Map standard library module
- **Features**: Creating maps, storing/retrieving values, getting keys/values arrays
- **Run**: `python main.py examples/map-module-demo.pas`

### 3. ArrayUtils Demo (`array-utils-demo.pas`)
- **Purpose**: Demonstrates array manipulation using ArrayUtils module
- **Features**: Sorting, searching, copying arrays, getting array size
- **Run**: `python main.py examples/array-utils-demo.pas`

### 4. Multiple Modules (`multiple-modules.pas`)
- **Purpose**: Shows how to use multiple standard library modules together
- **Features**: Combining Math, Map, and ArrayUtils in a single program
- **Run**: `python main.py examples/multiple-modules.pas`

### 5. Custom Unit Example (`custom-unit-example/`)
- **Purpose**: Demonstrates creating and using custom units
- **Files**: 
  - `StringUtils.pas` - Custom unit with string manipulation functions
  - `string-demo.pas` - Program using the custom StringUtils unit
- **Features**: Interface/implementation separation, private helper functions
- **Run**: `cd examples/custom-unit-example && python ../../main.py string-demo.pas`

## Standard Library Modules Used

### Math Module
- `ADD(a, b: integer): integer` - Addition
- `SUB(a, b: integer): integer` - Subtraction  
- `MUL(a, b: integer): integer` - Multiplication
- `DIV(a, b: integer): integer` - Integer division
- `MOD(a, b: integer): integer` - Modulo operation

### Map Module
- `TMap` class for key-value storage
- `Put(key: string; value: integer)` - Store key-value pair
- `Get(key: string): integer` - Retrieve value by key
- `Remove(key: string)` - Remove key-value pair
- `Keys(): array of string` - Get all keys
- `Values(): array of integer` - Get all values

### ArrayUtils Module
- `Sort(var arr: array of integer)` - Sort array in-place
- `Find(arr: array of integer; value: integer): integer` - Find value index
- `Copy(source: array of integer; var dest: array of integer)` - Copy array
- `Size(arr: array of integer): integer` - Get array size

## Learning Path

1. **Start with**: `basic-module-usage.pas` - Learn basic import syntax
2. **Then try**: Individual module demos to understand each standard library
3. **Next**: `multiple-modules.pas` - See how modules work together
4. **Finally**: Custom unit example - Learn to create your own modules

## Common Patterns

### Importing Modules
```pascal
program MyProgram;
uses ModuleName1, ModuleName2;
begin
  // Use module functions here
end.
```

### Using Standard Library Functions
```pascal
uses Math;
var result: integer;
begin
  result := ADD(5, 3);  // Call function from Math module
end.
```

### Creating Custom Units
```pascal
unit MyUnit;

interface
  // Public declarations
  function MyFunction(param: integer): integer;

implementation
  // Private implementations
  function MyFunction(param: integer): integer;
  begin
    MyFunction := param * 2;
  end;

end.
```

## Troubleshooting

If you encounter issues running the examples:

1. **Module not found**: Ensure you're running from the correct directory
2. **Syntax errors**: Check that all required standard library modules exist in `./stdlib/`
3. **Custom unit issues**: For custom unit example, make sure you're in the `custom-unit-example` directory

See `../docs/troubleshooting.md` for detailed troubleshooting guidance.