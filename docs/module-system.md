# Pascal Module System Documentation

## Overview

The Pascal interpreter supports a complete module system that allows you to organize code into reusable units. This system follows standard Pascal unit syntax with `interface` and `implementation` sections, enabling proper encapsulation and code organization.

## Unit Syntax

### Basic Unit Structure

```pascal
unit UnitName;

interface
  // Public declarations go here
  // - Type definitions
  // - Variable declarations  
  // - Function/procedure signatures

implementation
  // Private implementations go here
  // - Function/procedure bodies
  // - Private variables and types

end.
```

### Using Units in Programs

```pascal
program MyProgram;

uses UnitName1, UnitName2, UnitName3;

begin
  // Your program code here
  // Can access interface symbols from imported units
end.
```

## Key Features

### Interface vs Implementation

- **Interface Section**: Contains public declarations visible to importing programs
- **Implementation Section**: Contains actual code implementations and private declarations
- Only interface symbols are accessible from outside the unit

### Symbol Resolution

1. Current scope is checked first
2. Then imported units are searched (in import order)
3. Last imported unit takes precedence for name conflicts

### Module Search Paths

The interpreter searches for unit files in:
1. Current directory (`.`)
2. Standard library directory (`./stdlib`)

## Standard Library Modules

### Map Module

Provides a key-value mapping data structure.

```pascal
uses Map;

var
  myMap: TMap;
begin
  myMap := TMap.Create;
  myMap.Put('key1', 100);
  myMap.Put('key2', 200);
  
  writeln(myMap.Get('key1')); // Outputs: 100
  myMap.Remove('key1');
end.
```

Available methods:
- `Put(key: string; value: integer)` - Store a key-value pair
- `Get(key: string): integer` - Retrieve value by key
- `Remove(key: string)` - Remove a key-value pair
- `Keys(): array of string` - Get all keys
- `Values(): array of integer` - Get all values

### Math Module

Provides mathematical operations.

```pascal
uses Math;

var
  result: integer;
begin
  result := ADD(5, 3);     // result = 8
  result := MUL(4, 7);     // result = 28
  result := SUB(10, 3);    // result = 7
  result := DIV(15, 3);    // result = 5
  result := MOD(17, 5);    // result = 2
end.
```

Available functions:
- `ADD(a, b: integer): integer` - Addition
- `MUL(a, b: integer): integer` - Multiplication
- `SUB(a, b: integer): integer` - Subtraction
- `DIV(a, b: integer): integer` - Integer division
- `MOD(a, b: integer): integer` - Modulo operation

### ArrayUtils Module

Provides array manipulation utilities.

```pascal
uses ArrayUtils;

var
  numbers: array[1..5] of integer;
  index: integer;
begin
  numbers[1] := 5;
  numbers[2] := 2;
  numbers[3] := 8;
  numbers[4] := 1;
  numbers[5] := 9;
  
  Sort(numbers);                    // Sort array in-place
  index := Find(numbers, 8);        // Find index of value 8
  writeln('Size: ', Size(numbers)); // Output array size
end.
```

Available procedures/functions:
- `Sort(var arr: array of integer)` - Sort array in ascending order
- `Find(arr: array of integer; value: integer): integer` - Find index of value (-1 if not found)
- `Copy(source: array of integer; var dest: array of integer)` - Copy array contents
- `Size(arr: array of integer): integer` - Get array size

## Best Practices

### Unit Organization

1. **Single Responsibility**: Each unit should have a clear, focused purpose
2. **Minimal Interface**: Only expose what's necessary in the interface section
3. **Clear Naming**: Use descriptive unit and symbol names
4. **Documentation**: Comment your interface declarations

### Dependency Management

1. **Avoid Circular Dependencies**: Units should not depend on each other in a cycle
2. **Minimize Dependencies**: Only import what you actually need
3. **Order Matters**: Later imports take precedence for name conflicts

### Example Unit Structure

```pascal
unit Calculator;

interface
  // Public types
  type
    TOperation = (opAdd, opSubtract, opMultiply, opDivide);
  
  // Public functions
  function Calculate(a, b: integer; op: TOperation): integer;
  function IsValidOperation(op: TOperation): boolean;

implementation
  // Private helper function
  function ValidateInputs(a, b: integer; op: TOperation): boolean;
  begin
    // Implementation details
  end;
  
  // Public function implementations
  function Calculate(a, b: integer; op: TOperation): integer;
  begin
    if not ValidateInputs(a, b, op) then
      exit(-1);
    
    case op of
      opAdd: Calculate := a + b;
      opSubtract: Calculate := a - b;
      opMultiply: Calculate := a * b;
      opDivide: 
        if b <> 0 then
          Calculate := a div b
        else
          Calculate := 0;
    end;
  end;
  
  function IsValidOperation(op: TOperation): boolean;
  begin
    IsValidOperation := op in [opAdd, opSubtract, opMultiply, opDivide];
  end;

end.
```