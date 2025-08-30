# Module System Troubleshooting Guide

This guide helps you resolve common issues when working with the Pascal module system.

## Common Error Messages and Solutions

### 1. "Module not found: [ModuleName]"

**Problem**: The interpreter cannot locate the specified unit file.

**Possible Causes**:
- Unit file doesn't exist
- Unit file is in wrong location
- Typo in unit name

**Solutions**:
1. **Check file existence**: Ensure the unit file exists with `.pas` extension
   ```
   Expected locations:
   - ./[ModuleName].pas (current directory)
   - ./stdlib/[ModuleName].pas (standard library)
   ```

2. **Verify file name**: Unit file name must match the unit declaration
   ```pascal
   // File: Math.pas
   unit Math;  // Must match filename
   ```

3. **Check search paths**: The interpreter searches in:
   - Current directory (`.`)
   - Standard library directory (`./stdlib`)

**Example Fix**:
```pascal
// Wrong
uses MyMath;  // Looking for MyMath.pas

// Correct (if file is Math.pas)
uses Math;
```

### 2. "Circular dependency detected: [Unit1] -> [Unit2] -> [Unit1]"

**Problem**: Units depend on each other in a cycle, creating an infinite dependency loop.

**Example of Circular Dependency**:
```pascal
// UnitA.pas
unit UnitA;
interface
uses UnitB;  // UnitA depends on UnitB
// ...

// UnitB.pas  
unit UnitB;
interface
uses UnitA;  // UnitB depends on UnitA - CIRCULAR!
// ...
```

**Solutions**:
1. **Restructure dependencies**: Move shared functionality to a third unit
   ```pascal
   // Common.pas
   unit Common;
   interface
   // Shared types and functions
   
   // UnitA.pas
   unit UnitA;
   interface
   uses Common;  // Only depends on Common
   
   // UnitB.pas
   unit UnitB;
   interface  
   uses Common;  // Only depends on Common
   ```

2. **Remove unnecessary dependencies**: Only import what you actually need

3. **Use forward declarations**: Declare types without importing the full unit

### 3. "Symbol '[SymbolName]' not found in module '[ModuleName]'"

**Problem**: Trying to use a symbol that doesn't exist in the unit's interface section.

**Possible Causes**:
- Symbol is in implementation section (private)
- Symbol name is misspelled
- Symbol doesn't exist in the unit

**Solutions**:
1. **Check interface section**: Ensure symbol is declared in interface, not implementation
   ```pascal
   unit MyUnit;
   
   interface
     function PublicFunction: integer;  // Accessible from outside
   
   implementation
     function PrivateFunction: integer; // NOT accessible from outside
     begin
       // ...
     end;
     
     function PublicFunction: integer;
     begin
       // ...
     end;
   end.
   ```

2. **Verify spelling**: Check for typos in symbol names

3. **Check available symbols**: Review the unit's interface section for available symbols

### 4. "Interface/Implementation mismatch for '[SymbolName]'"

**Problem**: Function signature in interface doesn't match implementation.

**Example**:
```pascal
unit MyUnit;

interface
  function Calculate(a, b: integer): integer;  // Interface signature

implementation
  function Calculate(x: integer): integer;     // Wrong signature!
  begin
    // ...
  end;
end.
```

**Solution**: Ensure signatures match exactly
```pascal
unit MyUnit;

interface
  function Calculate(a, b: integer): integer;

implementation
  function Calculate(a, b: integer): integer;  // Correct signature
  begin
    // ...
  end;
end.
```

### 5. "Syntax error in unit '[UnitName]' at line [N]"

**Problem**: Unit file contains syntax errors.

**Common Syntax Issues**:
1. **Missing end terminator**:
   ```pascal
   unit MyUnit;
   interface
   implementation
   // Missing 'end.' at the end!
   ```
   
   **Fix**: Always end with `end.`
   ```pascal
   unit MyUnit;
   interface
   implementation
   end.  // Required terminator
   ```

2. **Wrong section order**:
   ```pascal
   unit MyUnit;
   implementation  // Wrong! Interface must come first
   interface
   end.
   ```
   
   **Fix**: Interface must come before implementation
   ```pascal
   unit MyUnit;
   interface
   implementation
   end.
   ```

3. **Missing semicolons**:
   ```pascal
   unit MyUnit;
   interface
     function Test: integer  // Missing semicolon
   implementation
   end.
   ```
   
   **Fix**: Add semicolons after declarations
   ```pascal
   unit MyUnit;
   interface
     function Test: integer;  // Semicolon added
   implementation
   end.
   ```

## Best Practices to Avoid Issues

### 1. Unit File Organization
- Keep unit files in appropriate directories
- Use descriptive, unique unit names
- Follow consistent naming conventions

### 2. Dependency Management
- Draw dependency diagrams for complex projects
- Minimize cross-unit dependencies
- Group related functionality in single units

### 3. Interface Design
- Keep interfaces minimal and focused
- Document public functions and procedures
- Use meaningful parameter names

### 4. Testing Strategy
- Test units independently before integration
- Create simple test programs for each unit
- Verify all interface functions work correctly

## Debugging Tips

### 1. Isolate the Problem
- Test units individually
- Create minimal reproduction cases
- Remove unnecessary dependencies

### 2. Check File Locations
```bash
# Verify unit files exist
ls *.pas
ls stdlib/*.pas
```

### 3. Validate Unit Syntax
- Check each unit file can be parsed independently
- Verify interface/implementation sections are properly structured
- Ensure all declarations have implementations

### 4. Trace Dependencies
- List all units your program uses
- Check each unit's dependencies
- Look for circular references

## Example Debugging Session

**Problem**: Program fails with "Module not found: Calculator"

**Debug Steps**:
1. Check if Calculator.pas exists in current directory
2. Check if Calculator.pas exists in stdlib directory
3. Verify unit declaration matches filename:
   ```pascal
   unit Calculator;  // Must match Calculator.pas
   ```
4. Check for typos in uses clause:
   ```pascal
   uses Calculator;  // Not Calculater or calculator
   ```

**Resolution**: Found Calculator.pas in wrong directory, moved to current directory.

## Getting Help

When reporting module system issues, include:
1. Complete error message
2. Relevant unit files
3. Program that reproduces the issue
4. Directory structure showing file locations

This information helps identify and resolve module-related problems quickly.