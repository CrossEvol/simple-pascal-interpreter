program BasicModuleUsage;

{ 
  This example demonstrates basic module usage with the Math standard library.
  It shows how to import a unit and use its functions.
}

uses Math;

var
  a, b, result: integer;

begin
  writeln('=== Basic Module Usage Example ===');
  writeln();
  
  a := 15;
  b := 4;
  
  writeln('Using Math module functions:');
  writeln('a = ', a, ', b = ', b);
  writeln();
  
  result := ADD(a, b);
  writeln('ADD(', a, ', ', b, ') = ', result);
  
  result := SUB(a, b);
  writeln('SUB(', a, ', ', b, ') = ', result);
  
  result := MUL(a, b);
  writeln('MUL(', a, ', ', b, ') = ', result);
  
  result := DIVIDE(a, b);
  writeln('DIVIDE(', a, ', ', b, ') = ', result);
  
  result := MODULO(a, b);
  writeln('MODULO(', a, ', ', b, ') = ', result);
  
  writeln();
  writeln('Module usage complete!');
end.