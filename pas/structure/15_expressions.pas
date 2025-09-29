program Expressions;

{演示表达式和运算符}

var
  a, b: Integer;
  x, y: Real;
  result: Boolean;
  ch : Char;

begin
  a := 10;
  b := 3;
  x := 5.5;
  y := 2.2;
  
  { 算术运算符 }
  writeln('Arithmetic Operations:');
  writeln(a, ' + ', b, ' = ', a + b);
  writeln(a, ' - ', b, ' = ', a - b);
  writeln(a, ' * ', b, ' = ', a * b);
  writeln(a, ' / ', b, ' = ', a / b); { 注意：整数除法结果是实数 }
  writeln(a, ' div ', b, ' = ', a div b); { 整数除法 }
  writeln(a, ' mod ', b, ' = ', a mod b); { 取模 }
  
  { 比较运算符 }
  writeln('Comparison Operations:');
  writeln(a, ' = ', b, ' ? ', a = b);
  writeln(a, ' <> ', b, ' ? ', a <> b);
  writeln(a, ' < ', b, ' ? ', a < b);
  writeln(a, ' > ', b, ' ? ', a > b);
  writeln(a, ' <= ', b, ' ? ', a <= b);
  writeln(a, ' >= ', b, ' ? ', a >= b);
  
  { 逻辑运算符 }
  writeln('Logical Operations:');
  result := (a > 5) and (b < 5);
  writeln('(', a, ' > 5) and (', b, ' < 5) = ', result);
  
  result := (a < 5) or (b > 5);
  writeln('(', a, ' < 5) or (', b, ' > 5) = ', result);
  
  result := not (a = b);
  writeln('not (', a, ' = ', b, ') = ', result);
  
  { 集合运算符 }
  writeln('Set Operations:');
  ch := 'a';
  if ch in ['a', 'b', 'c'] then
    writeln('a in [a,b,c]');
    
  if 5 in [1..10] then
    writeln('5 is in [1..10]');
end.