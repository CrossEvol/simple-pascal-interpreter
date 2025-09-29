program Functions;

{演示函数定义和调用}

{ 简单函数 }
function Add(a, b: Integer): Integer;
begin
  Add := a + b; { 传统赋值方式 }
end;

{ 使用函数名赋值的函数 }
function Multiply(a, b: Integer): Integer;
begin
  Multiply := a * b;
end;

{ 递归函数 }
function Factorial(n: Integer): Integer;
begin
  if n <= 1 then
    Factorial := 1
  else
    Factorial := n * Factorial(n - 1);
end;

{ 带const参数的函数 }
function Square(const x: Real): Real;
begin
  Square := x * x;
end;

var
  num1, num2: Integer;
  result: Integer;
  realNum: Real;

begin
  num1 := 5;
  num2 := 3;
  
  result := Add(num1, num2);
  writeln(num1, ' + ', num2, ' = ', result);
  
  result := Multiply(num1, num2);
  writeln(num1, ' * ', num2, ' = ', result);
  
  result := Factorial(5);
  writeln('5! = ', result);
  
  realNum := 3.14;
  writeln('Square of ', realNum, ' = ', Square(realNum));
end.