program Procedures;

{演示过程定义和调用}

{ 简单过程 }
procedure Greet(name: String);
begin
  writeln('Hello, ', name, '!');
end;

{ 带var参数的过程（可修改参数） }
procedure Swap(var a, b: Integer);
var
  temp: Integer;
begin
  temp := a;
  a := b;
  b := temp;
end;

{ 带const参数的过程（不可修改参数） }
procedure PrintValue(const value: Integer);
begin
  writeln('Value is: ', value);
end;

var
  x, y: Integer;

begin
  Greet('Alice');
  
  x := 10;
  y := 20;
  writeln('Before swap: x = ', x, ', y = ', y);
  Swap(x, y);
  writeln('After swap: x = ', x, ', y = ', y);
  
  PrintValue(x);
end.