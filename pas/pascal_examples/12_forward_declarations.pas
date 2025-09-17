program ForwardDeclarations;

{
  演示前向声明
}

// 前向声明
procedure SecondProcedure; forward;

procedure FirstProcedure;
begin
  writeln('This is the first procedure');
  SecondProcedure; // 调用第二个过程
end;

procedure SecondProcedure;
begin
  writeln('This is the second procedure');
end;

// 递归函数的前向声明示例
function IsEven(n: Integer): Boolean; forward;
function IsOdd(n: Integer): Boolean; forward;

function IsEven(n: Integer): Boolean;
begin
  if n = 0 then
    IsEven := True
  else
    IsEven := IsOdd(n - 1);
end;

function IsOdd(n: Integer): Boolean;
begin
  if n = 0 then
    IsOdd := False
  else
    IsOdd := IsEven(n - 1);
end;

begin
  FirstProcedure;
  
  writeln('Is 4 even? ', IsEven(4));
  writeln('Is 4 odd? ', IsOdd(4));
  writeln('Is 5 even? ', IsEven(5));
  writeln('Is 5 odd? ', IsOdd(5));
end.