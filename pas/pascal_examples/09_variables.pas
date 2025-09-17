program Variables;

{
  演示变量声明和作用域
}

var
  globalVar: Integer; // 全局变量

procedure TestProcedure;
var
  localVar: Integer; // 局部变量
begin
  globalVar := 20;
  localVar := 30;
  writeln('In procedure - GlobalVar: ', globalVar);
  writeln('In procedure - LocalVar: ', localVar);
end;

begin
  globalVar := 10;
  writeln('In main - GlobalVar: ', globalVar);
  
  TestProcedure;
  
  writeln('In main after procedure - GlobalVar: ', globalVar);
  // writeln(localVar); // 这会出错，因为localVar是局部变量
end.