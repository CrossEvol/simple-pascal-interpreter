program SumExample;

var 
    sum : Integer;

function Sum4(): integer;
begin
  Sum4 := 4;
end;

function Sum3(): integer;
begin
  Sum3 := 3;
  {模拟条件，这里直接提前返回}
  if true then  {可以替换为实际条件}
    Exit();  {提前退出Sum3，不会执行下面的加法}
  Sum3 := Sum3 + Sum4();  {这行不会执行}
end;

function Sum2(): integer;
begin
  Sum2 := 2 + Sum3();
end;

function Sum1(): integer;
begin
  Sum1 := 1 + Sum2();
end;

begin
  sum := Sum1();
  writeln('sum 的结果: ', sum);  {输出 6，验证提前返回的效果}
end.