program ExitExample;

var 
    count : Integer;

procedure Proc4;
begin
  writeln('enter Proc4');
  count := count + 4;
  writeln('leave Proc4');
end;

procedure Proc3;
begin
  writeln('enter Proc3');
  count := count + 3;
  {模拟某种条件，这里直接使用Exit提前退出}
  if true then  {可以替换为实际条件}
    Exit();  {提前退出Proc3，不会执行下面的代码}
  writeln('调用Proc4');  {这行不会执行}
  Proc4();
  writeln('leave Proc3');  {这行也不会执行}
end;

procedure Proc2;
begin
  writeln('enter Proc2');
  count := count + 2;
  Proc3();
  writeln('leave Proc2');
end;

procedure Proc1;
begin
  writeln('enter Proc1');
  count := count + 1;
  Proc2();
  writeln('leave Proc1');
end;

begin
  Proc1();
  writeln(count); {6}
end.