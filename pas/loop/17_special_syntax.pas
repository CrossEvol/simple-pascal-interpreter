program SpecialSyntax;

{演示特殊语法}

var
  ch: Char;
  i: Integer;

begin
  { 字符常量 }
  writeln('Character constants:');
  writeln('Null character: ', Ord(#0));
  writeln('Tab character: ', Ord(#9));
  writeln('Line feed: ', Ord(#10));
  writeln('Carriage return: ', Ord(#13));
  
  { 格式化输出 }
  writeln('Formatted output:');
  writeln('Integer: ', 42); { 至少5个字符宽 }
  writeln('Real: ', 3.14159); { 总宽8，小数点后3位 }
  
  
  { 循环和条件控制 }
  writeln('Loop control:');
  for i := 1 to 10 do
  begin
    if i = 5 then
      Continue; { 跳过本次循环 }
      
    if i = 8 then
      Break; { 退出循环 }
      
    write(i, ' ');
  end;
  writeln();
  
  { Exit语句 }
  writeln('Exit example:');
  for i := 1 to 5 do
  begin
    if i = 3 then
    begin
      writeln('Exiting at i = ', i);
      Exit(); { 退出整个程序 }
    end;
    writeln('i = ', i);
  end;
  writeln('This line will not be printed');
end.