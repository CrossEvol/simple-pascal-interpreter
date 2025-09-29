program Loops;

{演示循环语句}

var
  i, j: Integer;
  sum: Integer;
  count: Integer;
  ch: Char;

begin
  { for循环 - 递增 }
  writeln('For loop (1 to 5):');
  for i := 1 to 5 do
    write(i, ' ');
  writeln();
  
  { 计算1到10的和 }
  sum := 0;
  for i := 1 to 10 do
    sum := sum + i;
  writeln('Sum of 1 to 10: ', sum);
  
  { while循环 }
  writeln('While loop (count from 1 to 5):');
  count := 1;
  while count <= 5 do
  begin
    write(count, ' ');
    count := count + 1;
  end;
  writeln();
  
  
  { 嵌套循环 }
  writeln('Nested loops (multiplication table):');
  for i := 1 to 3 do
  begin
    for j := 1 to 3 do
      write(i * j);
    writeln();
  end;
end.