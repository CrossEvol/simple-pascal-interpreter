program StandardFunctions;

{
  演示标准函数和过程
}

uses
  math;

var
  ch: Char;
  str: String;
  num: Real;
  intNum: Integer;

begin
  // 字符处理函数
  ch := 'A';
  writeln('Character: ', ch);
  writeln('ASCII value of ''', ch, ''': ', Ord(ch));
  writeln('Character with ASCII 66: ', Chr(66));
  
  // 字符串处理
  str := 'Hello, Pascal!';
  writeln('String: ', str);
  writeln('Length of string: ', Length(str));
  
  // 数学函数
  num := 9.0;
  writeln('Square root of ', num:0:2, ': ', sqrt(num):0:2);
  
  num := 2.0;
  writeln(num:0:2, ' raised to power 3: ', power(num, 3):0:2);
  
  num := -5.7;
  writeln('Absolute value of ', num:0:2, ': ', abs(num):0:2);
  
  // 舍入函数
  num := 3.7;
  writeln('Round(', num:0:2, ') = ', Round(num));
  
  num := 3.2;
  writeln('Trunc(', num:0:2, ') = ', Trunc(num));
  
  // 内存管理
  writeln('Using New and Dispose for dynamic memory allocation');
  // 示例在指针类型示例中更详细
end.