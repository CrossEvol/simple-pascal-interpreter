program StandardFunctions;

{演示标准函数和过程}

var
  ch: Char;
  str: String;
  num: Real;
  intNum: Integer;

begin
  { 字符处理函数 }
  ch := 'A';
  writeln('Character: ', ch);
  writeln('ASCII value of ', ch, ': ', Ord(ch));
  writeln('Character with ASCII 66: ', Chr(66));
  
  { 字符串处理 }
  str := 'Hello, Pascal!';
  writeln('String: ', str);
  writeln('Length of string: ', Length(str));
  
end.