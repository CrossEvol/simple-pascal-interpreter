program ConditionalStatements;

{
  演示条件语句
}

var
  number: Integer;
  grade: Char;

begin
  number := 15;
  
  { 简单if语句 }
  if number > 10 then
    writeln(number, ' is greater than 10');
    
  { if-else语句 }
  if (number - (number div 2) * 2) = 0 then
    writeln(number, ' is even')
  else
    writeln(number, ' is odd');
    
  { 复合语句 }
  if number > 0 then
  begin
    writeln(number, ' is positive');
    writeln('Its square is ', number * number);
  end
  else if number < 0 then
  begin
    writeln(number, ' is negative');
    writeln('Its absolute value is ', -number);
  end
  else
    writeln(number, ' is zero');
    
  { case语句 }
  grade := 'B';
  case grade of
    'A': writeln('Excellent!');
    'B': writeln('Good job!');
    'C': writeln('Average');
    'D': writeln('Below average');
    'F': writeln('Fail');
  else
    writeln('Invalid grade');
  end;
end.