program Fibonacci;

{ Calculate Fibonacci sequence in Pascal }

var
  i: integer;
  a, b, temp: integer;

begin
  writeln('Fibonacci sequence (first 15 terms):');
  writeln('------------------------------------');
  
  { Initialize first two terms }
  a := 1;
  b := 1;
  
  { Output first and second terms }
  writeln('Term  1: ', a);
  writeln('Term  2: ', b);
  
  { Calculate and output terms 3 to 15 }
  for i := 3 to 15 do
  begin
    temp := a + b;  { Calculate next term }
    writeln('Term ', i, ': ', temp);
    a := b;         { Update a to previous term }
    b := temp;      { Update b to current term }
  end;
  
  writeln('------------------------------------');
  writeln('Calculation complete!');
  
  { Wait for user input before closing }
  writeln('Press Enter to exit...');
end.