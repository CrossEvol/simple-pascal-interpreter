program Fibonacci;

{ Calculate Fibonacci sequence in Pascal using memoized recursion }

var
  i: integer;
  memo: array[1..30] of integer;

procedure init_memo;
var
  j: integer;
begin
  for j := 1 to 30 do
    memo[j] := 0;  { 0 means not computed }
end;

function fib(n: integer): integer;
begin
  if memo[n] <> 0 then
    fib := memo[n]
  else
  begin
    if n <= 2 then
      fib := 1
    else
      fib := fib(n - 1) + fib(n - 2);
    memo[n] := fib;
  end;
end;

begin
  init_memo();
  writeln('Fibonacci sequence (first 30 terms):');
  writeln('------------------------------------');
  
  for i := 1 to 30 do
  begin
    writeln('Term ', i, ': ', fib(i));
  end;
  
  writeln('------------------------------------');
  writeln('Calculation complete!');
  
  { Wait for user input before closing }
  writeln('Press Enter to exit...');
end.