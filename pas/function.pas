program SimpleFunction;
var
  sum: Integer;
function Add(a, b: Integer): Integer;
begin
  Add := a + b;
end;

begin
  sum := Add(5, 3);
  {WriteLn('Sum: ', sum);}
end.