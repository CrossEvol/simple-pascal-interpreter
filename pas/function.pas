program SimpleFunction;
function Add(a, b: Integer): Integer;
begin
  Add := a + b;
end;

var
  sum: Integer;
begin
  sum := Add(5, 3);
  WriteLn('Sum: ', sum);
end.