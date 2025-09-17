program ArrayLength;

var
  arr: array[1..5] of Integer; 
  i: Integer;
begin
  for i := 1 to 5 do
  begin
    arr[i] := i + 1;
  end;

  i := Length(arr);
  writeln(i);

end.