program ArraySetLength;

var
  arr: array of Integer; 
  i, j, k: Integer;
begin
  i := Length(arr);
  writeln(i);
  setLength(arr,10);
  j := Length(arr);
  writeln(j);

  for k := 1 to 10 do
    begin
        arr[k-1] := k;
        writeln(arr[k-1]);
    end;

  setLength(arr,5); 

    for k:= 0 to 4 do
    begin
        writeln(arr[k]);
    end;
end.