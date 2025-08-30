unit ArrayUtils;

interface
  procedure Sort(var arr: array of integer; size: integer);
  function Find(arr: array of integer; size: integer; value: integer): integer;
  procedure Copy(source: array of integer; var dest: array of integer; size: integer);
  function Size(arr: array of integer): integer;

implementation
  procedure Sort(var arr: array of integer; size: integer);
  var
    i, j, temp: integer;
  begin
    { Bubble sort implementation }
    for i := 0 to size - 2 do
    begin
      for j := 0 to size - 2 - i do
      begin
        if arr[j] > arr[j + 1] then
        begin
          temp := arr[j];
          arr[j] := arr[j + 1];
          arr[j + 1] := temp;
        end;
      end;
    end;
  end;

  function Find(arr: array of integer; size: integer; value: integer): integer;
  var
    i: integer;
  begin
    Find := -1; { Return -1 if not found }
    
    for i := 0 to size - 1 do
    begin
      if arr[i] = value then
      begin
        Find := i;
        break;
      end;
    end;
  end;

  procedure Copy(source: array of integer; var dest: array of integer; size: integer);
  var
    i: integer;
  begin
    for i := 0 to size - 1 do
    begin
      dest[i] := source[i];
    end;
  end;

  function Size(arr: array of integer): integer;
  begin
    { Note: In Pascal, dynamic array size is not directly accessible }
    { This function would need to be called with a known size parameter }
    { For this implementation, we'll return a placeholder }
    Size := 0;
  end;

end.