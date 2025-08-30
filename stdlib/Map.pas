unit Map;

interface
  type
    TMapEntry = record
      key: string;
      value: integer;
      used: boolean;
    end;

    TMap = class
      private
        entries: array[0..99] of TMapEntry;
        count: integer;
        
      public
        constructor Create();
        procedure Put(key: string; value: integer);
        function Get(key: string): integer;
        procedure Remove(key: string);
        function Keys(): array of string;
        function Values(): array of integer;
        function Size(): integer;
    end;

implementation
  constructor TMap.Create();
  var
    i: integer;
  begin
    count := 0;
    for i := 0 to 99 do
    begin
      entries[i].used := false;
    end;
  end;

  procedure TMap.Put(key: string; value: integer);
  var
    i: integer;
    found: boolean;
  begin
    found := false;
    
    { First check if key already exists }
    for i := 0 to 99 do
    begin
      if entries[i].used and (entries[i].key = key) then
      begin
        entries[i].value := value;
        found := true;
        break;
      end;
    end;
    
    { If key doesn't exist, find empty slot }
    if not found then
    begin
      for i := 0 to 99 do
      begin
        if not entries[i].used then
        begin
          entries[i].key := key;
          entries[i].value := value;
          entries[i].used := true;
          count := count + 1;
          break;
        end;
      end;
    end;
  end;

  function TMap.Get(key: string): integer;
  var
    i: integer;
  begin
    Get := 0; { Default return value }
    
    for i := 0 to 99 do
    begin
      if entries[i].used and (entries[i].key = key) then
      begin
        Get := entries[i].value;
        break;
      end;
    end;
  end;

  procedure TMap.Remove(key: string);
  var
    i: integer;
  begin
    for i := 0 to 99 do
    begin
      if entries[i].used and (entries[i].key = key) then
      begin
        entries[i].used := false;
        count := count - 1;
        break;
      end;
    end;
  end;

  function TMap.Keys(): array of string;
  var
    i, j: integer;
    result: array[0..99] of string;
    resultCount: integer;
  begin
    resultCount := 0;
    
    for i := 0 to 99 do
    begin
      if entries[i].used then
      begin
        result[resultCount] := entries[i].key;
        resultCount := resultCount + 1;
      end;
    end;
    
    { Return array with actual size }
    Keys := result;
  end;

  function TMap.Values(): array of integer;
  var
    i, j: integer;
    result: array[0..99] of integer;
    resultCount: integer;
  begin
    resultCount := 0;
    
    for i := 0 to 99 do
    begin
      if entries[i].used then
      begin
        result[resultCount] := entries[i].value;
        resultCount := resultCount + 1;
      end;
    end;
    
    { Return array with actual size }
    Values := result;
  end;

  function TMap.Size(): integer;
  begin
    Size := count;
  end;

end.