unit StringUtils;

{
  Custom unit example demonstrating how to create your own units.
  This unit provides string manipulation utilities.
}

interface
  { Public function declarations }
  function UpperCase(s: string): string;
  function LowerCase(s: string): string;
  function StringLength(s: string): integer;
  function StringConcat(s1, s2: string): string;
  function StringReverse(s: string): string;

implementation
  { Private helper function - not visible outside this unit }
  function IsLowerCase(c: char): boolean;
  begin
    IsLowerCase := (c >= 'a') and (c <= 'z');
  end;
  
  function IsUpperCase(c: char): boolean;
  begin
    IsUpperCase := (c >= 'A') and (c <= 'Z');
  end;
  
  { Public function implementations }
  function UpperCase(s: string): string;
  var
    i: integer;
    result: string;
  begin
    result := s;
    for i := 1 to StringLength(s) do
    begin
      if IsLowerCase(s[i]) then
        result[i] := chr(ord(s[i]) - 32);
    end;
    UpperCase := result;
  end;
  
  function LowerCase(s: string): string;
  var
    i: integer;
    result: string;
  begin
    result := s;
    for i := 1 to StringLength(s) do
    begin
      if IsUpperCase(s[i]) then
        result[i] := chr(ord(s[i]) + 32);
    end;
    LowerCase := result;
  end;
  
  function StringLength(s: string): integer;
  var
    len: integer;
  begin
    len := 0;
    while (len < 255) and (s[len + 1] <> #0) do
      len := len + 1;
    StringLength := len;
  end;
  
  function StringConcat(s1, s2: string): string;
  begin
    StringConcat := s1 + s2;
  end;
  
  function StringReverse(s: string): string;
  var
    i, len: integer;
    result: string;
  begin
    len := StringLength(s);
    result := '';
    for i := len downto 1 do
      result := result + s[i];
    StringReverse := result;
  end;

end.