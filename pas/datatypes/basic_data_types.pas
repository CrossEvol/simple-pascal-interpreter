program BasicDataTypes;
var
  intVar: Integer;
  realVar: Real;
  charVar: Char;
  boolVar: Boolean;
begin
  intVar := 10;
  realVar := 3.14;
  charVar := 'A';
  boolVar := True;

  WriteLn('Integer: ', intVar);
  WriteLn('Real: ', realVar);
  WriteLn('Char: ', charVar);
  WriteLn('Boolean: ', boolVar);
end.