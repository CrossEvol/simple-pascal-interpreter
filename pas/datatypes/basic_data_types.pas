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
  WriteLn('Real: ', realVar:0:2);
  WriteLn('Char: ', charVar);
  WriteLn('Boolean: ', boolVar);
end.