program DataTypes;

{
  演示Pascal中的数据类型
}

var
  intVar: Integer;
  realVar: Real;
  boolVar: Boolean;
  charVar: Char;
  stringVar: String;

begin
  intVar := 42;
  realVar := 3.14159;
  boolVar := True;
  charVar := 'A';
  stringVar := 'Hello, Pascal!';
  
  writeln('Integer: ', intVar);
  writeln('Real: ', realVar:0:5);
  writeln('Boolean: ', boolVar);
  writeln('Char: ', charVar);
  writeln('String: ', stringVar);
end.