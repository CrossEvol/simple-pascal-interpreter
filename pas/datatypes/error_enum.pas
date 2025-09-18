program EnumDemo;

type
  {定义一个名为 TColor 的枚举类型}
  TColor = (Red, Green, Blue, Yellow, Purple);

var
  {声明 TColor 类型的变量}
  myColor: TColor;
  {声明一个整数变量用于存储枚举值的序号}
  c1,c2,c3, c4,c5: Integer;

begin
  {遍历所有枚举值}
  for myColor := Red to Purple do
  begin
    writeln(myColor);
    case myColor of
      Red:    c1 := Ord(myColor);
      Green:  c2 := Ord(myColor);
      Blue:   c3 := Ord(myColor);
      Yellow: c4 := Ord(myColor);
      Purple: c5 := Ord(myColor);
    end;
  end;

  writeln(c1);
  writeln(c2);
  writeln(c3);
  writeln(c4);
  writeln(c5);

end.