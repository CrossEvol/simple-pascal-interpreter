program EnumDemo;

type
  {定义一个名为 TColor 的枚举类型}
  TColor = (Red, Green, Blue, Yellow, Purple);

var
  {声明 TColor 类型的变量}
  r, g , b , y , p: TColor;
  myColor: TColor;
  {声明一个整数变量用于存储枚举值的序号}
  c1,c2,c3, c4,c5: Integer;
  f1, f2 , f3, f4, f5, f6:Boolean;

begin
  {给枚举变量赋值}
  r := Red;
  g := Green;
  b := Blue;
  y := Yellow;
  p := Purple;

 

  {比较枚举值}
  f1 := r = Red;
  f2 := r <> Green;
  f3 := g < Blue;
  f4 := b > Yellow;
  f5 := y <= Purple;
  f6 := p >= Red;

  {遍历所有枚举值}
  for myColor := Red to Purple do
  begin
    case myColor of
      Red:    c1 := 0;
      Green:  c2 := 1;
      Blue:   c3 := 2;
      Yellow: c4 := 3;
      Purple: c5 := 4;
    end;
  end;

  {注意：不能直接 Write(myColor) 输出枚举名称（某些编译器支持，但非标准）}
  {标准做法是使用 Case 语句转换为字符串输出}
end.