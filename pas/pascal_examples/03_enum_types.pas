program EnumTypes;

{
  演示枚举类型
}

type
  TColor = (Red, Green, Blue);
  TDay = (Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday);

var
  bgColor: TColor;
  today: TDay;

begin
  bgColor := Green;
  today := Monday;
  
  writeln('Background color: ', Ord(bgColor)); // 输出枚举值的序号
  writeln('Today is: ', Ord(today));
  
  // 枚举值比较
  if bgColor = Green then
    writeln('Background is green');
end.