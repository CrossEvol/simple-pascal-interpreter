program ArrayTypes;

{演示数组类型}

const
  MAX_SIZE = 5;

type
  TIntegerArray = array[1..MAX_SIZE] of Integer;
  TStringArray = array[0..2] of String[20];

var
  numbers: TIntegerArray;
  names: TStringArray;
  i: Integer;

begin
  {初始化数组}
  for i := 1 to MAX_SIZE do
    numbers[i] := i * 10;
    
  names[0] := 'Alice';
  names[1] := 'Bob';
  names[2] := 'Charlie';
  
  {输出数组内容}
  writeln('Numbers:');
  for i := 1 to MAX_SIZE do
    writeln('  numbers[', i, '] = ', numbers[i]);
    
  writeln('Names:');
  for i := 0 to 2 do
    writeln('  names[', i, '] = ', names[i]);
end.