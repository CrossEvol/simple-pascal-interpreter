program const_example;

const
  PI = 3.141592654;
  FLAG = true;
  NUM = 100;
  STR = 'str';
  ARR: array[0..2] of Integer = (1, 2, 3);  {Declaring an array constant}

var
  i: Integer;

begin
  writeln(PI);
  writeln(FLAG);
  writeln(NUM);
  writeln(STR);

  {Accessing and printing the array elements}
  for i := Low(ARR) to High(ARR) do
    writeln('ARR[', i, '] = ', ARR[i]);
end.