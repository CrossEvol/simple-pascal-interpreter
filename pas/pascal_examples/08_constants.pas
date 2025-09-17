program Constants;

{
  演示常量定义
}

const
  PI = 3.14159;
  MAX_SIZE = 100;
  GREETING = 'Hello, World!';
  TRUE_VALUE = True;
  FALSE_VALUE = False;

var
  radius: Real;
  area: Real;
  i: Integer;
  message: String;

begin
  radius := 5.0;
  area := PI * radius * radius;
  
  writeln('PI = ', PI:0:5);
  writeln('Radius = ', radius:0:2);
  writeln('Area = ', area:0:2);
  writeln('Max Size = ', MAX_SIZE);
  writeln('Greeting: ', GREETING);
  
  message := GREETING;
  writeln('Message: ', message);
end.