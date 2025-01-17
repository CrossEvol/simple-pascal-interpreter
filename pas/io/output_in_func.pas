program SimpleFunction;
var 
  a : integer;
  b : integer;

function sayHello(b: integer):Integer;
begin
  sayHello := a+b;
  Writeln(sayHello);
end;

begin {Main}
  a := 1;          { Initialize the variable 'a' within the main block }
  b := sayHello(2);     { Call the function 'sayHello' with argument 7 }
end. {Main}