program BasicIO;
var
  name: String;
  age: Integer;
begin
  Write('Enter your name: ');
  ReadLn(name);
  Write('Enter your age: ');
  ReadLn(age);

  WriteLn('Your name is ', name, ' and you are ', age, ' years old.');
end.