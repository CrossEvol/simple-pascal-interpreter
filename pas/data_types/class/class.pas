program SimpleClass;

{$mode objfpc} // directive to be used for defining classes
{$m+}		   // directive to be used for using constructor

type
  TPerson = class
    private
      name: String;
      age: Integer;
    public
      constructor Create(aName: String; aAge: Integer);
      procedure SetData(aName: String; aAge: Integer);
      procedure PrintData;
      function getName():String;
      function getAge():Integer;
  end;

constructor TPerson.Create(aName: String; aAge: Integer);
begin
  name := aName;
  age := aAge;
end;

procedure TPerson.SetData(aName: String; aAge: Integer);
begin
  name := aName;
  age := aAge;
end;

procedure TPerson.PrintData;
begin
  WriteLn('Name: ', name);
  WriteLn('Age: ', age);
end;


function TPerson.getName:String;
begin
    getName := name;
end;

function TPerson.getAge:Integer;
begin
    getAge := age;
end;

var
  person: TPerson;
begin
  person := TPerson.Create('Alice', 30);
  person.PrintData;
  person.SetData('Tom', 18);
  person.PrintData;
  WriteLn(person.getName);
  WriteLn(person.getAge);
  {person.Free;}
end.