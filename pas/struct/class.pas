program SimpleClass;

{$mode objfpc} // directive to be used for defining classes
{$m+}		   // directive to be used for using constructor

type
  TPerson = class
    private
      name: String;
      age: Integer;
    public
      procedure SetData(aName: String; aAge: Integer);
      procedure PrintData;
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

var
  person: TPerson;
begin
  person := TPerson.Create;
  person.SetData('Alice', 30);
  person.PrintData;
  person.Free;
end.