program MapModuleDemo;

{
  This example demonstrates usage of the Map standard library module.
  Shows how to create, populate, and manipulate a key-value map.
}

uses Map;

var
  studentGrades: TMap;
  keys: array of string;
  values: array of integer;
  i: integer;

begin
  writeln('=== Map Module Demonstration ===');
  writeln();
  
  { Create a new map instance }
  studentGrades := TMap.Create;
  
  { Add some student grades }
  writeln('Adding student grades...');
  studentGrades.Put('Alice', 95);
  studentGrades.Put('Bob', 87);
  studentGrades.Put('Charlie', 92);
  studentGrades.Put('Diana', 88);
  studentGrades.Put('Eve', 96);
  writeln('Added 5 student grades');
  writeln();
  
  { Retrieve specific grades }
  writeln('Individual grade lookups:');
  writeln('Alice: ', studentGrades.Get('Alice'));
  writeln('Bob: ', studentGrades.Get('Bob'));
  writeln('Charlie: ', studentGrades.Get('Charlie'));
  writeln();
  
  { Get all keys and values }
  writeln('All students and their grades:');
  keys := studentGrades.Keys();
  values := studentGrades.Values();
  
  for i := 0 to Length(keys) - 1 do
  begin
    writeln(keys[i], ': ', values[i]);
  end;
  writeln();
  
  { Remove a student }
  writeln('Removing Bob from the map...');
  studentGrades.Remove('Bob');
  
  { Show updated list }
  writeln('Updated student list:');
  keys := studentGrades.Keys();
  values := studentGrades.Values();
  
  for i := 0 to Length(keys) - 1 do
  begin
    writeln(keys[i], ': ', values[i]);
  end;
  
  writeln();
  writeln('Map demonstration complete!');
end.