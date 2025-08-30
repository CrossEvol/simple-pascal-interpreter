program MultipleModules;

{
  This example demonstrates using multiple modules together.
  Shows how different standard library modules can work together
  in a single program.
}

uses Math, Map, ArrayUtils;

var
  scores: array[1..5] of integer;
  studentMap: TMap;
  total, average: integer;
  i: integer;

begin
  writeln('=== Multiple Modules Example ===');
  writeln();
  
  { Initialize test scores }
  scores[1] := 85;
  scores[2] := 92;
  scores[3] := 78;
  scores[4] := 96;
  scores[5] := 88;
  
  writeln('Original test scores:');
  for i := 1 to 5 do
  begin
    write(scores[i], ' ');
  end;
  writeln();
  writeln();
  
  { Use ArrayUtils to sort scores }
  writeln('Sorting scores using ArrayUtils...');
  Sort(scores);
  
  writeln('Sorted scores:');
  for i := 1 to 5 do
  begin
    write(scores[i], ' ');
  end;
  writeln();
  writeln();
  
  { Use Math module to calculate total and average }
  writeln('Calculating statistics using Math module...');
  total := 0;
  for i := 1 to 5 do
  begin
    total := ADD(total, scores[i]);
  end;
  
  average := DIV(total, 5);
  
  writeln('Total: ', total);
  writeln('Average: ', average);
  writeln();
  
  { Use Map to store grade categories }
  writeln('Categorizing scores using Map module...');
  studentMap := TMap.Create;
  
  { Categorize each score }
  for i := 1 to 5 do
  begin
    if scores[i] >= 90 then
      studentMap.Put('A_grade', ADD(studentMap.Get('A_grade'), 1))
    else if scores[i] >= 80 then
      studentMap.Put('B_grade', ADD(studentMap.Get('B_grade'), 1))
    else if scores[i] >= 70 then
      studentMap.Put('C_grade', ADD(studentMap.Get('C_grade'), 1))
    else
      studentMap.Put('D_grade', ADD(studentMap.Get('D_grade'), 1));
  end;
  
  writeln('Grade distribution:');
  writeln('A grades (90+): ', studentMap.Get('A_grade'));
  writeln('B grades (80-89): ', studentMap.Get('B_grade'));
  writeln('C grades (70-79): ', studentMap.Get('C_grade'));
  writeln('D grades (<70): ', studentMap.Get('D_grade'));
  
  writeln();
  writeln('Multiple modules demonstration complete!');
end.