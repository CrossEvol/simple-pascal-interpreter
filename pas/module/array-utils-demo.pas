program ArrayUtilsDemo;

{
  This example demonstrates usage of the ArrayUtils standard library module.
  Shows array sorting, searching, copying, and size operations.
}

uses ArrayUtils;

var
  numbers: array[1..8] of integer;
  sortedNumbers: array[1..8] of integer;
  searchValue, foundIndex, arraySize: integer;
  i: integer;

begin
  writeln('=== ArrayUtils Module Demonstration ===');
  writeln();
  
  { Initialize array with unsorted values }
  numbers[1] := 64;
  numbers[2] := 34;
  numbers[3] := 25;
  numbers[4] := 12;
  numbers[5] := 22;
  numbers[6] := 11;
  numbers[7] := 90;
  numbers[8] := 5;
  
  writeln('Original array:');
  for i := 1 to 8 do
  begin
    write(numbers[i], ' ');
  end;
  writeln();
  writeln();
  
  { Get array size }
  arraySize := Size(numbers);
  writeln('Array size: ', arraySize);
  writeln();
  
  { Copy array before sorting }
  writeln('Copying array...');
  Copy(numbers, sortedNumbers);
  
  { Sort the copied array }
  writeln('Sorting array...');
  Sort(sortedNumbers);
  
  writeln('Sorted array:');
  for i := 1 to 8 do
  begin
    write(sortedNumbers[i], ' ');
  end;
  writeln();
  writeln();
  
  { Search for values in sorted array }
  searchValue := 25;
  foundIndex := Find(sortedNumbers, searchValue);
  if foundIndex >= 0 then
    writeln('Found ', searchValue, ' at index ', foundIndex)
  else
    writeln(searchValue, ' not found in array');
  
  searchValue := 100;
  foundIndex := Find(sortedNumbers, searchValue);
  if foundIndex >= 0 then
    writeln('Found ', searchValue, ' at index ', foundIndex)
  else
    writeln(searchValue, ' not found in array');
  
  writeln();
  
  { Show original array is unchanged }
  writeln('Original array (unchanged):');
  for i := 1 to 8 do
  begin
    write(numbers[i], ' ');
  end;
  writeln();
  
  writeln();
  writeln('ArrayUtils demonstration complete!');
end.