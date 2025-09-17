program exArrays;
var
    n: array [1..10] of integer;
    i, j, sum: integer;

begin
    for i := 1 to 10 do
        n[ i ] := i + 100;
    for j := 1 to 10 do
        writeln(n[j]);
        sum := sum + n[j];
end.