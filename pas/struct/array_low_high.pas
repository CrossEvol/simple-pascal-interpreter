program array_low_high;
var
    n: array [-10..10] of integer;
    a, b : integer; 

begin
    a := LOW(n);
    b := HIGH(n);
    writeln('a = ', a );
    writeln('b = ', b );
end.