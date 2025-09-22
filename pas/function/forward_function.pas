program ForwardFunction;

var 
    sum : Integer;

function Add(a, b: Integer): Integer; forward;

function DoubleAdd(a,b:Integer):Integer;
    begin
    DoubleAdd := Add(a,b) + Add(a,b);
    end;

function Add(a, b: Integer): Integer;
    begin
    Add := a + b;
    end;

begin
    sum := DoubleAdd(3,4);
    WriteLn(sum); { 14 }
end.
