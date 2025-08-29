program IfBlockChecking;
var
   a , b : integer;
   flag : boolean;
begin
   a := 1;
   b := -1;
   if flag then
    begin 
        a := a+1
    end
   else
    begin 
        b := b-1;
    end;

    writeln(a);
    writeln(b);
end.