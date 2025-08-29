program Main;

var x : integer;

procedure Alpha();

   procedure Beta();
   begin
      x := x +  20;
   end;

begin
   x := 10;
   Beta();      { procedure call }
end;

begin { Main }

   Alpha();  { procedure call }
   WRITE(x);

end.  { Main }
