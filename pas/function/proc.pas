program NestedProcedures;

procedure OuterProcedure;
var
  outerVar: Integer;

  procedure InnerProcedure;
  var
    innerVar: Integer;
  begin
    outerVar := 0;
    innerVar := 5;
    outerVar := outerVar + innerVar;
  end;

begin
  outerVar := 10;
  InnerProcedure();  
end;

begin
  OuterProcedure();  
end.