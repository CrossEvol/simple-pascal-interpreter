unit Math;

interface
  function ADD(a, b: integer): integer;
  function MUL(a, b: integer): integer;
  function SUB(a, b: integer): integer;
  function DIVIDE(a, b: integer): integer;
  function MODULO(a, b: integer): integer;

implementation
  function ADD(a, b: integer): integer;
  begin
    ADD := a + b;
  end;

  function MUL(a, b: integer): integer;
  begin
    MUL := a * b;
  end;

  function SUB(a, b: integer): integer;
  begin
    SUB := a - b;
  end;

  function DIVIDE(a, b: integer): integer;
  begin
    if b = 0 then
    begin
      { Division by zero - return 0 as error indicator }
      DIVIDE := 0;
    end
    else
    begin
      DIVIDE := a div b;
    end;
  end;

  function MODULO(a, b: integer): integer;
  begin
    if b = 0 then
    begin
      { Modulo by zero - return 0 as error indicator }
      MODULO := 0;
    end
    else
    begin
      MODULO := a mod b;
    end;
  end;

end.