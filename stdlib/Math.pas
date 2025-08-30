unit Math;

interface
  function ADD(a, b: integer): integer;
  function MUL(a, b: integer): integer;
  function SUB(a, b: integer): integer;
  function DIV(a, b: integer): integer;
  function MOD(a, b: integer): integer;

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

  function DIV(a, b: integer): integer;
  begin
    if b = 0 then
    begin
      { Division by zero - return 0 as error indicator }
      DIV := 0;
    end
    else
    begin
      DIV := a div b;
    end;
  end;

  function MOD(a, b: integer): integer;
  begin
    if b = 0 then
    begin
      { Modulo by zero - return 0 as error indicator }
      MOD := 0;
    end
    else
    begin
      MOD := a mod b;
    end;
  end;

end.