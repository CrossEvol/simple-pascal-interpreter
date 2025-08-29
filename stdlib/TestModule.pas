unit TestModule;

interface
  function TestFunction(): integer;

implementation
  function TestFunction(): integer;
  begin
    TestFunction := 42;
  end;

end.