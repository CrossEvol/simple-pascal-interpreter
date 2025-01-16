program CaseOfExample;

{$mode objfpc}{$H+}

type
  Day = (Sun, Mon, Tue, Wed, Thu, Fri, Sat);

var
  i1, i2: Integer;
  f1, f2: BOOLEAN;
  s1, s2, s3,s4: String;
  d1, d2 : Day;
  

begin
  i1 := 2;

  case i1 of
    1: i2 := 10;
    2: i2 := 20;
    3: i2 := 30;
  else
    Writeln('Invalid!');
  end;
  Writeln(i2);

  f1 := true;
  case f1 of
    true: f2 := true;
    false: f2 := false;
  else
    Writeln('Invalid!');
  end;
  Writeln(f2);

  s1 := 'a';
  case s1 of
    'a': s2 := 'a';
    'b': s2 := 'b';
    'c': s2 := 'c';
  else
    Writeln('Invalid!');
  end;
  Writeln(s2);

  s3 := 'a';
  case s1 of
    'a1': s4 := 'a';
    'b1': s4 := 'b';
    'c1': s4 := 'c';
  else
    s4 := 'd';
  end;
  Writeln(s4);

  d1 := Day.Sun;
  case d1 of
    Day.Sun: d2 := Day.Sun;
    Day.Mon: d2 := Day.Mon;
    Day.Tue: d2 := Day.Tue;
    Day.Wed: d2 := Day.Wed;
    Day.Thu: d2 := Day.Thu;
    Day.Fri: d2 := Day.Fri;
    Day.Sat: d2 := Day.Sat;
  else
    Writeln('Invalid!');
  end;
  Writeln(d2);
end.