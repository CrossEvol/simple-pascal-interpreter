program EnumIndexExample;

{$mode objfpc}{$H+}

type
  TDayOfWeek = (Sun, Mon, Tue, Wed, Thu, Fri, Sat);

var
  d0, d1, d2, d3, d4, d5, d6 : TDayOfWeek;
  o0 ,o1,o2,o3,o4,o5,o6 : integer;

begin
  d0 := TDayOfWeek.Sun;
  d1 := TDayOfWeek.Mon;
  d2 := TDayOfWeek.Tue;
  d3 := TDayOfWeek.Wed;
  d4 := TDayOfWeek.Thu;
  d5 := TDayOfWeek.Fri;
  d6 := TDayOfWeek.Sat;
  WriteLn(d0);
  WriteLn(d1);
  WriteLn(d2);
  WriteLn(d3);
  WriteLn(d4);
  WriteLn(d5);
  WriteLn(d6);
  
  o0 := Ord(d0);
  o1 := Ord(d1);
  o2 := Ord(d2);
  o3 := Ord(d3);
  o4 := Ord(d4);
  o5 := Ord(d5);
  o6 := Ord(d6);
  WriteLn(o0);
  WriteLn(o1);
  WriteLn(o2);
  WriteLn(o3);
  WriteLn(o4);
  WriteLn(o5);
  WriteLn(o6);
end.