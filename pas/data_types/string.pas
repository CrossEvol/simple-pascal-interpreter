program StringExample;
var
  str1: string[7]; {Declare a string wth a maximum size}
  str2 : string; {Declare a string without a maximum size}
  str3 : string; {will be used for setLength()}
  concat1 : string; {will be used for s1 + s2}
  concat2 : string[7]; {will be used for s1 + s2}
  a, b : string; {will use subscript to extra char from string}
  l1 , l2 , l3: integer;

begin
  str1 := 'abcdefghijklmnopqrstuvwxyz'; { will warn in sematic analyzer }
  str2 := 'abcdefghijklmnopqrstuvwxyz';
  str3 := str2;

  l1 := length(str1);
  l2 := length(str2);

  setLength(str3,14);
  l3 := length(str3);
  a := str3[1];
  b := str3[36];
  concat1 := str1 + '123' + '456' + str2[1];
  concat2 := str1 + '123' + '456' + str2[1]; { will warn in interpreter }

  writeln(str1);
  writeln(str2);
  writeln(str3);
  writeln(concat1);
  writeln(concat2);
end.