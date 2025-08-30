program StringDemo;

{
  This example demonstrates using a custom unit (StringUtils).
  Shows how to create and use your own units alongside standard library modules.
}

uses StringUtils;

var
  text1, text2, result: string;

begin
  writeln('=== Custom Unit (StringUtils) Example ===');
  writeln();
  
  text1 := 'Hello';
  text2 := 'World';
  
  writeln('Original strings:');
  writeln('text1: "', text1, '"');
  writeln('text2: "', text2, '"');
  writeln();
  
  { Test string length }
  writeln('String lengths:');
  writeln('Length of "', text1, '": ', StringLength(text1));
  writeln('Length of "', text2, '": ', StringLength(text2));
  writeln();
  
  { Test string concatenation }
  result := StringConcat(text1, text2);
  writeln('Concatenation:');
  writeln('"', text1, '" + "', text2, '" = "', result, '"');
  writeln();
  
  { Test case conversion }
  writeln('Case conversion:');
  writeln('UpperCase("', text1, '"): "', UpperCase(text1), '"');
  writeln('LowerCase("', text1, '"): "', LowerCase(text1), '"');
  writeln();
  
  { Test string reversal }
  writeln('String reversal:');
  writeln('Reverse("', text1, '"): "', StringReverse(text1), '"');
  writeln('Reverse("', text2, '"): "', StringReverse(text2), '"');
  
  { Test with concatenated string }
  result := StringConcat(text1, text2);
  writeln('Reverse("', result, '"): "', StringReverse(result), '"');
  
  writeln();
  writeln('Custom unit demonstration complete!');
end.