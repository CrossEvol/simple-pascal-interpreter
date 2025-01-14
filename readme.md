# Pascal syntax subset
## write/writeln
only support code like 
```pascal
WriteLn(a);
WriteLn(a+b);
WriteLn('a = ',a,' ','b = ',b);
```
do not support string.format() functionality.

## object pascal
should have the below previa as directives, but I do not want to parse them, only useful when copy and paste program to pascal compiler
```pascal
{$mode objfpc} // directive to be used for defining classes
{$m+}		   // directive to be used for using constructor
```

# TODO
## can not distinguish function call and procedure call if there is not assign operator
for example 
```pascal
begin
    call();
end.
```
it will fall into the procedure_call unless it is the below form 
```pascal
begin
   a := call();
end.
```