# Originated from 
[Let's build a simple interpreter](https://github.com/rspivak/lsbasi)

# Pascal syntax subset
## DataTypes
primitive type
- integer
- real
- bool

derived type
- enum
- const

enum will not occupy the global identifier, implementation here should use enum variable with fullname like "Day.Sun" not "Sun".

composite type
- string 
- class 
- record
- array

not support class inheritance. only support class fields, methods , constructor and destructor.

## io
only support code like 
```pascal
Write(a);
WriteLn(a);
WriteLn(a+b);
Read(a);
ReadLn(a);
WriteLn('a = ',a,' ','b = ',b);
```
do not support string.format() functionality.

## function 
support procedure and function, inner params will hide outer params when they are in the same name

## flow 
- for 
- while
- if-else
- case-of

## operation 
- comparison 
- logic calculus

## object pascal
should have the below previa as directives, but I do not want to parse them, only useful when copy and paste program to pascal compiler
```pascal
{$mode objfpc} // directive to be used for defining classes
{$m+}		   // directive to be used for using constructor
```

## built-in functions
- READ
- READLN
- WRITE
- WRITELN
- LENGTH
- SETLENGTH
- LOW
- HIGH
- ORD

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