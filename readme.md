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