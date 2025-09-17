program PointerTypes;

{
  演示指针类型
}

type
  PInteger = ^Integer;
  TPerson = record
    name: String[50];
    age: Integer;
  end;
  PPerson = ^TPerson;

var
  intPtr: PInteger;
  personPtr: PPerson;
  intValue: Integer;

begin
  // 为指针分配内存
  New(intPtr);
  New(personPtr);
  
  // 通过指针访问和修改值
  intPtr^ := 100;
  personPtr^.name := 'Alice';
  personPtr^.age := 30;
  
  writeln('Integer value via pointer: ', intPtr^);
  writeln('Person name via pointer: ', personPtr^.name);
  writeln('Person age via pointer: ', personPtr^.age);
  
  // 释放内存
  Dispose(intPtr);
  Dispose(personPtr);
end.