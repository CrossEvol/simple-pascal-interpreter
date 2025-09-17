program RecordTypes;

{
  演示记录（结构体）类型
}

type
  TPerson = record
    name: String[50];
    age: Integer;
    height: Real;
  end;

var
  person1, person2: TPerson;

begin
  // 初始化记录
  person1.name := 'Alice';
  person1.age := 30;
  person1.height := 1.65;
  
  person2.name := 'Bob';
  person2.age := 25;
  person2.height := 1.80;
  
  // 输出记录内容
  writeln('Person 1:');
  writeln('  Name: ', person1.name);
  writeln('  Age: ', person1.age);
  writeln('  Height: ', person1.height:0:2);
  
  writeln('Person 2:');
  writeln('  Name: ', person2.name);
  writeln('  Age: ', person2.age);
  writeln('  Height: ', person2.height:0:2);
end.