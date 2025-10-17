program TimerTest;

uses
  SysUtils;

var
  StartTime, EndTime: Integer;
  ElapsedTime: Integer;

begin
  WriteLn('========== Pascal 时间测试程序 ==========');
  WriteLn();
  
  { 记录开始时间（单位：毫秒）}
  StartTime := GetTickCount();
  WriteLn('开始时间: ', StartTime, ' ms');
  WriteLn();
  
  { 模拟程序运行 - Sleep延迟3000毫秒（3秒）}
  WriteLn('正在运行中...');
  {  Sleep(1000);}
  WriteLn('继续运行...');
  
  { 记录结束时间 }
  EndTime := GetTickCount();
  WriteLn();
  WriteLn('结束时间: ', EndTime, ' ms');
  WriteLn();
  
  { 计算耗时 }
  ElapsedTime := EndTime - StartTime;
  
  WriteLn('========== 执行结果 ==========');
  WriteLn('总耗时: ', ElapsedTime, ' 毫秒');
  WriteLn('总耗时: ', ElapsedTime div 1000, ' 秒');
  WriteLn();
  
end.