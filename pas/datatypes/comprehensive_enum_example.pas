program ComprehensiveEnumExample;

{
  Comprehensive example showing all enum features
}

type
  TColor = (Red, Green, Blue, Yellow, Purple);
  TDay = (Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday);
  TDirection = (North, East, South, West);

var
  backgroundColor: TColor;
  today: TDay;
  direction: TDirection;

begin
  { Enum assignment }
  backgroundColor := Green;
  today := Wednesday;
  direction := North;
  
  { Output enum values }
  writeln('Background color: ', backgroundColor); {Background color: Green}
  writeln('Today is: ', today); {Today is: Wednesday}
  writeln('Direction: ', direction); {Direction: North}
  
  { Using Ord function to get enum ordinal }
  writeln('Ordinal of Green: ', Ord(backgroundColor)); {Ordinal of Green: 1}
  writeln('Ordinal of Wednesday: ', Ord(today)); {Ordinal of Wednesday: 2}
  writeln('Ordinal of North: ', Ord(direction)); {Ordinal of North: 0}
  
  { Enum comparisons }
  if backgroundColor = Green then
    writeln('Background color is Green'); {Background color is Green}
    
  if today > Monday then
    writeln('Today is after Monday'); {Today is after Monday}
    
  if direction < East then
    writeln('Direction is before East'); {Direction is before East}
  
  { Enum for loop iteration }
  writeln('All colors:');
  for backgroundColor := Red to Purple do
    writeln('  Color: ', backgroundColor);
    
  writeln('Weekdays:');
  for today := Monday to Friday do
    writeln('  Day: ', today);
    
  { Case statement with enums }
  case today of
    Monday: writeln('Start of the work week');
    Wednesday: writeln('Midweek day');
    Friday: writeln('End of the work week');
    Saturday, Sunday: writeln('Weekend!');
  else
    writeln('Regular weekday');
  end;
end.