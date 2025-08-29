program RecordExample;

type
    // Define a record type named User
    User = record
        ID: Integer; // Integer field
        Name: String[50]; // String field with a maximum length of 50 characters
        Age: Integer; // Integer field
        Salary: Real; // Real number field
        Active: Boolean; // Boolean field
        Scores: array[1..5] of Integer; // Array of integers with 5 elements
    end;

var
    // Declare a variable of type User
    exampleUser: User;
    i: Integer;

begin
    // Assign values to the fields of the User record
    exampleUser.ID := 1;
    exampleUser.Name := 'John Doe';
    exampleUser.Age := 30;
    exampleUser.Salary := 45000.50;
    exampleUser.Active := True;
    exampleUser.Scores[1] := 85;
    exampleUser.Scores[2] := 90;
    exampleUser.Scores[3] := 78;
    exampleUser.Scores[4] := 92;
    exampleUser.Scores[5] := 88;

    // Print the values of the fields
    WriteLn('User ID: ', exampleUser.ID);
    WriteLn('Name: ', exampleUser.Name);
    WriteLn('Age: ', exampleUser.Age);
    WriteLn('Salary: ', exampleUser.Salary);
    WriteLn('Active: ', exampleUser.Active);
    Write('Scores: ');
    for i := 1 to 5 do
    begin
        Write(exampleUser.Scores[i], ' ');
    end;
    WriteLn();
    WriteLn(exampleUser.Scores[-1]);
    WriteLn(exampleUser.Name[1]);
end.