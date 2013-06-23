program ShouldTest;

uses
	Should in 'Should.pas',
	ConsoleTestRunner in 'ConsoleTestRunner.pas';

var
	n: integer;
begin
	ConsoleTestRunner.RunTests([]);

	n := 10;
	Its('整数の代入').Val(n).Should(EqualToValue(10));
end.