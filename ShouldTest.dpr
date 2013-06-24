program ShouldTest;

uses
	Should in 'Should.pas',
	ConsoleTestRunner in 'ConsoleTestRunner.pas',
	SampleSuite in 'SampleSuite.pas';

{$APPTYPE CONSOLE}
begin
	ConsoleTestRunner.RunTests([TSampleSuite]);
end.