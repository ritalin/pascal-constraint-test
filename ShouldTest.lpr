program ShouldTest;

uses
	Should in 'Should.pas',
	ConsoleTestRunner in 'ConsoleTestRunner.pas',
	SampleSuite in 'SampleSuite.pas';

begin
	ConsoleTestRunner.RunTests([TSampleSuite]);
end.