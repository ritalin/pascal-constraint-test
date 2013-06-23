unit ConsoleTestRunner;

interface

uses
	SysUtils, Classes;

type
	TConsoleTestRunner = class
	private
		FTestClass: TClass;
		FMethodNames: TStrings;

		procedure CollectTestMethods;
		procedure RunTest(test: string);
	public 
		constructor Create(testClass: TClass);
		destructor Destroy; override;

		procedure Run;
	end;

procedure RunTests(tests: array of TClass);

implementation

uses
	Should;

procedure RunTests(tests: array of TClass);
var
	i: integer;
	runner: TConsoleTestRunner;
begin
	for i := Low(tests) to High(tests) do begin
		runner := TConsoleTestRunner.Create(tests[i]);
		try
			runner.Run;
		finally
			runner.Free;
		end;
	end;
end;

constructor TConsoleTestRunner.Create(testClass: TClass);
begin
	FTestClass := testClass;
	FMethodNames := TStringList.Create;

	Self.CollectTestMethods;
end;

destructor TConsoleTestRunner.Destroy;
begin
	FMethodNames.Free;
end;

procedure TConsoleTestRunner.CollectTestMethods;
begin
	
end;

procedure TConsoleTestRunner.Run;
var
	i: integer;
	instance: TObject;
begin
	instance := FTestClass.Create;
	try
		Writeln(Format('Class %s testing... ', [FTestClass.ClassName]));
		for i := 0 to FMethodNames.Count-1 do begin
			try
				Write(Format('[%s]', [FMethodNames[i]]));

				Self.RunTest(FMethodNames[i]);

				Writeln('passed');
			except
				on ex: TTestAssertException do begin
					WriteLn('failed');
					Writeln(ex.Message);
					Writeln('');
				end;	
			end;
		end;
	finally
		instance.Free;
	end;
end;

procedure TConsoleTestRunner.RunTest(test: string);
begin
	
end;

end.