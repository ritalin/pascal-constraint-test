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
		procedure RunTest(instance: TObject; test: string);
	public
		constructor Create(testClass: TClass);
		destructor Destroy; override;

		procedure Run;
	end;

procedure RunTests(tests: array of TClass);

implementation

uses
	Should;

type
  TTestMethodProc = procedure of object;

procedure EvalMethod(instance: TObject; methodNames: array of string);
var
	method: TMethod;
	proc: TTestMethodProc;
  i: integer;
  m: string;
begin
  for i := Low(methodNames) to High(methodNames) do begin
    m := methodNames[i];

    method.Data := Pointer(m);
    method.Code := instance.MethodAddress(m);

    if Assigned(method.Code) then begin
      proc := TTestMethodProc(method);
      proc;

      Break;
    end;
  end;
end;

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

  Writeln('Press any key...');
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

type
  TMethodTable = packed record
    Count: smallint;
    Reserved1: array[0..5] of byte;
    Data: char;
  end;

procedure TConsoleTestRunner.CollectTestMethods;

  function IsIgnored(method: string): boolean;
  const
    IgnoreMethod: array[1..4] of string = ('Setup', 'TearDown', 'SetupOnce', 'TearDownOnce');
  var
    i: integer;
    m: string;
  begin
    Result := true;

    m := LowerCase(method);
    for i := Low(IgnoreMethod) to High(IgnoreMethod) do begin
      if LowerCase(IgnoreMethod[i]) = m then Exit;
    end;

    Result := false;
  end;

var
  ref: TClass;
  table: ^TMethodTable;
  i: integer;
  buf: ^ShortString;
begin
  ref := FTestClass;
  asm
    mov EAX, [ref]
    mov EAX,[EAX].vmtMethodTable
    mov [table], EAX
  end;

  buf := @table.Data;
  for i := 1 to table.Count do begin
    if not IsIgnored(buf^) then begin
      FMethodNames.Add(buf^);
    end;

    buf := Pointer(PChar(buf) + (Length(buf^)+1)+6);
  end;
end;

procedure TConsoleTestRunner.Run;
var
	i: integer;
	instance: TObject;
begin
	instance := FTestClass.Create;
	try
    EvalMethod(instance, ['SetupOnce', 'BeforeOnce']);

		Writeln(Format('Class %s testing... ', [FTestClass.ClassName]));
		for i := 0 to FMethodNames.Count-1 do begin
			try
				Write(Format('[%s]: ', [FMethodNames[i]]));

				Self.RunTest(instance, FMethodNames[i]);

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
    EvalMethod(instance, ['TearDownOnce', 'AfterOnce']);
		instance.Free;
	end;
end;

procedure TConsoleTestRunner.RunTest(instance: TObject; test: string);
begin
  EvalMethod(instance, ['Setup', 'Before']);
  try
    EvalMethod(instance, [test]);
  finally
    EvalMethod(instance, ['Teardown', 'After']);
  end;
end;

end.
