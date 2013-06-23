unit SampleSuite;

interface

type
	TSampleSuite = class
	published
		procedure SuccessTestA;
		procedure FailedTestA;
	end;

implementation

uses
	Should;

procedure TSampleSuite.SuccessTestA;
begin
	Its('整数').Val(100).Should(EqualTo(100));
end;

procedure TSampleSuite.FailedTestA;
begin
	Its('整数').Val(100).Should(EqualTo(10));
end;

end.

