unit Should;

interface

uses
//	Variants,
  SysUtils;

type
	TValueKind = (vkValue, vkClass, vkObject);

	TValueContainer = record
		FFieldName: string;
		FKind: TValueKind;
		FValue: variant;
		FClass: TClass;
		FObject: TObject;
	end;

	TTestEvaluator = class;
	TActualValue = class(TObject) 
	private
		FData: TValueContainer;
	public
		function Val(value: variant): TTestEvaluator;
	end;

	TConstraint = class;
	TTestEvaluator = class
	private
		FActual: TActualValue;

	public
		procedure Should(constraint: TConstraint);
	end;

	TConstraint = class
	public
		procedure Evaluate(actual: TValueContainer; negate: boolean); virtual; abstract;
	end;

	TBaseConstraint = class(TConstraint)
	private 
		FExpected: TValueContainer;
	protected
		procedure EvaluateValue(actual, expected: variant; negate: boolean; fieldName: string); virtual; abstract;
	public
		constructor Create(expected: TValueContainer);

		procedure Evaluate(actual: TValueContainer; negate: boolean); override;
	end;

	TTestAssertException = class(Exception);
	TTypeTestAssertException = class(TTestAssertException);

	TContentTestAssertException = class(TTestAssertException);

function Its(fieldName: string): TActualValue;

function EqualToValue(expected: variant): TBaseConstraint;

// helper function
function ContentAsValue(value: variant): TValueContainer;

implementation

function ContentAsValue(value: variant): TValueContainer;
begin
	FillChar(Result, SizeOf(TValueContainer), 0);

	Result.FKind := vkValue;
	Result.FValue := value;
end;

function Its(fieldName: string): TActualValue;
begin
	Result := TActualValue.Create;
	Result.FData.FFieldName := fieldName;
end;

function TActualValue.Val(value: variant): TTestEvaluator;
begin
	FData.FKind := vkValue;
	FData.FValue := value;

	Result := TTestEvaluator.Create;
	Result.FActual := Self;
end;

procedure TTestEvaluator.Should(constraint: TConstraint);
begin
	try
		constraint.Evaluate(FActual.FData, false);
	finally
		constraint.Free;
		FActual.Free;
		Self.Free;
	end;
end;

procedure TBaseConstraint.Evaluate(actual: TValueContainer; negate: boolean);
begin
	if FExpected.FKind <> actual.FKind then begin
		raise TTypeTestAssertException.Create('testing value type is unconsistensy.');
	end;

	case FExpected.FKind of
		vkValue: Self.EvaluateValue(actual.FValue, FExpected.FValue, negate, actual.FFieldName);
	end;
end;

constructor TBaseConstraint.Create(expected: TValueContainer);
begin
	FExpected := expected;
end;

type
	TEqualToConstraint = class(TBaseConstraint)
	protected
		procedure EvaluateValue(actual, expected: variant; negate: boolean; fieldName: string); override;
	end;

	procedure TEqualToConstraint.EvaluateValue(actual, expected: variant; negate: boolean; fieldName: string);
	Var
		n: string;
                msg: string;
	begin
		if (actual <> expected) or negate then begin
			if negate then n:= '' else n:='not';

                        msg := Format('The actual value (%s) did %s equal to a expected value.'#10#9'-actual: %s'#10#9'-expected: %s', [
		            fieldName, n, actual, expected
			]) ;
			raise TContentTestAssertException.Create(msg);
		end;
	end;

function EqualToValue(expected: variant): TBaseConstraint;
begin
	Result := TEqualToConstraint.Create(ContentAsValue(expected));
end;

end.
