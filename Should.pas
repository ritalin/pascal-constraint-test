unit Should;

interface

uses
//	Variants,
  SysUtils, Classes;

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
    function Obj(value: TObject): TTestEvaluator;
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
		procedure EvaluateAny(actual, expected: TValueContainer; negate: boolean; fieldName: string); virtual;
	public
		constructor Create(expected: TValueContainer);

		procedure Evaluate(actual: TValueContainer; negate: boolean); override;

    property Expected: TValueContainer read FExpected;
	end;

	TTestAssertException = class(EAssertionFailed);
  TTestIgnoreException = class(EAssertionFailed);

	TTypeTestAssertException = class(TTestAssertException);

	TContentTestAssertException = class(TTestAssertException);

function Its(fieldName: string): TActualValue;

// 値がBool値であるかどうか
function BeTrue: TBaseConstraint;
function BeFalse: TBaseConstraint;

// 値が等しいかどうか
function EqualToValue(expected: variant): TBaseConstraint;

// 指定された文字列で始まるかどうか
function StartsWith(expected: string): TBaseConstraint;
// 指定された文字列を含むかどうか
function IncludeText(expected: string): TBaseConstraint;

type TValueMustExistInListProc = function (const item, expected): boolean;

// 指定されたリストに指定された期待値すべて含むかどうか
function ValueMustExistInList(expected: variant; callback: TValueMustExistInListProc): TBaseConstraint;

// helper function
function ContentAsValue(value: variant): TValueContainer;

implementation

function BooleanToStr(val: boolean): string;
begin
  if val then begin
    Result := 'True';
  end
  else begin
    Result := 'False';
  end;
end;

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

function TActualValue.Obj(value: TObject): TTestEvaluator;
begin
	FData.FKind := vkObject;
	FData.FObject := value;

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

procedure TBaseConstraint.EvaluateAny(actual, expected: TValueContainer; negate: boolean; fieldName: string);
begin
  raise EInvalidOperation.Create('Unsupported');
end;

constructor TBaseConstraint.Create(expected: TValueContainer);
begin
	FExpected := expected;
end;

type
  TBooleanConstraint = class(TBaseConstraint)
	protected
		procedure EvaluateValue(actual, expected: variant; negate: boolean; fieldName: string); override;
	end;

	procedure TBooleanConstraint.EvaluateValue(actual, expected: variant; negate: boolean; fieldName: string);
	Var
		n: string;
    msg: string;
	begin
		if (actual <> expected) or negate then begin
			if negate then n:= '' else n:='not';

      msg := Format('The actual value (%s) was %s %s.'#10#9'-actual: %s'#10#9'-expected: %2:s', [
        fieldName, n, BooleanToStr(expected), BooleanToStr(actual)
			]) ;
			raise TContentTestAssertException.Create(msg);
		end;
  end;

// 値がBool値であるかどうか
function BeTrue: TBaseConstraint;
begin
  Result := TBooleanConstraint.Create(ContentAsValue(true));
end;

function BeFalse: TBaseConstraint;
begin
  Result := TBooleanConstraint.Create(ContentAsValue(false));
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

type
  TStartsWithConstraint = class(TBaseConstraint)
	protected
		procedure EvaluateValue(actual, expected: variant; negate: boolean; fieldName: string); override;
	end;

	procedure TStartsWithConstraint.EvaluateValue(actual, expected: variant; negate: boolean; fieldName: string);
	Var
		n: string;
    msg: string;
	begin
    if (AnsiPos(expected, actual) <> 1) or negate then begin
			if negate then n:= '' else n:='not';

      msg := Format('The actual value (%s) did %s start with a expected value.'#10#9'-actual: %s'#10#9'-expected: %s...', [
        fieldName, n, actual, expected
			]) ;
			raise TContentTestAssertException.Create(msg);
    end;
  end;

// 指定された文字列で始まるかどうか
function StartsWith(expected: string): TBaseConstraint;
begin
  Result := TStartsWithConstraint.Create(ContentAsValue(expected));
end;

type
  TIncludeTextConstraint = class(TBaseConstraint)
	protected
		procedure EvaluateValue(actual, expected: variant; negate: boolean; fieldName: string); override;
	end;

	procedure TIncludeTextConstraint.EvaluateValue(actual, expected: variant; negate: boolean; fieldName: string);
	Var
		n: string;
    msg: string;
	begin
    if (AnsiPos(expected, actual) = 0) or negate then begin
			if negate then n:= '' else n:='not';

      msg := Format('The actual value (%s) did %s include a expected value.'#10#9'-actual: %s'#10#9'-expected: ... %s ...', [
        fieldName, n, actual, expected
			]) ;
			raise TContentTestAssertException.Create(msg);
    end;
  end;

// 指定された文字列を含むかどうか
function IncludeText(expected: string): TBaseConstraint;
begin
  Result := TIncludeTextConstraint.Create(ContentAsValue(expected));
end;

type
  TCollectionEvaluator = class
  protected
    function GetCount: integer; virtual; abstract;

  public
    procedure InitCollection(collection: TObject); virtual; abstract;
    function EvaluateAt(const expected; index: integer; callback: TValueMustExistInListProc): boolean; virtual; abstract;

    property Count: integer read GetCount;
  end;
  TCollectionEvaluatorClass = class of TCollectionEvaluator;

  TValueMustExistInList = class(TBaseConstraint)
  private
    FEvaluator: TCollectionEvaluatorClass;
    FCallback: TValueMustExistInListProc;

	protected
    procedure EvaluateValue(actual, expected: variant; negate: boolean; fieldName: string); override;
		procedure EvaluateAny(actual, expected: TValueContainer; negate: boolean; fieldName: string); override;
  public
    constructor Create(expected: TValueContainer; evaluatorType: TCollectionEvaluatorClass; callback: TValueMustExistInListProc);

    procedure Evaluate(actual: TValueContainer; negate: boolean); override;
	end;

  constructor TValueMustExistInList.Create(expected: TValueContainer; evaluatorType: TCollectionEvaluatorClass; callback: TValueMustExistInListProc);
  begin
    inherited Create(expected);

    FEvaluator := evaluatorType;
    FCallback := callback;
  end;

  procedure TValueMustExistInList.Evaluate(actual: TValueContainer; negate: boolean);
  begin
    if actual.FKind = vkObject then begin
      Self.EvaluateAny(actual, Self.Expected, negate, actual.FFieldName);
    end
    else begin
      raise TTypeTestAssertException.Create('testing actual value type is unsupported.');
    end;
  end;

	procedure TValueMustExistInList.EvaluateValue(actual, expected: variant; negate: boolean; fieldName: string);
	begin
    raise EInvalidOperation.Create('unsuported');
  end;

  procedure TValueMustExistInList.EvaluateAny(actual, expected: TValueContainer; negate: boolean; fieldName: string);

    procedure AssertTrue(evalResult: boolean; message: string; expected: string);
    var
      n, msg: string;
    begin
      if (not evalResult) or negate then begin
        if negate then n:= '' else n:='not';

        msg := Format(message, [n, fieldName, expected]);
        raise TContentTestAssertException.Create(msg);
      end;
    end;

  var
    eval: TCollectionEvaluator;
    i: integer;
    val: string;
  begin
    eval := FEvaluator.Create;
    try
      eval.InitCollection(actual.FObject);
      for i := 0 to eval.Count-1 do begin
        case expected.FKind of
          vkValue: begin
            val := expected.FValue;

            AssertTrue(
              eval.EvaluateAt(val, i, FCallback),
              'a expected value did %s contain in the actual collection (%s).'#10#9'-expected: %s',
              expected.FValue
            );
          end;

          vkClass: begin
            AssertTrue(
              eval.EvaluateAt(expected.FClass, i, FCallback),
              'a expected class type did %s contain in the actual collection (%s).'#10#9'-expected: %s',
              expected.FClass.ClassName
            );
          end;

          vkObject: begin
            AssertTrue(
              eval.EvaluateAt(expected.FObject, i, FCallback),
              'a expected object did %s contain in the actual collection (%s).'#10#9'-expected: %s',
              Format('%p', [expected.FObject])
            );
          end;
        end;
      end;
    finally
      eval.Free;
    end;
  end;

type
  TListEvaluator = class(TCollectionEvaluator)
  private
    FList: TList;

  protected
    function GetCount: integer; override;

  public
    procedure InitCollection(collection: TObject); override;
    function EvaluateAt(const expected; index: integer; callback: TValueMustExistInListProc): boolean; override;
  end;

  procedure TListEvaluator.InitCollection(collection: TObject);
  begin
    if not (collection is TList) then begin
      raise EInvalidCast.Create('actual type is not TList');
    end;
    FList := TList(collection);
  end;

  function TListEvaluator.EvaluateAt(const expected; index: integer; callback: TValueMustExistInListProc): boolean;
  var
    actual: pointer;
  begin
    actual := FList[index];

    Result := callback(actual, expected);
  end;

  function TListEvaluator.GetCount: integer;
  begin
    Result := FList.Count;
  end;

  // 指定されたリストに指定された期待値すべて含むかどうか
function ValueMustExistInList(expected: variant; callback: TValueMustExistInListProc): TBaseConstraint;
begin
  Result := TValueMustExistInList.Create(ContentAsValue(expected), TListEvaluator, callback);
end;

end.
