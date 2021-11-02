// tom-weatherhead/thaw-grammar/src/languages/smalltalk/domain-object-model/operator-usage.ts

import { EvaluationException } from '../../../common/exceptions/evaluation-exception';
import { EnvironmentFrame } from '../../../common/domain-object-model/environment-frame';
import { ExpressionList } from '../../../common/domain-object-model/expression-list';
import { FunctionDefinition } from '../../../common/domain-object-model/function-definition';
import { IExpression } from '../../../common/domain-object-model/iexpression';
import { IGlobalInfo } from '../../../common/domain-object-model/iglobal-info';
import { Name } from '../../../common/domain-object-model/name';

import {
	ISmalltalkClass,
	ISmalltalkEnvironmentFrame,
	ISmalltalkExpression,
	ISmalltalkGlobalInfo,
	ISmalltalkValue
} from './interfaces/iexpression';
// import { ISmalltalkValue } from './interfaces/ivalue';

// export class SmalltalkOperatorUsage implements ISmalltalkExpression {
// 	public readonly operatorName: Name;
// 	public readonly expressionList: ExpressionList<ISmalltalkValue>;
// 	private readonly twoArgumentIntegerPredicates = new Map<
// 		string,
// 		(operand1: number, operand2: number) => boolean
// 	>();
// 	private readonly twoArgumentIntegerOperators = new Map<
// 		string,
// 		(operand1: number, operand2: number) => number
// 	>();
//
// 	constructor(operatorName: Name, expressionList: ExpressionList<ISmalltalkValue>) {
// 		this.operatorName = operatorName;
// 		this.expressionList = expressionList;
//
// 		this.twoArgumentIntegerPredicates.set(
// 			'<',
// 			(operand1: number, operand2: number) => operand1 < operand2
// 		);
// 		this.twoArgumentIntegerPredicates.set(
// 			'>',
// 			(operand1: number, operand2: number) => operand1 > operand2
// 		);
//
// 		this.twoArgumentIntegerOperators.set(
// 			'+',
// 			(operand1: number, operand2: number) => operand1 + operand2
// 		);
// 		this.twoArgumentIntegerOperators.set(
// 			'-',
// 			(operand1: number, operand2: number) => operand1 - operand2
// 		);
// 		this.twoArgumentIntegerOperators.set(
// 			'*',
// 			(operand1: number, operand2: number) => operand1 * operand2
// 		);
// 		this.twoArgumentIntegerOperators.set(
// 			'/',
// 			(operand1: number, operand2: number) => operand1 / operand2
// 		);
// 	}
//
// 	public toString(): string {
// 		if (this.expressionList.value.length === 0) {
// 			return `(${this.operatorName})`;
// 		}
//
// 		return `(${this.operatorName} ${this.expressionList})`;
// 	}
//
// 	// This is virtual because Scheme.PrimOp overrides it.
//
// 	public evaluate(
// 		localEnvironment: ISmalltalkEnvironmentFrame | undefined,
// 		receiver: ISmalltalkValue,
// 		c: ISmalltalkClass | undefined,
// 		globalInfo: ISmalltalkGlobalInfo
// 	): ISmalltalkValue {
// 		const actualNumArgs = this.expressionList.value.length;
// 		const expectedNumArgs = this.tryGetExpectedNumArgs(globalInfo);
//
// 		if (expectedNumArgs === undefined) {
// 			throw new EvaluationException(
// 				`OperatorUsage : Unknown operator name '${this.operatorName.value}`,
// 				this.operatorName.line,
// 				this.operatorName.column
// 			);
// 		} else if (expectedNumArgs >= 0 && actualNumArgs !== expectedNumArgs) {
// 			throw new EvaluationException(
// 				`OperatorUsage : Expected ${expectedNumArgs} argument(s) for operator '${this.operatorName.value}', instead of the actual ${actualNumArgs} argument(s)`,
// 				this.operatorName.line,
// 				this.operatorName.column
// 			);
// 		}
//
// 		// T macroResult;
//
// 		// if (TryInvokeMacro(expressionList.value, localEnvironment, globalInfo, out macroResult))
// 		// {
// 		// 	return macroResult;
// 		// }
//
// 		const evaluatedArguments = this.expressionList.value.map(
// 			(expr: ISmalltalkExpression) => expr.evaluate(localEnvironment, ?, ?, globalInfo)
// 		);
// 		// var argTypesErrorMessage = CheckArgTypes(evaluatedArguments);
//
// 		// if (!string.IsNullOrEmpty(argTypesErrorMessage))
// 		// {
// 		// 	throw new EvaluationException(
// 		// 		string.Format("Operator '{0}': {1}", operatorName.Value, argTypesErrorMessage),
// 		// 		operatorName.Line, operatorName.Column);
// 		// }
//
// 		return this.evaluateAux(evaluatedArguments, localEnvironment, globalInfo);
// 	}
//
// 	protected tryGetExpectedNumArgs(globalInfo: ISmalltalkGlobalInfo): number | undefined {
// 		if (['<', '>', '+', '-', '*', '/'].indexOf(this.operatorName.value) >= 0) {
// 			return 2;
// 		}
//
// 		const fnDefRaw = globalInfo.functionDefinitions.get(this.operatorName.value);
// 		// const macroDef = globalInfo.MacroDefinitions.get(this.operatorName);
//
// 		switch (this.operatorName.value) {
// 			case 'print':
// 				return -1; // Was 1. print now takes any number of arguments.
//
// 			case '=':
// 				return 2;
//
// 			default:
// 				if (fnDefRaw !== undefined) {
// 					const fnDef = fnDefRaw as FunctionDefinition<ISmalltalkValue>;
//
// 					return fnDef.argList.value.length;
// 					// } else if (globalInfo.MacroDefinitions != null && globalInfo.MacroDefinitions.ContainsKey(this.operatorName)) {
// 					// 	return globalInfo.MacroDefinitions[operatorName].ArgumentCount;
// 				} else {
// 					return undefined;
// 				}
// 		}
// 	}
//
// 	// eslint-disable-next-line @typescript-eslint/no-unused-vars
// 	protected checkArgTypes(evaluatedArguments: ISmalltalkValue[]): string | null {
// 		return null;
// 	}
//
// 	// protected virtual bool TryInvokeMacro(
// 	// 	List<IExpression<T>> unevaluatedArguments,
// 	// 	EnvironmentFrame<T> localEnvironment,
// 	// 	IGlobalInfo<T> globalInfo,
// 	// 	out T macroResult)
// 	// {
// 	// 	macroResult = default(T);
// 	// 	return false;
// 	// }
//
// 	// protected virtual void UpdateStackTrace(EnvironmentFrame<T> oldEnvFrame, EnvironmentFrame<T> newEnvFrame,
// 	// 	int line, int column)
// 	// {
// 	// }
//
// 	protected evaluateAux(
// 		evaluatedArguments: ISmalltalkValue[],
// 		localEnvironment: ISmalltalkEnvironmentFrame,
// 		globalInfo: ISmalltalkGlobalInfo
// 	): ISmalltalkValue {
// 		const firstArgAsInt =
// 			evaluatedArguments.length > 0 && globalInfo.valueIsInteger(evaluatedArguments[0])
// 				? globalInfo.valueAsInteger(evaluatedArguments[0])
// 				: 0;
// 		const secondArgAsInt =
// 			evaluatedArguments.length > 1 && globalInfo.valueIsInteger(evaluatedArguments[1])
// 				? globalInfo.valueAsInteger(evaluatedArguments[1])
// 				: 0;
//
// 		const twoArgumentIntegerPredicateRaw = this.twoArgumentIntegerPredicates.get(
// 			this.operatorName.value
// 		);
// 		const twoArgumentIntegerOperatorRaw = this.twoArgumentIntegerOperators.get(
// 			this.operatorName.value
// 		);
//
// 		// if (IntegerOperatorKeeper.TwoArgumentOperators.ContainsKey(this.operatorName.Value))
// 		if (typeof twoArgumentIntegerOperatorRaw !== 'undefined') {
// 			// return globalInfo.IntegerAsValue(IntegerOperatorKeeper.TwoArgumentOperators[operatorName.Value](firstArgAsInt, secondArgAsInt));
// 			return globalInfo.integerAsValue(
// 				(twoArgumentIntegerOperatorRaw as (operand1: number, operand2: number) => number)(
// 					firstArgAsInt,
// 					secondArgAsInt
// 				)
// 			);
// 			// } else if (IntegerOperatorKeeper.TwoArgumentPredicates.ContainsKey(this.operatorName.Value))
// 		} else if (typeof twoArgumentIntegerPredicateRaw !== 'undefined') {
// 			// return IntegerOperatorKeeper.TwoArgumentPredicates[operatorName.Value](firstArgAsInt, secondArgAsInt) ? globalInfo.TrueValue : globalInfo.FalseValue;
// 			return (
// 				twoArgumentIntegerPredicateRaw as (operand1: number, operand2: number) => boolean
// 			)(firstArgAsInt, secondArgAsInt)
// 				? globalInfo.trueValue
// 				: globalInfo.falseValue;
// 		}
//
// 		const fnDefRaw = globalInfo.functionDefinitions.get(this.operatorName.value);
//
// 		switch (this.operatorName.value) {
// 			case '=':
// 				return evaluatedArguments[0] === evaluatedArguments[1]
// 					? globalInfo.trueValue
// 					: globalInfo.falseValue;
//
// 			// case 'print':
// 			// 	globalInfo.print(evaluatedArguments);
// 			//
// 			// 	return evaluatedArguments[0];
//
// 			default:
// 				if (typeof fnDefRaw !== 'undefined') {
// 					// Evaluate a user-defined function.
// 					const newEnvironment = new EnvironmentFrame<ISmalltalkValue>(
// 						globalInfo.dynamicScoping ? localEnvironment : globalInfo.globalEnvironment
// 					);
//
// 					// if (globalInfo.Debug)
// 					// {
// 					// 	UpdateStackTrace(localEnvironment, newEnvironment, operatorName.Line, operatorName.Column);
// 					// }
//
// 					const fnDef = fnDefRaw; // as ISmalltalkFunctionDefinition;
//
// 					newEnvironment.compose(fnDef.argList, evaluatedArguments);
//
// 					return fnDef.body.evaluate(newEnvironment, ?, ?, globalInfo);
// 				}
//
// 				throw new EvaluationException(
// 					`EvaluateAux() : Unknown operator name '${this.operatorName.value}'`,
// 					this.operatorName.line,
// 					this.operatorName.column
// 				);
// 		}
// 	}
// }

export class SmalltalkOperatorUsage implements ISmalltalkExpression {
	// private readonly operatorName: Name;
	// public readonly expressionList: ISmalltalkExpression[];
	// The method reference cache, as described in Exercise 12 on page 348:
	private cachedClassReference: ISmalltalkClass | undefined;
	private cachedMethodReference: ISmalltalkFunctionDefinition | undefined;
	// private static readonly HashSet<string> OperatorsThatTakeEitherIntOrFloatArgs = new HashSet<string>() { "<", /* ">", */ "+", "-", "*", "/" };
	private readonly valueOpNames = [
		'print', '=', '<', 'exp', 'ln', 'sin', 'cos', 'tan', '+', '-', '*', '/', 'pow', 'atan2',
		'strcat', 'number?', 'symbol?', 'char?', 'string?', 'object?', 'array?', 'random', 'tostring', 'stringtosymbol',
		'floor', 'throw', 'strlen', 'typename', 'hash', 'newarray', 'arraylength',
		'string<', 'ref=', 'arrayget', 'stridx', 'substr', 'arrayset' ];

	constructor(private readonly operatorName: Name, public readonly expressionList: ISmalltalkExpression[]) {
		// OperatorName = operatorName;
		// ExpressionList = expressionList;
		// CachedClassReference = null;
		// CachedMethodReference = null;
	}

	private EvaluateNew(globalInfo: ISmalltalkGlobalInfo): ISmalltalkValue {

		if (this.expressionList.length === 0) {
			// throw new EvaluationException('EvaluateNew() : There are no arguments.', this.operatorName.line, this.operatorName.column);
			throw new Error('EvaluateNew() : There are no arguments.');
		}

		// ExpressionList[0] is a SmalltalkVariable; its name is the name of the class of which we will create an instance.
		var variable = ExpressionList[0] as SmalltalkVariable;

		if (variable == null)
		{
			throw new EvaluationException("EvaluateNew() : The first argument is not in the form of a variable.",
				OperatorName.Line, OperatorName.Column);
		}

		var className = variable.Name;

		if (!globalInfo.classDict.ContainsKey(className))
		{
			throw new EvaluationException(string.Format("EvaluateNew() : Unknown class name '{0}'.", className),
				OperatorName.Line, OperatorName.Column);
		}

		var smalltalkClass = globalInfo.classDict[className];
		var env = new SmalltalkEnvironmentFrame(null);

		foreach (var memberVariable in smalltalkClass.ClRep)
		{
			env.Dict[memberVariable] = globalInfo.ZeroValue;
		}

		var result = new SmalltalkUserValue(smalltalkClass, env);

		result.Value.Dict[SmalltalkObjectClassKeeper.SelfVar] = result;
		return result;
	}

	private ISmalltalkValue EvaluateAuxInt(List<ISmalltalkValue> evaluatedArguments, SmalltalkGlobalInfo globalInfo)
	{
		var firstArgAsInt = ((ISmalltalkNumber)evaluatedArguments[0]).ToInteger();
		var secondArgAsInt = ((ISmalltalkNumber)evaluatedArguments[1]).ToInteger();

		if (IntegerOperatorKeeper.TwoArgumentOperators.ContainsKey(OperatorName.Value))
		{
			return new SmalltalkIntegerValue(IntegerOperatorKeeper.TwoArgumentOperators[OperatorName.Value](firstArgAsInt, secondArgAsInt));
		}
		else if (IntegerOperatorKeeper.TwoArgumentPredicates.ContainsKey(OperatorName.Value))
		{
			return IntegerOperatorKeeper.TwoArgumentPredicates[OperatorName.Value](firstArgAsInt, secondArgAsInt) ? globalInfo.TrueValue : globalInfo.FalseValue;
		}

		// Internal error:
		throw new Exception(string.Format("SmalltalkOperatorUsage.EvaluateAuxInt() : Invalid operator {0}", OperatorName.Value));
	}

	private ISmalltalkValue EvaluateAuxFloat(List<ISmalltalkValue> evaluatedArguments, SmalltalkGlobalInfo globalInfo)
	{
		// Currently, EvaluateAuxFloat() is only called for two-argument functions.
		var firstArgAsDouble = ((ISmalltalkNumber)evaluatedArguments[0]).ToDouble();
		var secondArgAsDouble = ((ISmalltalkNumber)evaluatedArguments[1]).ToDouble();

		if (DoubleOperatorKeeper.TwoArgumentOperators.ContainsKey(OperatorName.Value))
		{
			return new SmalltalkFloatValue(DoubleOperatorKeeper.TwoArgumentOperators[OperatorName.Value](firstArgAsDouble, secondArgAsDouble));
		}
		else if (DoubleOperatorKeeper.TwoArgumentPredicates.ContainsKey(OperatorName.Value))
		{
			return DoubleOperatorKeeper.TwoArgumentPredicates[OperatorName.Value](firstArgAsDouble, secondArgAsDouble) ? globalInfo.TrueValue : globalInfo.FalseValue;
		}

		// Internal error:
		throw new Exception(string.Format("SmalltalkOperatorUsage.EvaluateAuxFloat() : Invalid operator {0}", OperatorName.Value));
	}

	// TODO 2014/02/04: Split EvaluateGlobalFunction() into EvaluateValueOp() and a new, much smaller EvaluateGlobalFunction() (i.e. user-defined functions).

	private ISmalltalkValue EvaluateValueOp(List<ISmalltalkValue> evaluatedArguments, SmalltalkGlobalInfo globalInfo)
	{

		for (var i = 0; i < evaluatedArguments.Count; ++i)
		{
			evaluatedArguments[i] = SmalltalkGlobalInfo.UnblockValue(evaluatedArguments[i]);
		}

		// First, verify the number of arguments.
		var expectedNumArgs = -1;
		var actualNumArgs = evaluatedArguments.Count;

		// Note: We only check DoubleOperatorKeeper here (and not IntegerOperatorKeeper) because
		// the integer operators are a subset of the double operators.

		if (DoubleOperatorKeeper.OneArgumentOperators.ContainsKey(OperatorName.Value))
		{
			expectedNumArgs = 1;
		}
		else if (DoubleOperatorKeeper.TwoArgumentOperators.ContainsKey(OperatorName.Value) ||
			DoubleOperatorKeeper.TwoArgumentPredicates.ContainsKey(OperatorName.Value))
		{
			expectedNumArgs = 2;
		}
		else
		{

			switch (OperatorName.Value)
			{
				case "strcat":
					expectedNumArgs = -1;   // Any number of arguments will do.
					break;

				case "print":
				case "number?":
				case "symbol?":
				case "char?":
				case "string?":
				case "object?":
				case "array?":
				case "random":
				case "tostring":
				case "stringtosymbol":
				case "floor":
				case "throw":
				case "strlen":
				case "typename":
				case "hash":
				case "newarray":
				case "arraylength":
					expectedNumArgs = 1;
					break;

				case "=":
				case "string<":
				case "ref=":
				case "arrayget":
				case "stridx":
					expectedNumArgs = 2;
					break;

				case "substr":
				case "arrayset":
					expectedNumArgs = 3;
					break;

				default:
					break; // Do not throw an exception; global user-defined functions pass through here.
			}
		}

		if (expectedNumArgs >= 0 && actualNumArgs != expectedNumArgs)
		{
			throw new EvaluationException(
				string.Format("EvaluateValueOp: Expected {0} arguments for operator '{1}', instead of the actual {2} arguments.",
				expectedNumArgs, OperatorName.Value, actualNumArgs), OperatorName.Line, OperatorName.Column);
		}

		// Next, check the types of the arguments.
		string exceptionMessage = null;

		switch (OperatorName.Value)
		{
			case "+":
			case "-":
			case "*":
			case "/":
			//case "<":
			//case ">":
			case "pow":
			case "atan2":

				if (!evaluatedArguments[0].IsNumber())
				{
					exceptionMessage = "First argument is not a number";
				}
				else if (!evaluatedArguments[1].IsNumber())
				{
					exceptionMessage = "Second argument is not a number";
				}

				break;

			case "<":

				if (!(evaluatedArguments[0].IsNumber() && evaluatedArguments[1].IsNumber()) &&
					!(evaluatedArguments[0].IsSymbol() && evaluatedArguments[1].IsSymbol()) &&
					!(evaluatedArguments[0].IsCharacter() && evaluatedArguments[1].IsCharacter()) &&
					!(evaluatedArguments[0].IsString() && evaluatedArguments[1].IsString()))
				{
					exceptionMessage = "Arguments must be both numbers or both symbols or both characters or both strings";
				}

				break;

			case "random": // 2014/02/01: TODO: Should we insist that random's argument be an integer?
			case "exp":
			case "ln":
			case "sin":
			case "cos":
			case "tan":
			case "floor":

				if (!evaluatedArguments[0].IsNumber())
				{
					exceptionMessage = "Argument is not a number";
				}

				break;

			case "stringtosymbol":

				if (!evaluatedArguments[0].IsString())
				{
					exceptionMessage = "Argument is not a string";
				}
				else if (string.IsNullOrEmpty(((SmalltalkStringValue)evaluatedArguments[0]).Value))
				{
					exceptionMessage = "Argument is the empty string"; // We know that it's empty and not the null .NET reference.
				}

				break;

			case "throw":
			case "strlen":

				if (!evaluatedArguments[0].IsString())
				{
					exceptionMessage = "Argument is not a string";
				}

				break;

			case "string<": // Deprecated; see <

				if (!evaluatedArguments[0].IsString())
				{
					exceptionMessage = "First argument is not a string";
				}
				else if (!evaluatedArguments[1].IsString())
				{
					exceptionMessage = "Second argument is not a string";
				}

				break;

			case "substr":

				if (!evaluatedArguments[0].IsString())
				{
					exceptionMessage = "First argument is not a string";
				}
				else if (!evaluatedArguments[1].IsNumber())
				{
					exceptionMessage = "Second argument is not a number";
				}
				else if (!evaluatedArguments[2].IsNumber())
				{
					exceptionMessage = "Third argument is not a number";
				}

				break;

			case "strcat":

				for (var i = 0; i < evaluatedArguments.Count; ++i)
				{

					if (evaluatedArguments[i].IsObject())
					{
						exceptionMessage = string.Format("Argument {0} is an object of type '{1}'",
							i + 1, evaluatedArguments[i].Owner.ClassName);
						break;
					}
				}

				break;

			case "newarray":

				if (!(evaluatedArguments[0] is SmalltalkIntegerValue))
				{
					exceptionMessage = "Argument is not an integer";
				}

				break;

			case "arraylength":

				if (!evaluatedArguments[0].IsArray())
				{
					exceptionMessage = "Argument is not an array";
				}

				break;

			case "arrayget":
			case "arrayset":

				if (!evaluatedArguments[0].IsArray())
				{
					exceptionMessage = "First argument is not an array";
				}
				else if (!(evaluatedArguments[1] is SmalltalkIntegerValue))
				{
					exceptionMessage = "Second argument is not an integer";
				}

				break;

			case "stridx":

				if (!evaluatedArguments[0].IsString())
				{
					exceptionMessage = "First argument is not a string";
				}
				else if (!evaluatedArguments[1].IsNumber())
				{
					exceptionMessage = "Second argument is not a number";
				}

				break;

			default:
				break;
		}

		if (!string.IsNullOrEmpty(exceptionMessage))
		{
			throw new EvaluationException(
				string.Format("EvaluateValueOp: Operator '{0}': {1}", OperatorName.Value, exceptionMessage),
				OperatorName.Line, OperatorName.Column);
		}

		// Now evaluate.

		try
		{

			switch (OperatorName.Value)
			{
				case "=":
					return evaluatedArguments[0].Equals(evaluatedArguments[1]) ? globalInfo.TrueValue : globalInfo.FalseValue;

				case "print":
					Console.WriteLine(evaluatedArguments[0]);
					return evaluatedArguments[0];

				case "number?":
					return evaluatedArguments[0].IsNumber() ? globalInfo.TrueValue : globalInfo.FalseValue;

				case "symbol?":
					return evaluatedArguments[0].IsSymbol() ? globalInfo.TrueValue : globalInfo.FalseValue;

				case "char?":
					return evaluatedArguments[0].IsCharacter() ? globalInfo.TrueValue : globalInfo.FalseValue;

				case "string?":
					return evaluatedArguments[0].IsString() ? globalInfo.TrueValue : globalInfo.FalseValue;

				case "object?":
					return evaluatedArguments[0].IsObject() ? globalInfo.TrueValue : globalInfo.FalseValue;

				case "array?":
					return evaluatedArguments[0].IsArray() ? globalInfo.TrueValue : globalInfo.FalseValue;

				case "random":
					return new SmalltalkIntegerValue(globalInfo.RandomNumberGenerator.Next(globalInfo.ValueAsInteger(evaluatedArguments[0])));

				case "tostring":
					return new SmalltalkStringValue(evaluatedArguments[0].ToString());

				case "stringtosymbol":
					var str2sym = (SmalltalkStringValue)evaluatedArguments[0];

					return new SmalltalkSymbolValue(str2sym.Value);

				case "floor":
					return new SmalltalkIntegerValue(((ISmalltalkNumber)evaluatedArguments[0]).ToInteger());

				case "throw":
					throw new SmalltalkException(((SmalltalkStringValue)evaluatedArguments[0]).Value, OperatorName.Line, OperatorName.Column);

				case "string<": // See page 54.  Deprecated; see <
					return ((SmalltalkStringValue)evaluatedArguments[0]).Value.CompareTo(((SmalltalkStringValue)evaluatedArguments[1]).Value) < 0
						? globalInfo.TrueValue : globalInfo.FalseValue;

				case "strlen":
					var strForLen = (SmalltalkStringValue)evaluatedArguments[0];

					return new SmalltalkIntegerValue(strForLen.Value.Length);

				case "substr":
					var strForSubstr = (SmalltalkStringValue)evaluatedArguments[0];
					var startForSubstr = globalInfo.ValueAsInteger(evaluatedArguments[1]);
					var lengthForSubstr = globalInfo.ValueAsInteger(evaluatedArguments[2]);

					return new SmalltalkStringValue(strForSubstr.Value.Substring(startForSubstr, lengthForSubstr));

				case "typename":
					return new SmalltalkStringValue(evaluatedArguments[0].GetTypename());

				case "hash":
					// ThAW 2014/01/28 : For now, we will avoid calling GetHashCode() on Smalltalk objects.
					// Of course, Smalltalk classes are free to implement their own hash functions.
					// (Would a hash function stub in the Object class prevent this global "hash" implementation from being called?)
					var hashResult = 0;

					if (!evaluatedArguments[0].IsObject())
					{
						hashResult = evaluatedArguments[0].GetHashCode();
					}

					return new SmalltalkIntegerValue(hashResult);

				case "ref=":
					return object.ReferenceEquals(evaluatedArguments[0], evaluatedArguments[1]) ? globalInfo.TrueValue : globalInfo.FalseValue;

				case "strcat": // TODO 2014/12/09 : Use string.Join() instead of a StringBuilder?
					var sb = new StringBuilder();

					foreach (var ea in evaluatedArguments)
					{
						sb.Append(ea.ToString());
					}

					return new SmalltalkStringValue(sb.ToString());

				case "newarray":
					return new SmalltalkArrayValue(((SmalltalkIntegerValue)evaluatedArguments[0]).Value);

				case "arraylength":
					return new SmalltalkIntegerValue(((SmalltalkArrayValue)evaluatedArguments[0]).Value.Length);

				case "arrayget":
					return ((SmalltalkArrayValue)evaluatedArguments[0]).GetElement(((SmalltalkIntegerValue)evaluatedArguments[1]).Value);

				case "arrayset":
					return ((SmalltalkArrayValue)evaluatedArguments[0]).SetElement(
						((SmalltalkIntegerValue)evaluatedArguments[1]).Value, evaluatedArguments[2]);

				case "stridx":
					return ((SmalltalkStringValue)evaluatedArguments[0]).Index(evaluatedArguments[1]);

				default:

					if (OperatorName.Value == "<")
					{

						if (evaluatedArguments[0].IsSymbol())
						{
							return ((SmalltalkSymbolValue)evaluatedArguments[0]).Value.CompareTo(((SmalltalkSymbolValue)evaluatedArguments[1]).Value) < 0
								? globalInfo.TrueValue : globalInfo.FalseValue;
						}
						else if (evaluatedArguments[0].IsCharacter())
						{
							return ((SmalltalkCharacterValue)evaluatedArguments[0]).Value < ((SmalltalkCharacterValue)evaluatedArguments[1]).Value
								? globalInfo.TrueValue : globalInfo.FalseValue;
						}
						else if (evaluatedArguments[0].IsString())
						{
							return ((SmalltalkStringValue)evaluatedArguments[0]).Value.CompareTo(((SmalltalkStringValue)evaluatedArguments[1]).Value) < 0
								? globalInfo.TrueValue : globalInfo.FalseValue;
						}
					}

					if (OperatorsThatTakeEitherIntOrFloatArgs.Contains(OperatorName.Value))
					{

						if (evaluatedArguments.Any(arg => arg is SmalltalkFloatValue))
						{
							return EvaluateAuxFloat(evaluatedArguments, globalInfo);
						}
						else
						{
							return EvaluateAuxInt(evaluatedArguments, globalInfo);
						}
					}
					// The next two cases involve operators that must take arguments as doubles, not ints.
					else if (DoubleOperatorKeeper.OneArgumentOperators.ContainsKey(OperatorName.Value))
					{
						return new SmalltalkFloatValue(DoubleOperatorKeeper.OneArgumentOperators[OperatorName.Value](((ISmalltalkNumber)evaluatedArguments[0]).ToDouble()));
					}
					else if (DoubleOperatorKeeper.TwoArgumentOperators.ContainsKey(OperatorName.Value))
					{
						return new SmalltalkFloatValue(DoubleOperatorKeeper.TwoArgumentOperators[OperatorName.Value](
							((ISmalltalkNumber)evaluatedArguments[0]).ToDouble(),
							((ISmalltalkNumber)evaluatedArguments[1]).ToDouble()));
					}

#if DEAD_CODE
					// Evaluate a user-defined function.

					if (!globalInfo.FunctionDefinitions.ContainsKey(OperatorName.Value))
					{
						throw new EvaluationException(
							string.Format("EvaluateGlobalFunction: Unknown function name '{0}'", OperatorName.Value),
							OperatorName.Line, OperatorName.Column);
					}

					var funDef = globalInfo.FunctionDefinitions[OperatorName.Value];
					var newEnvironment = new SmalltalkEnvironmentFrame(null);

					newEnvironment.Compose(funDef.ArgList, evaluatedArguments);
					return funDef.Body.Evaluate(newEnvironment, globalInfo.ObjectInstance, null, globalInfo);
#else
					throw new EvaluationException(
						string.Format("EvaluateValueOp: Unknown value op '{0}'", OperatorName.Value),
						OperatorName.Line, OperatorName.Column);
#endif
			}
		}
		catch (EvaluationException)
		{
			throw;
		}
		catch (Exception ex)
		{
			throw new EvaluationException(
				string.Format("EvaluateValueOp: Operator '{0}': {1}", OperatorName.Value, ex.Message),
				OperatorName.Line, OperatorName.Column);
		}
	}

	private ISmalltalkValue EvaluateGlobalFunction(List<ISmalltalkValue> evaluatedArguments, SmalltalkGlobalInfo globalInfo)
	{

		if (ValueOpNames.Contains(OperatorName.Value))
		{
			return EvaluateValueOp(evaluatedArguments, globalInfo);
		}

		try
		{
			// Evaluate a user-defined function.

			if (!globalInfo.FunctionDefinitions.ContainsKey(OperatorName.Value))
			{
				throw new EvaluationException(
					string.Format("EvaluateGlobalFunction: Unknown function name '{0}'", OperatorName.Value),
					OperatorName.Line, OperatorName.Column);
			}

			var funDef = globalInfo.FunctionDefinitions[OperatorName.Value];
			var newEnvironment = new SmalltalkEnvironmentFrame(null);

			newEnvironment.Compose(funDef.ArgList, evaluatedArguments);
			return funDef.Body.Evaluate(newEnvironment, globalInfo.ObjectInstance, null, globalInfo);
		}
		catch (EvaluationException)
		{
			throw;
		}
		catch (Exception ex)
		{
			throw new EvaluationException(
				string.Format("EvaluateGlobalFunction: Operator '{0}': {1}", OperatorName.Value, ex.Message),
				OperatorName.Line, OperatorName.Column);
		}
	}

	private ISmalltalkValue EvaluateMethod(SmalltalkFunctionDefinition method, List<ISmalltalkValue> evaluatedArguments, ISmalltalkValue receiver, SmalltalkClass c, SmalltalkGlobalInfo globalInfo)
	{
		var newEnvironment = new SmalltalkEnvironmentFrame(null /* globalInfo.GlobalEnvironment */);

		newEnvironment.Compose(method.ArgList, evaluatedArguments);
		return method.Body.Evaluate(newEnvironment, receiver, c, globalInfo);
	}

	public ISmalltalkValue Evaluate(SmalltalkEnvironmentFrame localEnvironment, ISmalltalkValue receiver, SmalltalkClass c, SmalltalkGlobalInfo globalInfo)
	{

		if (OperatorName.Value == "new")
		{
			return EvaluateNew(globalInfo);
		}

		List<ISmalltalkValue> evaluatedArguments;
		SmalltalkFunctionDefinition method;
		SmalltalkClass classInWhichMethodWasFound;

		if (ExpressionList.Count > 0)
		{
			var variable = ExpressionList[0] as SmalltalkVariable;

			if (variable != null && variable.Name == "super")
			{

				if (c == null)
				{
					throw new EvaluationException(
						string.Format("{0}: super usage: c is null", OperatorName.Value),
						OperatorName.Line, OperatorName.Column);
				}

				if (c.SuperClass == null)
				{
					throw new EvaluationException(
						string.Format("{0}: super usage: c.SuperClass is null", OperatorName.Value),
						OperatorName.Line, OperatorName.Column);
				}

				method = c.SuperClass.FindMethod(OperatorName.Value, out classInWhichMethodWasFound);

				if (method == null)
				{
					throw new EvaluationException(
						string.Format("super usage: Method '{0}' not found", OperatorName.Value),
						OperatorName.Line, OperatorName.Column);
				}

				var selfValue = SmalltalkObjectClassKeeper.SelfVar.Evaluate(localEnvironment, receiver, c, globalInfo);

				evaluatedArguments = ExpressionList.Skip(1).Select(expr => expr.Evaluate(localEnvironment, receiver, c, globalInfo)).ToList();
				return EvaluateMethod(method, evaluatedArguments, selfValue, classInWhichMethodWasFound, globalInfo);
			}
		}

#if USE_BLOCKS
		// Create blocks (suspended computations) from the expressions.
		evaluatedArguments = ExpressionList.Select(expr => (ISmalltalkValue)new SmalltalkBlock(expr, localEnvironment, receiver, c, globalInfo)).ToList();
#else
		evaluatedArguments = ExpressionList.Select(expr => expr.Evaluate(localEnvironment, receiver, c, globalInfo)).ToList();
#endif

		ISmalltalkValue result;

		if (evaluatedArguments.Count == 0)
		{
			result = EvaluateGlobalFunction(evaluatedArguments, globalInfo);
		}
		else
		{
			evaluatedArguments[0] = SmalltalkGlobalInfo.UnblockValue(evaluatedArguments[0]);

			var newReceiverClass = evaluatedArguments[0].Owner;

			if (newReceiverClass == CachedClassReference)
			{
				method = CachedMethodReference;
			}
			else
			{
				method = newReceiverClass.FindMethod(OperatorName.Value, out classInWhichMethodWasFound);
				newReceiverClass = classInWhichMethodWasFound;

				CachedClassReference = newReceiverClass;
				CachedMethodReference = method;
			}

			if (method != null)
			{
				result = EvaluateMethod(method, evaluatedArguments.Skip(1).ToList(), evaluatedArguments[0], newReceiverClass, globalInfo);
			}
			else
			{
				result = EvaluateGlobalFunction(evaluatedArguments, globalInfo);
			}
		}

		return SmalltalkGlobalInfo.UnblockValue(result);
	}
}
