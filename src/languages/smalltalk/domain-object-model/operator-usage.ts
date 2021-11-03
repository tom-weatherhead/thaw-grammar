// tom-weatherhead/thaw-grammar/src/languages/smalltalk/domain-object-model/operator-usage.ts

import { Name } from 'thaw-interpreter-core';

import { EvaluationException } from '../../../common/exceptions/evaluation-exception';

import {
	ISmalltalkClass,
	ISmalltalkEnvironmentFrame,
	ISmalltalkExpression,
	ISmalltalkFunctionDefinition,
	ISmalltalkGlobalInfo,
	ISmalltalkStringValue,
	ISmalltalkValue
} from './interfaces/iexpression';

import { SmalltalkBlock, unblockValue } from './block';

import { selfVar } from './bootstrap';

import { SmalltalkEnvironmentFrame } from './environment-frame';

import { SmalltalkIntegerValue } from './integer';

import { SmalltalkUserValue } from './user-value';

import { isSmalltalkVariable } from './variable';

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
// 		// 		string.Format("Operator '{0}': {1}", operatorName.Value, argTypesErrorMessage), this.operatorName.line, this.operatorName.column);
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
	// private cachedClassReference: ISmalltalkClass | undefined;
	// private cachedMethodReference: ISmalltalkFunctionDefinition | undefined;
	// private static readonly HashSet<string> OperatorsThatTakeEitherIntOrFloatArgs = new HashSet<string>() { "<", /* ">", */ "+", "-", "*", "/" };

	private readonly valueOpNames = [
		// Arithmetic:
		'=',
		'<',
		'>',
		'+',
		'-',
		'*',
		'/',

		// Math functions:
		'pow',
		'exp',
		'ln',
		'sin',
		'cos',
		'tan',
		'atan2',
		'random',
		'floor',

		// Type predicates
		'number?',
		'symbol?',
		'char?',
		'string?',
		'object?',
		'array?',

		'typename',
		// 'hash',
		// 'ref=',

		// String functions
		'tostring',
		'stringtosymbol',
		'string<',
		'strlen',
		'stridx',
		'substr',
		'strcat',

		// Array functions
		'newarray',
		'arraylength',
		'arrayget',
		'arrayset',

		// Utility functions
		'print',
		'throw'
	];

	constructor(
		private readonly operatorName: Name,
		public readonly expressionList: ISmalltalkExpression[]
	) {
		// CachedClassReference = null;
		// CachedMethodReference = null;
	}

	private evaluateNew(globalInfo: ISmalltalkGlobalInfo): ISmalltalkValue {
		if (this.expressionList.length === 0) {
			throw new EvaluationException(
				'EvaluateNew() : There are no arguments.',
				this.operatorName.line,
				this.operatorName.column
			);
		}

		// ExpressionList[0] is a SmalltalkVariable; its name is the name of the class of which we will create an instance.
		const expr0 = this.expressionList[0]; // as SmalltalkVariable;

		// if (typeof variable === 'undefined') {
		if (!isSmalltalkVariable(expr0)) {
			// throw new EvaluationException("EvaluateNew() : The first argument is not in the form of a variable.",
			// 	OperatorName.Line, OperatorName.Column);
			throw new EvaluationException(
				'EvaluateNew() : The first argument is not in the form of a variable.',
				this.operatorName.line,
				this.operatorName.column
			);
		}

		const className = expr0.name;
		const smalltalkClass = globalInfo.classDict.get(className);

		if (typeof smalltalkClass === 'undefined') {
			throw new EvaluationException(
				`EvaluateNew() : Unknown class name '${className}'.`,
				this.operatorName.line,
				this.operatorName.column
			);
		}

		// var smalltalkClass = globalInfo.classDict[className];
		const env = new SmalltalkEnvironmentFrame();

		for (const memberVariable of smalltalkClass.clRep) {
			env.dict.set(memberVariable.name, globalInfo.zeroValue);
			// Or: env.add(memberVariable, globalInfo.zeroValue);
		}

		const result = new SmalltalkUserValue(smalltalkClass, env);

		result.value.dict.set(selfVar.name, result);
		// Or: result.value.add(selfVar, result);

		return result;
	}

	private evaluateAuxInt(
		evaluatedArguments: ISmalltalkValue[],
		// eslint-disable-next-line @typescript-eslint/no-unused-vars
		globalInfo: ISmalltalkGlobalInfo
	): ISmalltalkValue {
		const firstArgAsInt = evaluatedArguments[0].toInteger();
		const secondArgAsInt = evaluatedArguments[1].toInteger();

		if (typeof firstArgAsInt === 'undefined') {
			throw new Error('evaluateAuxInt() : firstArg is not an integer.');
		} else if (typeof secondArgAsInt === 'undefined') {
			throw new Error('evaluateAuxInt() : secondArg is not an integer.');
		}

		switch (this.operatorName.value) {
			case '+':
				return new SmalltalkIntegerValue(firstArgAsInt + secondArgAsInt);

			case '-':
				return new SmalltalkIntegerValue(firstArgAsInt - secondArgAsInt);

			case '*':
				return new SmalltalkIntegerValue(firstArgAsInt * secondArgAsInt);

			case '/':
				return new SmalltalkIntegerValue(Math.floor(firstArgAsInt / secondArgAsInt));

			case '=':
				return firstArgAsInt === secondArgAsInt
					? globalInfo.trueValue
					: globalInfo.falseValue;

			case '<':
				return firstArgAsInt < secondArgAsInt
					? globalInfo.trueValue
					: globalInfo.falseValue;

			case '>':
				return firstArgAsInt > secondArgAsInt
					? globalInfo.trueValue
					: globalInfo.falseValue;

			default:
				throw new Error(
					`evaluateAuxInt() : Unsupported operator '${this.operatorName.value}'.`
				);
		}

		// if (IntegerOperatorKeeper.TwoArgumentOperators.ContainsKey(OperatorName.Value))
		// {
		// 	return new SmalltalkIntegerValue(IntegerOperatorKeeper.TwoArgumentOperators[OperatorName.Value](firstArgAsInt, secondArgAsInt));
		// }
		// else if (IntegerOperatorKeeper.TwoArgumentPredicates.ContainsKey(OperatorName.Value))
		// {
		// 	return IntegerOperatorKeeper.TwoArgumentPredicates[OperatorName.Value](firstArgAsInt, secondArgAsInt) ? globalInfo.TrueValue : globalInfo.FalseValue;
		// }
		//
		// // Internal error:
		// throw new Exception(string.Format("SmalltalkOperatorUsage.EvaluateAuxInt() : Invalid operator {0}", OperatorName.Value));
	}

	// 	private ISmalltalkValue EvaluateAuxFloat(List<ISmalltalkValue> evaluatedArguments, SmalltalkGlobalInfo globalInfo)
	// 	{
	// 		// Currently, EvaluateAuxFloat() is only called for two-argument functions.
	// 		var firstArgAsDouble = ((ISmalltalkNumber)evaluatedArguments[0]).ToDouble();
	// 		var secondArgAsDouble = ((ISmalltalkNumber)evaluatedArguments[1]).ToDouble();
	//
	// 		if (DoubleOperatorKeeper.TwoArgumentOperators.ContainsKey(OperatorName.Value))
	// 		{
	// 			return new SmalltalkFloatValue(DoubleOperatorKeeper.TwoArgumentOperators[OperatorName.Value](firstArgAsDouble, secondArgAsDouble));
	// 		}
	// 		else if (DoubleOperatorKeeper.TwoArgumentPredicates.ContainsKey(OperatorName.Value))
	// 		{
	// 			return DoubleOperatorKeeper.TwoArgumentPredicates[OperatorName.Value](firstArgAsDouble, secondArgAsDouble) ? globalInfo.TrueValue : globalInfo.FalseValue;
	// 		}
	//
	// 		// Internal error:
	// 		throw new Exception(string.Format("SmalltalkOperatorUsage.EvaluateAuxFloat() : Invalid operator {0}", OperatorName.Value));
	// 	}
	//
	// 	// TODO 2014/02/04: Split EvaluateGlobalFunction() into EvaluateValueOp() and a new, much smaller EvaluateGlobalFunction() (i.e. user-defined functions).

	private evaluateValueOp(
		evaluatedArguments: ISmalltalkValue[],
		globalInfo: ISmalltalkGlobalInfo
	): ISmalltalkValue {
		for (let i = 0; i < evaluatedArguments.length; ++i) {
			evaluatedArguments[i] = unblockValue(evaluatedArguments[i]);
		}

		// First, verify the number of arguments.
		let expectedNumArgs = -1;
		const actualNumArgs = evaluatedArguments.length;

		// Note: We only check DoubleOperatorKeeper here (and not IntegerOperatorKeeper) because
		// the integer operators are a subset of the double operators.

		// if (DoubleOperatorKeeper.OneArgumentOperators.ContainsKey(this.operatorName.value)) {
		// 	expectedNumArgs = 1;
		// } else if (DoubleOperatorKeeper.TwoArgumentOperators.ContainsKey(this.operatorName.value) ||
		// 	DoubleOperatorKeeper.TwoArgumentPredicates.ContainsKey(this.operatorName.value)) {
		// 	expectedNumArgs = 2;
		if (['+', '-', '*', '/', '=', '<', '>'].indexOf(this.operatorName.value) >= 0) {
			expectedNumArgs = 2;
		} else {
			switch (this.operatorName.value) {
				case 'strcat':
					expectedNumArgs = -1; // Any number of arguments will do.
					break;

				case 'print':
				case 'number?':
				case 'symbol?':
				case 'char?':
				case 'string?':
				case 'object?':
				case 'array?':
				case 'random':
				case 'tostring':
				case 'stringtosymbol':
				case 'floor':
				case 'throw':
				case 'strlen':
				case 'typename':
				case 'hash':
				case 'newarray':
				case 'arraylength':
					expectedNumArgs = 1;
					break;

				case '=':
				case 'string<':
				case 'ref=':
				case 'arrayget':
				case 'stridx':
					expectedNumArgs = 2;
					break;

				case 'substr':
				case 'arrayset':
					expectedNumArgs = 3;
					break;

				default:
					break; // Do not throw an exception; global user-defined functions pass through here.
			}
		}

		if (expectedNumArgs >= 0 && actualNumArgs != expectedNumArgs) {
			throw new EvaluationException(
				`EvaluateValueOp: Expected ${expectedNumArgs} arguments for operator '${this.operatorName.value}', instead of the actual ${actualNumArgs} arguments.`,
				this.operatorName.line,
				this.operatorName.column
			);
		}

		// Next, check the types of the arguments.
		let exceptionMessage = '';

		switch (this.operatorName.value) {
			case '+':
			case '-':
			case '*':
			case '/':
			//case '<':
			//case '>':
			case 'pow':
			case 'atan2':
				if (!evaluatedArguments[0].isNumber()) {
					exceptionMessage = 'First argument is not a number';
				} else if (!evaluatedArguments[1].isNumber()) {
					exceptionMessage = 'Second argument is not a number';
				}

				break;

			case '<':
				if (
					!(evaluatedArguments[0].isNumber() && evaluatedArguments[1].isNumber()) &&
					!(evaluatedArguments[0].isSymbol() && evaluatedArguments[1].isSymbol()) &&
					!(evaluatedArguments[0].isCharacter() && evaluatedArguments[1].isCharacter()) &&
					!(evaluatedArguments[0].isString() && evaluatedArguments[1].isString())
				) {
					exceptionMessage =
						'Arguments must be both numbers or both symbols or both characters or both strings';
				}

				break;

			case 'random': // 2014/02/01: TODO: Should we insist that random's argument be an integer?
			case 'exp':
			case 'ln':
			case 'sin':
			case 'cos':
			case 'tan':
			case 'floor':
				if (!evaluatedArguments[0].isNumber()) {
					exceptionMessage = 'Argument is not a number';
				}

				break;

			case 'stringtosymbol':
				if (!evaluatedArguments[0].isString()) {
					exceptionMessage = 'Argument is not a string';
				} else if ((evaluatedArguments[0] as ISmalltalkStringValue).value === '') {
					exceptionMessage = 'Argument is the empty string'; // We know that it's empty and not the null .NET reference.
				}

				break;

			case 'throw':
			case 'strlen':
				if (!evaluatedArguments[0].isString()) {
					exceptionMessage = 'Argument is not a string';
				}

				break;

			case 'string<': // Deprecated; see <
				if (!evaluatedArguments[0].isString()) {
					exceptionMessage = 'First argument is not a string';
				} else if (!evaluatedArguments[1].isString()) {
					exceptionMessage = 'Second argument is not a string';
				}

				break;

			case 'substr':
				if (!evaluatedArguments[0].isString()) {
					exceptionMessage = 'First argument is not a string';
				} else if (!evaluatedArguments[1].isNumber()) {
					exceptionMessage = 'Second argument is not a number';
				} else if (!evaluatedArguments[2].isNumber()) {
					exceptionMessage = 'Third argument is not a number';
				}

				break;

			case 'strcat':
				for (let i = 0; i < evaluatedArguments.length; ++i) {
					if (evaluatedArguments[i].isObject()) {
						const owner = evaluatedArguments[i].owner;

						exceptionMessage = `Argument ${i + 1} is an object of type '${
							typeof owner !== 'undefined' ? owner.className : '<No owner>'
						}'`;
						break;
					}
				}

				break;

			case 'newarray':
				// if (!(evaluatedArguments[0] is SmalltalkIntegerValue))
				if (!evaluatedArguments[0].isInteger) {
					exceptionMessage = 'Argument is not an integer';
				}

				break;

			case 'arraylength':
				if (!evaluatedArguments[0].isArray()) {
					exceptionMessage = 'Argument is not an array';
				}

				break;

			case 'arrayget':
			case 'arrayset':
				if (!evaluatedArguments[0].isArray()) {
					exceptionMessage = 'First argument is not an array';
				} else if (!evaluatedArguments[1].isInteger) {
					exceptionMessage = 'Second argument is not an integer';
				}

				break;

			case 'stridx':
				if (!evaluatedArguments[0].isString()) {
					exceptionMessage = 'First argument is not a string';
				} else if (!evaluatedArguments[1].isNumber()) {
					exceptionMessage = 'Second argument is not a number';
				}

				break;

			default:
				break;
		}

		if (exceptionMessage) {
			throw new EvaluationException(
				`EvaluateValueOp: Operator '${this.operatorName.value}': ${exceptionMessage}`,
				this.operatorName.line,
				this.operatorName.column
			);
		}

		// Now evaluate.

		try {
			switch (this.operatorName.value) {
				// case '=':
				// 	return evaluatedArguments[0].Equals(evaluatedArguments[1]) ? globalInfo.trueValue : globalInfo.falseValue;

				case 'print':
					console.log(evaluatedArguments[0]);

					return evaluatedArguments[0];

				case 'number?':
					return evaluatedArguments[0].isNumber()
						? globalInfo.trueValue
						: globalInfo.falseValue;

				case 'symbol?':
					return evaluatedArguments[0].isSymbol()
						? globalInfo.trueValue
						: globalInfo.falseValue;

				case 'char?':
					return evaluatedArguments[0].isCharacter()
						? globalInfo.trueValue
						: globalInfo.falseValue;

				case 'string?':
					return evaluatedArguments[0].isString()
						? globalInfo.trueValue
						: globalInfo.falseValue;

				case 'object?':
					return evaluatedArguments[0].isObject()
						? globalInfo.trueValue
						: globalInfo.falseValue;

				case 'array?':
					return evaluatedArguments[0].isArray()
						? globalInfo.trueValue
						: globalInfo.falseValue;

				// case 'random':
				// 	return new SmalltalkIntegerValue(globalInfo.RandomNumberGenerator.Next(globalInfo.ValueAsInteger(evaluatedArguments[0])));
				//
				// case 'tostring':
				// 	return new SmalltalkStringValue(evaluatedArguments[0].ToString());
				//
				// case 'stringtosymbol':
				// 	var str2sym = (SmalltalkStringValue)evaluatedArguments[0];
				//
				// 	return new SmalltalkSymbolValue(str2sym.Value);
				//
				// case 'floor':
				// 	return new SmalltalkIntegerValue(((ISmalltalkNumber)evaluatedArguments[0]).ToInteger());
				//
				// case 'throw':
				// 	throw new SmalltalkException(((SmalltalkStringValue)evaluatedArguments[0]).Value, OperatorName.Line, OperatorName.Column);
				//
				// case 'string<': // See page 54.  Deprecated; see <
				// 	return ((SmalltalkStringValue)evaluatedArguments[0]).Value.CompareTo(((SmalltalkStringValue)evaluatedArguments[1]).Value) < 0
				// 		? globalInfo.TrueValue : globalInfo.FalseValue;
				//
				// case 'strlen':
				// 	var strForLen = (SmalltalkStringValue)evaluatedArguments[0];
				//
				// 	return new SmalltalkIntegerValue(strForLen.Value.Length);
				//
				// case 'substr':
				// 	var strForSubstr = (SmalltalkStringValue)evaluatedArguments[0];
				// 	var startForSubstr = globalInfo.ValueAsInteger(evaluatedArguments[1]);
				// 	var lengthForSubstr = globalInfo.ValueAsInteger(evaluatedArguments[2]);
				//
				// 	return new SmalltalkStringValue(strForSubstr.Value.Substring(startForSubstr, lengthForSubstr));
				//
				// case 'typename':
				// 	return new SmalltalkStringValue(evaluatedArguments[0].GetTypename());
				//
				// case 'hash':
				// 	// ThAW 2014/01/28 : For now, we will avoid calling GetHashCode() on Smalltalk objects.
				// 	// Of course, Smalltalk classes are free to implement their own hash functions.
				// 	// (Would a hash function stub in the Object class prevent this global "hash" implementation from being called?)
				// 	var hashResult = 0;
				//
				// 	if (!evaluatedArguments[0].IsObject())
				// 	{
				// 		hashResult = evaluatedArguments[0].GetHashCode();
				// 	}
				//
				// 	return new SmalltalkIntegerValue(hashResult);
				//
				// case 'ref=':
				// 	return object.ReferenceEquals(evaluatedArguments[0], evaluatedArguments[1]) ? globalInfo.TrueValue : globalInfo.FalseValue;
				//
				// case 'strcat': // TODO 2014/12/09 : Use string.Join() instead of a StringBuilder?
				// 	var sb = new StringBuilder();
				//
				// 	foreach (var ea in evaluatedArguments)
				// 	{
				// 		sb.Append(ea.ToString());
				// 	}
				//
				// 	return new SmalltalkStringValue(sb.ToString());
				//
				// case 'newarray':
				// 	return new SmalltalkArrayValue(((SmalltalkIntegerValue)evaluatedArguments[0]).Value);
				//
				// case 'arraylength':
				// 	return new SmalltalkIntegerValue(((SmalltalkArrayValue)evaluatedArguments[0]).Value.Length);
				//
				// case 'arrayget':
				// 	return ((SmalltalkArrayValue)evaluatedArguments[0]).GetElement(((SmalltalkIntegerValue)evaluatedArguments[1]).Value);
				//
				// case 'arrayset':
				// 	return ((SmalltalkArrayValue)evaluatedArguments[0]).SetElement(
				// 		((SmalltalkIntegerValue)evaluatedArguments[1]).Value, evaluatedArguments[2]);
				//
				// case 'stridx':
				// 	return ((SmalltalkStringValue)evaluatedArguments[0]).Index(evaluatedArguments[1]);

				default:
					// if (this.operatorName.value === '<') {
					//
					// 	if (evaluatedArguments[0].isSymbol()) {
					// 		return ((SmalltalkSymbolValue)evaluatedArguments[0]).value.CompareTo(((SmalltalkSymbolValue)evaluatedArguments[1]).value) < 0
					// 			? globalInfo.trueValue : globalInfo.falseValue;
					// 	}
					// 	else if (evaluatedArguments[0].isCharacter())
					// 	{
					// 		return ((SmalltalkCharacterValue)evaluatedArguments[0]).Value < ((SmalltalkCharacterValue)evaluatedArguments[1]).Value
					// 			? globalInfo.TrueValue : globalInfo.FalseValue;
					// 	}
					// 	else if (evaluatedArguments[0].isString())
					// 	{
					// 		return ((SmalltalkStringValue)evaluatedArguments[0]).Value.CompareTo(((SmalltalkStringValue)evaluatedArguments[1]).Value) < 0
					// 			? globalInfo.TrueValue : globalInfo.FalseValue;
					// 	}
					// }

					// if (OperatorsThatTakeEitherIntOrFloatArgs.Contains(this.operatorName.value))
					if (['+', '-', '*', '/', '=', '<', '>'].indexOf(this.operatorName.value) >= 0) {
						// if (evaluatedArguments.Any(arg => arg is SmalltalkFloatValue)) {
						// 	return EvaluateAuxFloat(evaluatedArguments, globalInfo);
						// } else {
						return this.evaluateAuxInt(evaluatedArguments, globalInfo);
						// }
					}
					// The next two cases involve operators that must take arguments as doubles, not ints.
					// else if (DoubleOperatorKeeper.OneArgumentOperators.ContainsKey(OperatorName.Value))
					// {
					// 	return new SmalltalkFloatValue(DoubleOperatorKeeper.OneArgumentOperators[OperatorName.Value](((ISmalltalkNumber)evaluatedArguments[0]).ToDouble()));
					// }
					// else if (DoubleOperatorKeeper.TwoArgumentOperators.ContainsKey(OperatorName.Value))
					// {
					// 	return new SmalltalkFloatValue(DoubleOperatorKeeper.TwoArgumentOperators[OperatorName.Value](
					// 		((ISmalltalkNumber)evaluatedArguments[0]).ToDouble(),
					// 		((ISmalltalkNumber)evaluatedArguments[1]).ToDouble()));
					// }

					// #if DEAD_CODE
					// // Evaluate a user-defined function.
					//
					// if (!globalInfo.FunctionDefinitions.ContainsKey(OperatorName.Value))
					// {
					// 	throw new EvaluationException(
					// 		string.Format(`EvaluateGlobalFunction: Unknown function name '{0}'`, OperatorName.Value), this.operatorName.line, this.operatorName.column);
					// }
					//
					// var funDef = globalInfo.FunctionDefinitions[OperatorName.Value];
					// var newEnvironment = new SmalltalkEnvironmentFrame(null);
					//
					// newEnvironment.Compose(funDef.ArgList, evaluatedArguments);
					// return funDef.Body.Evaluate(newEnvironment, globalInfo.ObjectInstance, null, globalInfo);
					// #else
					throw new EvaluationException(
						`EvaluateValueOp: Unknown value op '${this.operatorName.value}'`,
						this.operatorName.line,
						this.operatorName.column
					);
				// #endif
			}
		} catch (ex) {
			// catch (EvaluationException)
			// {
			// 	throw;
			// }
			throw new EvaluationException(
				`EvaluateValueOp: Operator '${this.operatorName.value}': ${ex}`,
				this.operatorName.line,
				this.operatorName.column
			);
		}
	}

	private evaluateGlobalFunction(
		evaluatedArguments: ISmalltalkValue[],
		globalInfo: ISmalltalkGlobalInfo
	): ISmalltalkValue {
		if (this.valueOpNames.indexOf(this.operatorName.value) >= 0) {
			return this.evaluateValueOp(evaluatedArguments, globalInfo);
		}

		try {
			// Evaluate a user-defined function.
			const funDef = globalInfo.functionDefinitions.get(this.operatorName.value);

			if (typeof funDef === 'undefined') {
				throw new EvaluationException(
					`EvaluateGlobalFunction: Unknown function name '${this.operatorName.value}'`,
					this.operatorName.line,
					this.operatorName.column
				);
			}

			// var funDef = globalInfo.FunctionDefinitions[OperatorName.Value];
			const newEnvironment = new SmalltalkEnvironmentFrame();

			newEnvironment.compose(funDef.argList, evaluatedArguments);

			return funDef.body.evaluate(
				newEnvironment,
				globalInfo.objectInstance,
				undefined,
				globalInfo
			);
			// } catch (error: EvaluationException) {
			// 	throw error;
		} catch (error) {
			throw new EvaluationException(
				`EvaluateGlobalFunction: Operator '${this.operatorName.value}': ${error}`,
				this.operatorName.line,
				this.operatorName.column
			);
		}
	}

	private evaluateMethod(
		method: ISmalltalkFunctionDefinition,
		evaluatedArguments: ISmalltalkValue[],
		receiver: ISmalltalkValue, // | undefined,
		c: ISmalltalkClass | undefined,
		globalInfo: ISmalltalkGlobalInfo
	): ISmalltalkValue {
		const newEnvironment = new SmalltalkEnvironmentFrame(/* globalInfo.GlobalEnvironment */);

		newEnvironment.compose(method.argList, evaluatedArguments);

		return method.body.evaluate(newEnvironment, receiver, c, globalInfo);
	}

	public evaluate(
		localEnvironment: ISmalltalkEnvironmentFrame | undefined,
		receiver: ISmalltalkValue,
		c: ISmalltalkClass | undefined,
		globalInfo: ISmalltalkGlobalInfo
	): ISmalltalkValue {
		if (this.operatorName.value === 'new') {
			return this.evaluateNew(globalInfo);
		}

		let evaluatedArguments: ISmalltalkValue[];
		// let method: ISmalltalkFunctionDefinition;
		// SmalltalkClass classInWhichMethodWasFound;

		if (this.expressionList.length > 0) {
			// const variable = this.expressionList[0] as ISmalltalkVariable;
			const expr0 = this.expressionList[0];

			// if (variable != null && variable.Name == "super") {
			if (isSmalltalkVariable(expr0) && expr0.name === 'super') {
				if (typeof c === 'undefined') {
					throw new EvaluationException(
						`${this.operatorName.value}: super usage: c is undefined`,
						this.operatorName.line,
						this.operatorName.column
					);
				}

				if (typeof c.superClass === 'undefined') {
					throw new EvaluationException(
						`${this.operatorName.value}: super usage: c.superClass is undefined`,
						this.operatorName.line,
						this.operatorName.column
					);
				}

				const { method, classInWhichMethodWasFound } = c.superClass.findMethod(
					this.operatorName.value
				);

				if (typeof method === 'undefined') {
					throw new EvaluationException(
						`${this.operatorName.value}: super usage: Method '${this.operatorName.value}' not found`,
						this.operatorName.line,
						this.operatorName.column
					);
				}

				const selfValue = selfVar.evaluate(localEnvironment, receiver, c, globalInfo);

				evaluatedArguments = this.expressionList
					.slice(1)
					.map((expr) => expr.evaluate(localEnvironment, receiver, c, globalInfo));

				return this.evaluateMethod(
					method,
					evaluatedArguments,
					selfValue,
					classInWhichMethodWasFound,
					globalInfo
				);
			}
		}

		// #if USE_BLOCKS
		// Create blocks (suspended computations) from the expressions.
		evaluatedArguments = this.expressionList.map(
			(expr) =>
				new SmalltalkBlock(
					expr,
					localEnvironment,
					receiver,
					c,
					globalInfo
				) as ISmalltalkValue
		);
		// #else
		// 		evaluatedArguments = ExpressionList.Select(expr => expr.Evaluate(localEnvironment, receiver, c, globalInfo)).ToList();
		// #endif

		let result: ISmalltalkValue;

		if (evaluatedArguments.length === 0) {
			result = this.evaluateGlobalFunction(evaluatedArguments, globalInfo);
		} else {
			evaluatedArguments[0] = unblockValue(evaluatedArguments[0]);

			let method: ISmalltalkFunctionDefinition | undefined;
			// let classInWhichMethodWasFound: ISmalltalkClass | undefined;

			let newReceiverClass = evaluatedArguments[0].owner;

			if (typeof newReceiverClass !== 'undefined') {
				// throw new Error('OperatorUsage.evaluate() : newReceiverClass is undefined');

				// if (newReceiverClass === CachedClassReference) {
				// 	method = CachedMethodReference;
				// } else {
				// { method, classInWhichMethodWasFound }
				const foo = newReceiverClass.findMethod(this.operatorName.value);

				method = foo.method;
				newReceiverClass = foo.classInWhichMethodWasFound;
			}

			// 	CachedClassReference = newReceiverClass;
			// 	CachedMethodReference = method;
			// }

			if (typeof method !== 'undefined') {
				result = this.evaluateMethod(
					method,
					evaluatedArguments.slice(1),
					evaluatedArguments[0],
					newReceiverClass,
					globalInfo
				);
			} else {
				result = this.evaluateGlobalFunction(evaluatedArguments, globalInfo);
			}
		}

		return unblockValue(result);
	}
}
