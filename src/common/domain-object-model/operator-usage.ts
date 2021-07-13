// tom-weatherhead/thaw-grammar/src/common/domain-object-model/operator-usage.ts

import { EvaluationException } from '../exceptions/evaluation-exception';
import { EnvironmentFrame } from './environment-frame';
import { ExpressionList } from './expression-list';
import { FunctionDefinition } from './function-definition';
import { IExpression } from './iexpression';
import { IGlobalInfo } from './iglobal-info';
import { Name } from './name';

export class OperatorUsage<T> implements IExpression<T> {
	public readonly operatorName: Name;
	public readonly expressionList: ExpressionList<T>;
	private readonly twoArgumentIntegerPredicates = new Map<
		string,
		(operand1: number, operand2: number) => boolean
	>();
	private readonly twoArgumentIntegerOperators = new Map<
		string,
		(operand1: number, operand2: number) => number
	>();

	constructor(operatorName: Name, expressionList: ExpressionList<T>) {
		this.operatorName = operatorName;
		this.expressionList = expressionList;

		this.twoArgumentIntegerPredicates.set(
			'<',
			(operand1: number, operand2: number) => operand1 < operand2
		);
		this.twoArgumentIntegerPredicates.set(
			'>',
			(operand1: number, operand2: number) => operand1 > operand2
		);

		this.twoArgumentIntegerOperators.set(
			'+',
			(operand1: number, operand2: number) => operand1 + operand2
		);
		this.twoArgumentIntegerOperators.set(
			'-',
			(operand1: number, operand2: number) => operand1 - operand2
		);
		this.twoArgumentIntegerOperators.set(
			'*',
			(operand1: number, operand2: number) => operand1 * operand2
		);
		this.twoArgumentIntegerOperators.set(
			'/',
			(operand1: number, operand2: number) => operand1 / operand2
		);
	}

	public toString(): string {
		if (this.expressionList.value.length === 0) {
			return `(${this.operatorName})`;
		}

		return `(${this.operatorName} ${this.expressionList})`;
	}

	// This is virtual because Scheme.PrimOp overrides it.

	public evaluate(localEnvironment: EnvironmentFrame<T>, globalInfo: IGlobalInfo<T>): T {
		const actualNumArgs = this.expressionList.value.length;
		const expectedNumArgs = this.tryGetExpectedNumArgs(globalInfo);

		if (expectedNumArgs === undefined) {
			throw new EvaluationException(
				`OperatorUsage : Unknown operator name '${this.operatorName.value}`,
				this.operatorName.line,
				this.operatorName.column
			);
		} else if (expectedNumArgs >= 0 && actualNumArgs !== expectedNumArgs) {
			throw new EvaluationException(
				`OperatorUsage : Expected ${expectedNumArgs} argument(s) for operator '${this.operatorName.value}', instead of the actual ${actualNumArgs} argument(s)`,
				this.operatorName.line,
				this.operatorName.column
			);
		}

		// T macroResult;

		// if (TryInvokeMacro(expressionList.value, localEnvironment, globalInfo, out macroResult))
		// {
		// 	return macroResult;
		// }

		const evaluatedArguments = this.expressionList.value.map((expr: IExpression<T>) =>
			expr.evaluate(localEnvironment, globalInfo)
		);
		// var argTypesErrorMessage = CheckArgTypes(evaluatedArguments);

		// if (!string.IsNullOrEmpty(argTypesErrorMessage))
		// {
		// 	throw new EvaluationException(
		// 		string.Format("Operator '{0}': {1}", operatorName.Value, argTypesErrorMessage),
		// 		operatorName.Line, operatorName.Column);
		// }

		return this.evaluateAux(evaluatedArguments, localEnvironment, globalInfo);
	}

	protected tryGetExpectedNumArgs(globalInfo: IGlobalInfo<T>): number | undefined {
		if (['<', '>', '+', '-', '*', '/'].indexOf(this.operatorName.value) >= 0) {
			return 2;
		}

		const fnDefRaw = globalInfo.functionDefinitions.get(this.operatorName.value);
		// const macroDef = globalInfo.MacroDefinitions.get(this.operatorName);

		switch (this.operatorName.value) {
			case 'print':
				return -1; // Was 1. print now takes any number of arguments.

			case '=':
				return 2;

			default:
				if (fnDefRaw !== undefined) {
					const fnDef = fnDefRaw as FunctionDefinition<T>;

					return fnDef.argList.value.length;
					// } else if (globalInfo.MacroDefinitions != null && globalInfo.MacroDefinitions.ContainsKey(this.operatorName)) {
					// 	return globalInfo.MacroDefinitions[operatorName].ArgumentCount;
				} else {
					return undefined;
				}
		}
	}

	// eslint-disable-next-line @typescript-eslint/no-unused-vars
	protected checkArgTypes(evaluatedArguments: T[]): string | null {
		return null;
	}

	// protected virtual bool TryInvokeMacro(
	// 	List<IExpression<T>> unevaluatedArguments,
	// 	EnvironmentFrame<T> localEnvironment,
	// 	IGlobalInfo<T> globalInfo,
	// 	out T macroResult)
	// {
	// 	macroResult = default(T);
	// 	return false;
	// }

	// protected virtual void UpdateStackTrace(EnvironmentFrame<T> oldEnvFrame, EnvironmentFrame<T> newEnvFrame,
	// 	int line, int column)
	// {
	// }

	protected evaluateAux(
		evaluatedArguments: T[],
		localEnvironment: EnvironmentFrame<T>,
		globalInfo: IGlobalInfo<T>
	): T {
		const firstArgAsInt =
			evaluatedArguments.length > 0 && globalInfo.valueIsInteger(evaluatedArguments[0])
				? globalInfo.valueAsInteger(evaluatedArguments[0])
				: 0;
		const secondArgAsInt =
			evaluatedArguments.length > 1 && globalInfo.valueIsInteger(evaluatedArguments[1])
				? globalInfo.valueAsInteger(evaluatedArguments[1])
				: 0;

		const twoArgumentIntegerPredicateRaw = this.twoArgumentIntegerPredicates.get(
			this.operatorName.value
		);
		const twoArgumentIntegerOperatorRaw = this.twoArgumentIntegerOperators.get(
			this.operatorName.value
		);

		// if (IntegerOperatorKeeper.TwoArgumentOperators.ContainsKey(this.operatorName.Value))
		if (typeof twoArgumentIntegerOperatorRaw !== 'undefined') {
			// return globalInfo.IntegerAsValue(IntegerOperatorKeeper.TwoArgumentOperators[operatorName.Value](firstArgAsInt, secondArgAsInt));
			return globalInfo.integerAsValue(
				(twoArgumentIntegerOperatorRaw as (operand1: number, operand2: number) => number)(
					firstArgAsInt,
					secondArgAsInt
				)
			);
			// } else if (IntegerOperatorKeeper.TwoArgumentPredicates.ContainsKey(this.operatorName.Value))
		} else if (typeof twoArgumentIntegerPredicateRaw !== 'undefined') {
			// return IntegerOperatorKeeper.TwoArgumentPredicates[operatorName.Value](firstArgAsInt, secondArgAsInt) ? globalInfo.TrueValue : globalInfo.FalseValue;
			return (
				twoArgumentIntegerPredicateRaw as (operand1: number, operand2: number) => boolean
			)(firstArgAsInt, secondArgAsInt)
				? globalInfo.trueValue
				: globalInfo.falseValue;
		}

		const fnDefRaw = globalInfo.functionDefinitions.get(this.operatorName.value);

		switch (this.operatorName.value) {
			case '=':
				return evaluatedArguments[0] === evaluatedArguments[1]
					? globalInfo.trueValue
					: globalInfo.falseValue;

			case 'print':
				globalInfo.print(evaluatedArguments);

				return evaluatedArguments[0];

			default:
				if (typeof fnDefRaw !== 'undefined') {
					// Evaluate a user-defined function.
					const newEnvironment = new EnvironmentFrame<T>(
						globalInfo.dynamicScoping ? localEnvironment : globalInfo.globalEnvironment
					);

					// if (globalInfo.Debug)
					// {
					// 	UpdateStackTrace(localEnvironment, newEnvironment, operatorName.Line, operatorName.Column);
					// }

					const fnDef = fnDefRaw as FunctionDefinition<T>;

					newEnvironment.compose(fnDef.argList.value, evaluatedArguments);

					return fnDef.body.evaluate(newEnvironment, globalInfo);
				}

				throw new EvaluationException(
					`EvaluateAux() : Unknown operator name '${this.operatorName.value}'`,
					this.operatorName.line,
					this.operatorName.column
				);
		}
	}
}
