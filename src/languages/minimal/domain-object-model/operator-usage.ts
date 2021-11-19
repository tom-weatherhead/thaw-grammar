// tom-weatherhead/thaw-grammar/src/languages/minimal/domain-object-model/operator-usage.ts

import { ArgumentException, EvaluationException, Name } from 'thaw-interpreter-core';

import { IEnvironmentFrame } from '../../../common/domain-object-model/environment-frame';
// import { ExpressionList } from '../../../common/domain-object-model/expression-list';
import { IExpression } from '../../../common/domain-object-model/iexpression';
import { IGlobalInfo } from '../../../common/domain-object-model/iglobal-info';

export class OperatorUsage implements IExpression<number> {
	// public readonly operatorName: Name;
	// public readonly expressionList: ExpressionList<number>;
	// public readonly twoArgumentIntegerPredicates = new Map<string, (operand1: number, operand2: number) => boolean>();
	// public readonly twoArgumentIntegerOperators = new Map<string, (operand1: number, operand2: number) => number>();

	constructor(
		public readonly operatorName: Name,
		public readonly expressionList: IExpression<number>[]
	) {
		if (operatorName.value !== '+') {
			throw new ArgumentException(
				"OperatorUsage constructor: operator name is not '+'.",
				'operatorName'
			);
		}

		// this.operatorName = operatorName;
		// this.expressionList = expressionList;

		// this.twoArgumentIntegerPredicates.set('<', (operand1: number, operand2: number) => operand1 < operand2);
		// this.twoArgumentIntegerPredicates.set('>', (operand1: number, operand2: number) => operand1 > operand2);

		// this.twoArgumentIntegerOperators.set('+', (operand1: number, operand2: number) => operand1 + operand2);
		// this.twoArgumentIntegerOperators.set('-', (operand1: number, operand2: number) => operand1 - operand2);
		// this.twoArgumentIntegerOperators.set('*', (operand1: number, operand2: number) => operand1 * operand2);
		// this.twoArgumentIntegerOperators.set('/', (operand1: number, operand2: number) => operand1 / operand2);
	}

	// public override string ToString()
	// {

	// 	if (expressionList.Value.Count == 0)
	// 	{
	// 		return string.Format("({0})", operatorName);
	// 	}

	// 	return string.Format("({0} {1})", operatorName, expressionList);
	// }

	// This is virtual because Scheme.PrimOp overrides it.

	// public evaluate(
	// 	localEnvironment: EnvironmentFrame<number>,
	// 	globalInfo: IGlobalInfo<number>
	// ): number {
	public evaluate(
		globalInfo: IGlobalInfo<number>,
		localEnvironment?: IEnvironmentFrame<number>,
		// eslint-disable-next-line @typescript-eslint/no-unused-vars
		options?: unknown
	): number {
		const actualNumArgs = this.expressionList.length;

		if (actualNumArgs !== 2) {
			throw new EvaluationException(
				`OperatorUsage : Expected two argument(s) for operator '+', instead of the actual ${actualNumArgs} argument(s)`,
				this.operatorName.line,
				this.operatorName.column
			);
		}

		const evaluatedArguments = this.expressionList.map((expr: IExpression<number>) =>
			expr.evaluate(globalInfo, localEnvironment)
		);

		if (!globalInfo.valueIsInteger(evaluatedArguments[0])) {
			throw new EvaluationException(
				`EvaluateAux() : The first argument '${evaluatedArguments[0]}' is not an integer`,
				this.operatorName.line,
				this.operatorName.column
			);
		} else if (!globalInfo.valueIsInteger(evaluatedArguments[1])) {
			throw new EvaluationException(
				`EvaluateAux() : The second argument '${evaluatedArguments[1]}' is not an integer`,
				this.operatorName.line,
				this.operatorName.column
			);
		} else {
			// return globalInfo.ValueAsInteger(evaluatedArguments[0]) + globalInfo.ValueAsInteger(evaluatedArguments[1]);

			const sum = evaluatedArguments[0] + evaluatedArguments[1];

			// console.log(`OperatorUsage.Evaluate() : Returning sum ${sum}`);

			return sum;
		}
	}
}
