// tom-weatherhead/thaw-grammar/src/languages/chapter1/domain-object-model/operator-usage.ts

import { EvaluationException } from 'thaw-interpreter-core';

import { IEnvironmentFrame } from '../../../common/domain-object-model/environment-frame';
import { IGlobalInfo } from '../../../common/domain-object-model/iglobal-info';
import { OperatorUsage } from '../../../common/domain-object-model/operator-usage';

export class Chapter1OperatorUsage extends OperatorUsage<number> {
	protected override tryGetExpectedNumArgs(globalInfo: IGlobalInfo<number>): number | undefined {
		switch (this.operatorName.value) {
			// case 'list':
			// 	return -1;  // Any number of arguments is permitted.

			case 'random':
			case 'throw':
				return 1;

			// case 'cons':
			// 	return 2;

			default:
				return super.tryGetExpectedNumArgs(globalInfo);
		}
	}

	// protected override evaluateAux(
	// 	evaluatedArguments: number[],
	// 	localEnvironment: EnvironmentFrame<number>,
	// 	globalInfo: IGlobalInfo<number>
	// ): number {
	protected override evaluateAux(
		evaluatedArguments: number[],
		globalInfo: IGlobalInfo<number>,
		localEnvironment?: IEnvironmentFrame<number>,
		options?: unknown
	): number {
		switch (this.operatorName.value) {
			// 2019-12-22: Hack:
			case '+':
				return evaluatedArguments.reduce(
					(accumulator, evaluatedArgument) => accumulator + evaluatedArgument,
					0
				);

			case '-':
				return evaluatedArguments[0] - evaluatedArguments[1];

			case '*':
				// return evaluatedArguments[0] * evaluatedArguments[1];

				return evaluatedArguments.reduce(
					(accumulator, evaluatedArgument) => accumulator * evaluatedArgument,
					1
				);

			case '/':
				if (evaluatedArguments[1] === 0) {
					throw new EvaluationException(
						'Division by zero error',
						this.operatorName.line,
						this.operatorName.column
					);
				}

				return Math.floor(evaluatedArguments[0] / evaluatedArguments[1]);

			case '=':
				return evaluatedArguments[0] === evaluatedArguments[1]
					? globalInfo.trueValue
					: globalInfo.falseValue;

			case '<':
				return evaluatedArguments[0] < evaluatedArguments[1]
					? globalInfo.trueValue
					: globalInfo.falseValue;

			case '>':
				return evaluatedArguments[0] > evaluatedArguments[1]
					? globalInfo.trueValue
					: globalInfo.falseValue;

			case 'print':
				globalInfo.print(evaluatedArguments);

				return evaluatedArguments[0];

			case 'random':
				return Math.floor(evaluatedArguments[0] * Math.random());

			case 'throw':
				throw new EvaluationException(
					'Exception thrown as requested',
					this.operatorName.line,
					this.operatorName.column
				);

			default:
				return super.evaluateAux(evaluatedArguments, globalInfo, localEnvironment, options); // This handles = for all types
		}
	}
}
