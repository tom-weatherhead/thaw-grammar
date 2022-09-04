// tom-weatherhead/thaw-grammar/src/common/domain-object-model/cond-usage.ts

import { IEnvironmentFrame } from './environment-frame';
import { IExpression } from './iexpression';
import { IGlobalInfo } from './iglobal-info';

const typenameCondUsage = 'CondUsage';

export function isCondUsage<T>(obj: unknown): obj is CondUsage<T> {
	const condUsage = obj as CondUsage<T>;

	return typeof condUsage !== 'undefined' && condUsage.typename === typenameCondUsage;
}

export class CondUsage<T> implements IExpression<T> {
	public readonly typename: string = typenameCondUsage;

	constructor(public readonly exprPairList: [IExpression<T>, IExpression<T>][]) {}

	public toString(): string {
		return `(cond ${this.exprPairList
			.map(
				([expr1, expr2]: [IExpression<T>, IExpression<T>]) =>
					'(' + expr1.toString() + ' ' + expr2.toString() + ')'
			)
			.join(' ')})`;
	}

	public evaluate(
		globalInfo: IGlobalInfo<T>,
		localEnvironment?: IEnvironmentFrame<T>,
		options?: unknown
	): T {
		for (const [expr1, expr2] of this.exprPairList) {
			if (!globalInfo.valueIsFalse(expr1.evaluate(globalInfo, localEnvironment, options))) {
				return expr2.evaluate(globalInfo, localEnvironment, options);
			}
		}

		return globalInfo.falseValue;
	}
}
