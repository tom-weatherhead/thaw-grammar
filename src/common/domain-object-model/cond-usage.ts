// tom-weatherhead/thaw-grammar/src/common/domain-object-model/cond-usage.ts

import { IEnvironmentFrame } from './environment-frame';
import { IExpression } from './iexpression';
import { IGlobalInfo } from './iglobal-info';

export class CondUsage<T> implements IExpression<T> {
	// public readonly exprPairList: [IExpression<T>, IExpression<T>][];

	constructor(public readonly exprPairList: [IExpression<T>, IExpression<T>][]) {
		// this.exprPairList = exprPairList;
	}

	public toString(): string {
		return `(cond ${this.exprPairList
			.map(
				([expr1, expr2]: [IExpression<T>, IExpression<T>]) =>
					'(' + expr1.toString() + ' ' + expr2.toString() + ')'
			)
			.join(' ')})`;
	}

	// public evaluate(localEnvironment: EnvironmentFrame<T>, globalInfo: IGlobalInfo<T>): T {
	public evaluate(
		globalInfo: IGlobalInfo<T>,
		localEnvironment?: IEnvironmentFrame<T>,
		// eslint-disable-next-line @typescript-eslint/no-unused-vars
		options?: unknown
	): T {
		for (const [expr1, expr2] of this.exprPairList) {
			if (!globalInfo.valueIsFalse(expr1.evaluate(globalInfo, localEnvironment))) {
				return expr2.evaluate(globalInfo, localEnvironment);
			}
		}

		return globalInfo.falseValue;
	}
}
