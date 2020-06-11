// tom-weatherhead/thaw-grammar/src/common/domain-object-model/cond-usage.ts

'use strict';

import { EnvironmentFrame } from './environment-frame';
import { IExpression } from './iexpression';
import { IGlobalInfo } from './iglobal-info';

export class CondUsage<T> implements IExpression<T> {
	public readonly exprPairList: Array<[IExpression<T>, IExpression<T>]>;

	constructor(exprPairList: Array<[IExpression<T>, IExpression<T>]>) {
		this.exprPairList = exprPairList;
	}

	public toString(): string {
		const fnExprPairAsString = ([expr1, expr2]: [
			IExpression<T>,
			IExpression<T>
		]) => `(${expr1} ${expr2})`;

		return `(cond ${this.exprPairList
			.map(
				([expr1, expr2]: [IExpression<T>, IExpression<T>]) =>
					'(' + expr1.toString() + ' ' + expr2.toString() + ')'
			)
			.join(' ')})`;
	}

	public evaluate(
		localEnvironment: EnvironmentFrame<T>,
		globalInfo: IGlobalInfo<T>
	): T {
		for (const [expr1, expr2] of this.exprPairList) {
			if (
				!globalInfo.valueIsFalse(
					expr1.evaluate(localEnvironment, globalInfo)
				)
			) {
				return expr2.evaluate(localEnvironment, globalInfo);
			}
		}

		return globalInfo.falseValue;
	}
}
