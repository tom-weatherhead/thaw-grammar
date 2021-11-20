// tom-weatherhead/thaw-grammar/src/common/domain-object-model/while-usage.ts

import { IEnvironmentFrame } from './environment-frame';
import { IExpression } from './iexpression';
import { IGlobalInfo } from './iglobal-info';

export class WhileUsage<T> implements IExpression<T> {
	constructor(public readonly condition: IExpression<T>, public readonly body: IExpression<T>) {}

	public toString(): string {
		return `(while ${this.condition} ${this.body})`;
	}

	public evaluate(
		globalInfo: IGlobalInfo<T>,
		localEnvironment?: IEnvironmentFrame<T>,
		options?: unknown
	): T {
		while (
			!globalInfo.valueIsFalse(this.condition.evaluate(globalInfo, localEnvironment, options))
		) {
			this.body.evaluate(globalInfo, localEnvironment, options);
		}

		return globalInfo.falseValue;
	}
}
