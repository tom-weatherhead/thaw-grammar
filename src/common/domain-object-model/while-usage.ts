// tom-weatherhead/thaw-grammar/src/common/domain-object-model/while-usage.ts

import { IEnvironmentFrame } from './environment-frame';
import { IExpression } from './iexpression';
import { IGlobalInfo } from './iglobal-info';

export class WhileUsage<T> implements IExpression<T> {
	public readonly condition: IExpression<T>;
	public readonly body: IExpression<T>;

	constructor(condition: IExpression<T>, body: IExpression<T>) {
		this.condition = condition;
		this.body = body;
	}

	public toString(): string {
		return `(if ${this.condition} ${this.body})`;
	}

	// public evaluate(localEnvironment: EnvironmentFrame<T>, globalInfo: IGlobalInfo<T>): T {
	public evaluate(
		globalInfo: IGlobalInfo<T>,
		localEnvironment?: IEnvironmentFrame<T>,
		// eslint-disable-next-line @typescript-eslint/no-unused-vars
		options?: unknown
	): T {
		while (!globalInfo.valueIsFalse(this.condition.evaluate(globalInfo, localEnvironment))) {
			this.body.evaluate(globalInfo, localEnvironment);
		}

		return globalInfo.falseValue;
	}
}
