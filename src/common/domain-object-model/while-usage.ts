// tom-weatherhead/thaw-grammar/src/common/domain-object-model/while-usage.ts

import { IEnvironmentFrame } from './environment-frame';
import { IExpression } from './iexpression';
import { IGlobalInfo } from './iglobal-info';

const typenameWhileUsage = 'WhileUsage';

export function isWhileUsage<T>(obj: unknown): obj is WhileUsage<T> {
	const whileUsage = obj as WhileUsage<T>;

	return typeof whileUsage !== 'undefined' && whileUsage.typename === typenameWhileUsage;
}

export class WhileUsage<T> implements IExpression<T> {
	public readonly typename: string = typenameWhileUsage;

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
