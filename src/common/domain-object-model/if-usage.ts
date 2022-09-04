// tom-weatherhead/thaw-grammar/src/common/domain-object-model/if-usage.ts

import { IEnvironmentFrame } from './environment-frame';
import { IExpression } from './iexpression';
import { IGlobalInfo } from './iglobal-info';

const typenameIfUsage = 'IfUsage';

export function isIfUsage<T>(obj: unknown): obj is IfUsage<T> {
	const ifUsage = obj as IfUsage<T>;

	return typeof ifUsage !== 'undefined' && ifUsage.typename === typenameIfUsage;
}

export class IfUsage<T> implements IExpression<T> {
	public readonly typename: string = typenameIfUsage;

	constructor(
		public readonly condition: IExpression<T>,
		public readonly ifBody: IExpression<T>,
		public readonly elseBody: IExpression<T>
	) {}

	public toString(): string {
		return `(if ${this.condition} ${this.ifBody} ${this.elseBody})`;
	}

	public evaluate(
		globalInfo: IGlobalInfo<T>,
		localEnvironment?: IEnvironmentFrame<T>,
		options?: unknown
	): T {
		const conditionValue = this.condition.evaluate(globalInfo, localEnvironment, options);

		if (!globalInfo.valueIsFalse(conditionValue)) {
			return this.ifBody.evaluate(globalInfo, localEnvironment, options);
		} else {
			return this.elseBody.evaluate(globalInfo, localEnvironment, options);
		}
	}
}
