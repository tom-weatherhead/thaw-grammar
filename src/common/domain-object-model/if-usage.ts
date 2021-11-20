// tom-weatherhead/thaw-grammar/src/common/domain-object-model/if-usage.ts

import { IEnvironmentFrame } from './environment-frame';
import { IExpression } from './iexpression';
import { IGlobalInfo } from './iglobal-info';

export class IfUsage<T> implements IExpression<T> {
	// public readonly condition: IExpression<T>;
	// public readonly ifBody: IExpression<T>;
	// public readonly elseBody: IExpression<T>;

	constructor(
		public readonly condition: IExpression<T>,
		public readonly ifBody: IExpression<T>,
		public readonly elseBody: IExpression<T>
	) {
		// this.condition = condition;
		// this.ifBody = ifBody;
		// this.elseBody = elseBody;
	}

	public toString(): string {
		return `(if ${this.condition} ${this.ifBody} ${this.elseBody})`;
	}

	public evaluate(
		globalInfo: IGlobalInfo<T>,
		localEnvironment?: IEnvironmentFrame<T>,
		// eslint-disable-next-line @typescript-eslint/no-unused-vars
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
