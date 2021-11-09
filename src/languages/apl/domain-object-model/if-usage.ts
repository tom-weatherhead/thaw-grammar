// thaw-grammar/src/languages/apl/domain-object-model/if-usage.ts

import { IAPLValue } from './interfaces/ivalue';

import { EnvironmentFrame } from '../../../common/domain-object-model/environment-frame';

import { IExpression } from '../../../common/domain-object-model/iexpression';

import { IGlobalInfo } from '../../../common/domain-object-model/iglobal-info';

export class APLIfUsage implements IExpression<IAPLValue> {
	constructor(
		public readonly condition: IExpression<IAPLValue>,
		public readonly ifBody: IExpression<IAPLValue>,
		public readonly elseBody: IExpression<IAPLValue>
	) {}

	public toString(): string {
		return `(if ${this.condition} ${this.ifBody} ${this.elseBody})`;
	}

	public evaluate(
		localEnvironment: EnvironmentFrame<IAPLValue>,
		globalInfo: IGlobalInfo<IAPLValue>
	): IAPLValue {
		const conditionValue = !this.condition.evaluate(localEnvironment, globalInfo)
			.isFirstScalarEqualToZero;

		if (conditionValue) {
			return this.ifBody.evaluate(localEnvironment, globalInfo);
		} else {
			return this.elseBody.evaluate(localEnvironment, globalInfo);
		}
	}
}
