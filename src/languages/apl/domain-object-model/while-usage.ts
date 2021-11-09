// thaw-grammar/src/languages/apl/domain-object-model/while-usage.ts

import { IAPLValue } from './interfaces/ivalue';

import { EnvironmentFrame } from '../../../common/domain-object-model/environment-frame';

import { IExpression } from '../../../common/domain-object-model/iexpression';

import { IGlobalInfo } from '../../../common/domain-object-model/iglobal-info';

export class APLWhileUsage implements IExpression<IAPLValue> {
	constructor(
		public readonly condition: IExpression<IAPLValue>,
		public readonly body: IExpression<IAPLValue>
	) {}

	public toString(): string {
		return `(while ${this.condition} ${this.body})`;
	}

	public evaluate(
		localEnvironment: EnvironmentFrame<IAPLValue>,
		globalInfo: IGlobalInfo<IAPLValue>
	): IAPLValue {
		while (!this.condition.evaluate(localEnvironment, globalInfo).isFirstScalarEqualToZero) {
			this.body.evaluate(localEnvironment, globalInfo);
		}

		return globalInfo.falseValue;
	}
}
