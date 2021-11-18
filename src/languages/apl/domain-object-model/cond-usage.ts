// thaw-grammar/src/languages/apl/domain-object-model/cond-usage.ts

import { IAPLValue } from './interfaces/ivalue';

import { IEnvironmentFrame } from '../../../common/domain-object-model/environment-frame';

import { IExpression } from '../../../common/domain-object-model/iexpression';

import { IGlobalInfo } from '../../../common/domain-object-model/iglobal-info';

export class APLCondUsage implements IExpression<IAPLValue> {
	// public readonly List<KeyValuePair<IExpression<IAPLValue>, IExpression<IAPLValue>>> ExprPairList;

	constructor(public readonly exprPairList: [IExpression<IAPLValue>, IExpression<IAPLValue>][]) {
		// ExprPairList = exprPairList;
	}

	// public evaluate(
	// 	localEnvironment: EnvironmentFrame<IAPLValue>,
	// 	globalInfo: IGlobalInfo<IAPLValue>
	// ): IAPLValue {
	public evaluate(
		globalInfo: IGlobalInfo<IAPLValue>,
		localEnvironment?: IEnvironmentFrame<IAPLValue>,
		// eslint-disable-next-line @typescript-eslint/no-unused-vars
		options?: unknown
	): IAPLValue {
		for (const [key, value] of this.exprPairList) {
			if (!key.evaluate(globalInfo, localEnvironment).isFirstScalarEqualToZero) {
				return value.evaluate(globalInfo, localEnvironment);
			}
		}

		return globalInfo.falseValue;
	}
}
