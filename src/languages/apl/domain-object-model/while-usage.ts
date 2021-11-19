// thaw-grammar/src/languages/apl/domain-object-model/while-usage.ts

// import { IAPLValue } from './interfaces/ivalue';
//
// import { IEnvironmentFrame } from '../../../common/domain-object-model/environment-frame';
//
// import { IExpression } from '../../../common/domain-object-model/iexpression';
//
// import { IGlobalInfo } from '../../../common/domain-object-model/iglobal-info';
//
// export class APLWhileUsage implements IExpression<IAPLValue> {
// 	constructor(
// 		public readonly condition: IExpression<IAPLValue>,
// 		public readonly body: IExpression<IAPLValue>
// 	) {}
//
// 	public toString(): string {
// 		return `(while ${this.condition} ${this.body})`;
// 	}
//
// 	public evaluate(
// 		globalInfo: IGlobalInfo<IAPLValue>,
// 		localEnvironment?: IEnvironmentFrame<IAPLValue>,
// 		// eslint-disable-next-line @typescript-eslint/no-unused-vars
// 		options?: unknown
// 	): IAPLValue {
// 		while (!this.condition.evaluate(globalInfo, localEnvironment).isFirstScalarEqualToZero) {
// 			this.body.evaluate(globalInfo, localEnvironment);
// 		}
//
// 		return globalInfo.falseValue;
// 	}
// }
