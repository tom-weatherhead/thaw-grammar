// clu/domain-object-model/while-usage.ts

// import { IEnvironmentFrame } from '../../../common/domain-object-model/environment-frame';
//
// import { IGlobalInfo } from '../../../common/domain-object-model/iglobal-info';
//
// import {
// 	// ICLUEnvironmentFrame,
// 	ICLUExpression,
// 	// ICLUGlobalInfo,
// 	// ICluster,
// 	ICLUValue
// } from './interfaces/ivalue';
//
// export class CLUWhileUsage implements ICLUExpression {
// 	constructor(public readonly condition: ICLUExpression, public readonly body: ICLUExpression) {}
//
// 	/*
// 	public override string ToString()
// 	{
// 		return string.Format("(while {0} {1})", Condition, Body);
// 	}
// 	 */
//
// 	// public evaluate(
// 	// 	localEnvironment: ICLUEnvironmentFrame,
// 	// 	cluster: ICluster | undefined,
// 	// 	globalInfo: ICLUGlobalInfo
// 	// ): ICLUValue {
// 	public evaluate(
// 		globalInfo: IGlobalInfo<ICLUValue>,
// 		localEnvironment?: IEnvironmentFrame<ICLUValue>,
// 		options?: unknown
// 	): ICLUValue {
// 		while (
// 			!globalInfo.valueIsFalse(this.condition.evaluate(globalInfo, localEnvironment, options))
// 		) {
// 			this.body.evaluate(globalInfo, localEnvironment, options);
// 		}
//
// 		return globalInfo.falseValue;
// 	}
// }
