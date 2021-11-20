// clu/domain-object-model/cond-usage.ts

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
// export class CLUCondUsage implements ICLUExpression {
// 	constructor(public readonly exprPairList: [ICLUExpression, ICLUExpression][]) {}
//
// 	// public evaluate(
// 	// 	localEnvironment: ICLUEnvironmentFrame,
// 	// 	cluster: ICluster | undefined,
// 	// 	globalInfo: ICLUGlobalInfo
// 	// ): ICLUValue {
// 	public evaluate(
// 		globalInfo: IGlobalInfo<ICLUValue>,
// 		localEnvironment?: IEnvironmentFrame<ICLUValue>,
// 		// eslint-disable-next-line @typescript-eslint/no-unused-vars
// 		options?: unknown
// 	): ICLUValue {
// 		for (const [key, value] of this.exprPairList) {
// 			if (!globalInfo.valueIsFalse(key.evaluate(globalInfo, localEnvironment))) {
// 				return value.evaluate(globalInfo, localEnvironment);
// 			}
// 		}
//
// 		return globalInfo.falseValue;
// 	}
// }
