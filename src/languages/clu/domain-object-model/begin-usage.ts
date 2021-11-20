// clu/domain-object-model/begin-usage.ts

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
// export class CLUBeginUsage implements ICLUExpression {
// 	constructor(
// 		public readonly firstExpression: ICLUExpression,
// 		public readonly expressionList: ICLUExpression[]
// 	) {}
//
// 	/*
// 	public override string ToString()
// 	{
// 		return string.Format("(begin {0} {1})", FirstExpression, ExpressionList);
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
// 		let result = this.firstExpression.evaluate(globalInfo, localEnvironment, options);
//
// 		for (const expression of this.expressionList) {
// 			result = expression.evaluate(globalInfo, localEnvironment, options);
// 		}
//
// 		return result;
// 	}
// }
