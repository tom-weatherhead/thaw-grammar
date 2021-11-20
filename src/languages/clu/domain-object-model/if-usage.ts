// clu/domain-object-model/if-usage.ts

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
// export class CLUIfUsage implements ICLUExpression {
// 	constructor(
// 		public readonly condition: ICLUExpression,
// 		public readonly ifBody: ICLUExpression,
// 		public readonly elseBody: ICLUExpression
// 	) {}
//
// 	/*
// 	public override string ToString()
// 	{
// 		return string.Format("(if {0} {1} {2})", Condition, IfBody, ElseBody);
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
// 		const conditionValue = this.condition.evaluate(globalInfo, localEnvironment, options);
//
// 		if (!globalInfo.valueIsFalse(conditionValue)) {
// 			return this.ifBody.evaluate(globalInfo, localEnvironment, options);
// 		} else {
// 			return this.elseBody.evaluate(globalInfo, localEnvironment, options);
// 		}
// 	}
// }
