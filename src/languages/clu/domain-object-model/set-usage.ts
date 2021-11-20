// clu/domain-object-model/set-usage.ts

// import { IEnvironmentFrame } from '../../../common/domain-object-model/environment-frame';
//
// import { IGlobalInfo } from '../../../common/domain-object-model/iglobal-info';
//
// import {
// 	// ICLUEnvironmentFrame,
// 	ICLUExpression,
// 	// ICLUGlobalInfo,
// 	// ICluster,
// 	ICLUValue,
// 	ICLUVariable
// } from './interfaces/ivalue';
//
// export class CLUSetUsage implements ICLUExpression {
// 	constructor(
// 		public readonly variableName: ICLUVariable,
// 		public readonly expression: ICLUExpression
// 	) {}
//
// 	/*
// 	public override string ToString()
// 	{
// 		return string.Format("(set {0} {1})", VariableName, Expression);
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
// 		if (typeof localEnvironment === 'undefined') {
// 			throw new Error('CLUSetUsage.evaluate() : localEnvironment is undefined');
// 		}
//
// 		const expressionValue = this.expression.evaluate(globalInfo, localEnvironment, options);
//
// 		// If the variable is not already defined in the local env, we may have to assign it to the global env (assuming that there are only two envs).
// 		localEnvironment.addBubbleDown(this.variableName, expressionValue); // TODO: Warning: This may be too simple and very wrong.
// 		return expressionValue;
// 	}
// }
