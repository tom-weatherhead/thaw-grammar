// clu/domain-object-model/variable.ts

// import { IEnvironmentFrame } from '../../../common/domain-object-model/environment-frame';
//
// import { IGlobalInfo } from '../../../common/domain-object-model/iglobal-info';
//
// import {
// 	ICLUValue,
// 	ICLUVariable
// } from './interfaces/ivalue';
//
// export class CLUVariable implements ICLUVariable {
// 	constructor(
// 		public readonly name: string,
// 		public readonly line = 0,
// 		public readonly column = 0
// 	) {
// 		if (!name) {
// 			// throw new ArgumentNullException("name", "A CLUVariable cannot have a null or empty name");
// 			throw new Error('A CLUVariable cannot have a falsy name.');
// 		}
// 	}
//
// 	public toString(): string {
// 		return this.name;
// 	}
//
// 	public evaluate(
// 		globalInfo: IGlobalInfo<ICLUValue>,
// 		localEnvironment?: IEnvironmentFrame<ICLUValue>,
// 		// eslint-disable-next-line @typescript-eslint/no-unused-vars
// 		options?: unknown
// 	): ICLUValue {
// 		if (typeof localEnvironment === 'undefined') {
// 			throw new Error('CLUVariable.evaluate() : localEnvironment is undefined');
// 		}
//
// 		return localEnvironment.lookup(this);
// 	}
// }
