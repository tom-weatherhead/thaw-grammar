// clu/domain-object-model/variable.ts

import {
	ICLUEnvironmentFrame,
	ICLUGlobalInfo,
	ICluster,
	ICLUValue,
	ICLUVariable
} from './interfaces/ivalue';

export class CLUVariable implements ICLUVariable {
	constructor(public readonly name: string) {
		if (!name) {
			// throw new ArgumentNullException("name", "A CLUVariable cannot have a null or empty name");
			throw new Error('A CLUVariable cannot have a falsy name.');
		}
	}

	public toString(): string {
		return this.name;
	}

	public evaluate(
		localEnvironment: ICLUEnvironmentFrame,
		// eslint-disable-next-line @typescript-eslint/no-unused-vars
		cluster: ICluster | undefined,
		// eslint-disable-next-line @typescript-eslint/no-unused-vars
		globalInfo: ICLUGlobalInfo
	): ICLUValue {
		return localEnvironment.lookup(this);
	}
}
