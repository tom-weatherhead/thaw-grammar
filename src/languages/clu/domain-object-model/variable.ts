// clu/domain-object-model/variable.ts

import { ICLUEnvironmentFrame, ICLUGlobalInfo, ICluster, ICLUValue, ICLUVariable } from './interfaces/ivalue';

export class CLUVariable implements ICLUVariable {
	// public readonly string Name;

	constructor(public readonly name: string) {

		if (!name) {
			// throw new ArgumentNullException("name", "A CLUVariable cannot have a null or empty name");
			throw new Error('A CLUVariable cannot have a falsy name.');
		}

		// Name = name;
	}

	public toString(): string {
		return this.name;
	}

	// public override bool Equals(object obj)
	// {
	//
	// 	if (object.ReferenceEquals(this, obj))
	// 	{
	// 		return true;
	// 	}
	//
	// 	CLUVariable otherVariable = obj as CLUVariable;
	//
	// 	return otherVariable != null && Name == otherVariable.Name;
	// }
	//
	// public override int GetHashCode()
	// {
	// 	return Name.GetHashCode();
	// }

	public evaluate(localEnvironment: ICLUEnvironmentFrame, cluster: ICluster, globalInfo: ICLUGlobalInfo): ICLUValue {
		return localEnvironment.lookup(this);
	}
}
