// clu/domain-object-model/data-types/primitive-value.ts

import { ICLUEnvironmentFrame, ICLUGlobalInfo, ICluster, ICLUValue } from '../interfaces/ivalue';

export class CLUPrimitiveValue implements ICLUValue {
	// public readonly int Value;

	constructor(public readonly value: number) {
		// Value = value;
	}

	public toString(): string {
		return this.value.toString();
	}

	// public override bool Equals(object obj)
	// {
	//
	// 	if (object.ReferenceEquals(this, obj))
	// 	{
	// 		return true;
	// 	}
	//
	// 	var otherPrim = obj as CLUPrimitiveValue;
	//
	// 	return otherPrim != null && Value == otherPrim.Value;
	// }
	//
	// public override int GetHashCode()
	// {
	// 	return Value.GetHashCode();
	// }

	public evaluate(localEnvironment: ICLUEnvironmentFrame, cluster: ICluster, globalInfo: ICLUGlobalInfo): ICLUValue {
		return this;
	}
}
