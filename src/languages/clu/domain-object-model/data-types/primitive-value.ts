// clu/domain-object-model/data-types/primitive-value.ts

import { ICLUEnvironmentFrame, ICLUGlobalInfo, ICluster, ICLUValue } from '../interfaces/ivalue';

const typenameCLUPrimitiveValue = 'CLUPrimitiveValue';

export function isCLUPrimitiveValue(obj: unknown): obj is CLUPrimitiveValue {
	const v = obj as CLUPrimitiveValue;

	return (
		typeof v !== 'undefined' &&
		typeof v.typename !== 'undefined' &&
		v.typename === typenameCLUPrimitiveValue
	);
}

export class CLUPrimitiveValue implements ICLUValue {
	public readonly typename: string = typenameCLUPrimitiveValue;
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

	public evaluate(
		// eslint-disable-next-line @typescript-eslint/no-unused-vars
		localEnvironment: ICLUEnvironmentFrame,
		// eslint-disable-next-line @typescript-eslint/no-unused-vars
		cluster: ICluster | undefined,
		// eslint-disable-next-line @typescript-eslint/no-unused-vars
		globalInfo: ICLUGlobalInfo
	): ICLUValue {
		return this;
	}
}
