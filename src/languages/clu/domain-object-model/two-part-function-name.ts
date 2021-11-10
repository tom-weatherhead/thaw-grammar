// two-part-function-name.ts

// import { ICLUEnvironmentFrame, ICLUExpression, ICLUFunctionName, ICLUGlobalInfo, ICluster, ICLUValue } from './interfaces/ivalue';

import { OnePartFunctionName } from './one-part-function-name';

const typenameTwoPartFunctionName = 'TwoPartFunctionName';

export function isTwoPartFunctionName(obj: unknown): obj is TwoPartFunctionName {
	const v = obj as TwoPartFunctionName;

	return (
		typeof v !== 'undefined' &&
		typeof v.typename !== 'undefined' &&
		v.typename === typenameTwoPartFunctionName
	);
}

export class TwoPartFunctionName extends OnePartFunctionName {
	// public readonly string ClusterPart;

	constructor(public readonly clusterPart: string, f: string) {
		super(f);
		// ClusterPart = c;
		this.typename = typenameTwoPartFunctionName;
	}

	public override toString(): string {
		// return string.Format("{0}${1}", ClusterPart, FunctionPart);

		return `${this.clusterPart}$${this.functionPart}`;
	}

	// public override bool Equals(object obj)
	// {
	//
	// 	if (object.ReferenceEquals(this, obj))
	// 	{
	// 		return true;
	// 	}
	//
	// 	if (obj == null || !GetType().Equals(obj.GetType()))
	// 	{
	// 		return false;
	// 	}
	//
	// 	var otherTwoPartFunName = (TwoPartFunctionName)obj;
	//
	// 	return ClusterPart == otherTwoPartFunName.ClusterPart && FunctionPart == otherTwoPartFunName.FunctionPart;
	// }
	//
	// public override int GetHashCode()
	// {
	// 	return ClusterPart.GetHashCode() * 101 + FunctionPart.GetHashCode();
	// }
}
