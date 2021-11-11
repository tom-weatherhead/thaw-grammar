// one-part-function-name.ts

import {
	// ICLUEnvironmentFrame,
	// ICLUExpression,
	ICLUFunctionName // ,
	// ICLUGlobalInfo,
	// ICluster,
	// ICLUValue
} from './interfaces/ivalue';

const typenameOnePartFunctionName = 'OnePartFunctionName';

export function isOnePartFunctionName(obj: unknown): obj is OnePartFunctionName {
	const v = obj as OnePartFunctionName;

	return (
		typeof v !== 'undefined' &&
		typeof v.typename !== 'undefined' &&
		v.typename === typenameOnePartFunctionName
	);
}

export class OnePartFunctionName implements ICLUFunctionName {
	public typename: string = typenameOnePartFunctionName;
	// public readonly string FunctionPart;

	constructor(public readonly functionPart: string) {
		// FunctionPart = f;
	}

	public toString(): string {
		return this.functionPart;
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
	// 	var otherOnePartFunName = (OnePartFunctionName)obj;
	//
	// 	return FunctionPart == otherOnePartFunName.FunctionPart;
	// }
	//
	// public override int GetHashCode()
	// {
	// 	return FunctionPart.GetHashCode();
	// }
}
