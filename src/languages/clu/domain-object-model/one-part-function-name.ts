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

	constructor(public readonly functionPart: string) {}

	public toString(): string {
		return this.functionPart;
	}
}
