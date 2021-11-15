// two-part-function-name.ts

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
	constructor(public readonly clusterPart: string, f: string) {
		super(f);

		this.typename = typenameTwoPartFunctionName;
	}

	public override toString(): string {
		return `${this.clusterPart}$${this.functionPart}`;
	}
}
