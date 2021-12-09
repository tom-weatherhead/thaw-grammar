// tom-weatherhead/thaw-grammar/src/languages/arithmetic/domain-object-model/number.ts

import { ArgumentException } from 'thaw-interpreter-core';

import { IEnvironmentFrame } from '../../../common/domain-object-model/environment-frame';
import { IExpression } from '../../../common/domain-object-model/iexpression';
import { IGlobalInfo } from '../../../common/domain-object-model/iglobal-info';

export class IntegerLiteral implements IExpression<number> {
	// Actually, it can be any real numeric value, not just an integer.
	public readonly value: number;

	constructor(value: unknown) {
		if (typeof value !== 'number') {
			throw new ArgumentException(
				'IntegerLiteral constructor: value is not a number.',
				'value'
			);
			// } else if (Math.floor(value) !== value) {
			// 	throw new ArgumentException(
			// 		'IntegerLiteral constructor: value is not an integer.',
			// 		'value'
			// 	);
		}

		this.value = value as number;
	}

	public toString(): string {
		return `${this.value}`;
	}

	// public override bool Equals(object obj)
	// {
	// 	IntegerLiteral otherIntLit = obj as IntegerLiteral;

	// 	return otherIntLit != null && Value == otherIntLit.Value;
	// }

	/* eslint-disable @typescript-eslint/no-unused-vars */
	public evaluate(
		globalInfo: IGlobalInfo<number>,
		localEnvironment?: IEnvironmentFrame<number>,
		// eslint-disable-next-line @typescript-eslint/no-unused-vars
		options?: unknown
	): number {
		return this.value;
	}
	/* eslint-enable @typescript-eslint/no-unused-vars */
}
