// tom-weatherhead/thaw-grammar/src/languages/chapter1/domain-object-model/integer-literal.ts

import { ArgumentException } from 'thaw-interpreter-core';

import { IEnvironmentFrame } from '../../../common/domain-object-model/environment-frame';
import { IExpression } from '../../../common/domain-object-model/iexpression';
import { IGlobalInfo } from '../../../common/domain-object-model/iglobal-info';

export class IntegerLiteral implements IExpression<number> {
	public readonly value: number;

	constructor(value: unknown, public readonly line = 0, public readonly column = 0) {
		if (typeof value !== 'number') {
			throw new ArgumentException(
				`IntegerLiteral constructor: typeof value is not 'number'; it is '${typeof value}'.`,
				'value'
			);
		} else if (Number.isNaN(value)) {
			throw new ArgumentException(
				'IntegerLiteral constructor: value is not a number (NaN).',
				'value'
			);
		} else if (Math.floor(value) !== value) {
			throw new ArgumentException(
				'IntegerLiteral constructor: value is not an integer.',
				'value'
			);
		}

		this.value = value as number;
	}

	public toString(): string {
		// Do not allow the output to be formatted as scientific notation.

		return `${this.value}`;
	}

	/* eslint-disable @typescript-eslint/no-unused-vars */
	public evaluate(
		globalInfo: IGlobalInfo<number>,
		localEnvironment?: IEnvironmentFrame<number>,
		options?: unknown
	): number {
		return this.value;
	}
	/* eslint-enable @typescript-eslint/no-unused-vars */
}
