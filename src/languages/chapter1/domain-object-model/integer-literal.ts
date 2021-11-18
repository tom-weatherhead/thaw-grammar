// tom-weatherhead/thaw-grammar/src/languages/chapter1/domain-object-model/integer-literal.ts

import { ArgumentException } from 'thaw-interpreter-core';

import { IEnvironmentFrame } from '../../../common/domain-object-model/environment-frame';
import { IExpression } from '../../../common/domain-object-model/iexpression';
import { IGlobalInfo } from '../../../common/domain-object-model/iglobal-info';

export class IntegerLiteral implements IExpression<number> {
	public readonly value: number;
	public readonly line: number;
	public readonly column: number;

	constructor(value: unknown, line = 0, column = 0) {
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
		this.line = line;
		this.column = column;
	}

	public toString(): string {
		// Do not allow the output to be formatted as scientific notation.

		return `${this.value}`;
	}

	/* eslint-disable @typescript-eslint/no-unused-vars */
	// public evaluate(
	// 	localEnvironment: EnvironmentFrame<number>,
	// 	globalInfo: IGlobalInfo<number>
	// ): number {
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
