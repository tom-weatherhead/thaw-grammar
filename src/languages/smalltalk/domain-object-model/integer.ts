// tom-weatherhead/thaw-grammar/src/languages/smalltalk/domain-object-model/integer.ts

import { EnvironmentFrame } from '../../../common/domain-object-model/environment-frame';
// import { IExpression } from '../../../common/domain-object-model/iexpression';
import { IGlobalInfo } from '../../../common/domain-object-model/iglobal-info';
import { ArgumentException } from '../../../common/exceptions/argument-exception';

import { ISmalltalkExpression } from './interfaces/iexpression';
import { ISmalltalkValue } from './interfaces/ivalue';

export class SmalltalkIntegerValue implements ISmalltalkExpression, ISmalltalkValue {
	public readonly value: number;
	// public readonly line: number;
	// public readonly column: number;

	constructor(value: unknown, public readonly line = 0, public readonly column = 0) {
		if (typeof value !== 'number') {
			throw new ArgumentException(
				`SmalltalkIntegerValue constructor: typeof value is not 'number'; it is '${typeof value}'.`,
				'value'
			);
		} else if (Number.isNaN(value)) {
			throw new ArgumentException(
				'SmalltalkIntegerValue constructor: value is not a number (NaN).',
				'value'
			);
		} else if (Math.floor(value) !== value) {
			throw new ArgumentException(
				'SmalltalkIntegerValue constructor: value is not an integer.',
				'value'
			);
		}

		this.value = value as number;
		// this.line = line;
		// this.column = column;
	}

	public toString(): string {
		// Do not allow the output to be formatted as scientific notation.

		return `${this.value}`;
	}

	public get isInteger(): boolean {
		return true;
	}

	/* eslint-disable @typescript-eslint/no-unused-vars */
	public evaluate(
		localEnvironment: EnvironmentFrame<ISmalltalkValue>,
		globalInfo: IGlobalInfo<ISmalltalkValue>
	): ISmalltalkValue {
		return this;
	}
	/* eslint-enable @typescript-eslint/no-unused-vars */
}
