// tom-weatherhead/thaw-grammar/src/languages/smalltalk/domain-object-model/float.ts

import { ArgumentException } from '../../../common/exceptions/argument-exception';

import { objectClass } from './bootstrap';

import { SmalltalkValueBase } from './value-base';

export class SmalltalkFloat extends SmalltalkValueBase {
	public readonly value: number;

	constructor(value: unknown, public readonly line = 0, public readonly column = 0) {
		super(objectClass);

		if (typeof value !== 'number') {
			throw new ArgumentException(
				`SmalltalkFloat constructor: typeof value is not 'number'; it is '${typeof value}'.`,
				'value'
			);
		} else if (Number.isNaN(value)) {
			throw new ArgumentException(
				'SmalltalkFloat constructor: value is not a number (NaN).',
				'value'
			);
		}

		this.value = value as number;
	}

	public override toString(): string {
		// Do not allow the output to be formatted as scientific notation.

		return `${this.value}`;
	}

	public override getTypename(): string {
		return 'float';
	}

	public override isNumber(): boolean {
		return true;
	}

	public override toFloat(): number | undefined {
		return this.value;
	}
}
