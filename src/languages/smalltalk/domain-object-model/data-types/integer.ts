// tom-weatherhead/thaw-grammar/src/languages/smalltalk/domain-object-model/integer.ts

// public interface ISmalltalkNumber
// {
// 	int ToInteger();
// 	double ToDouble();
// }

// public abstract class SmalltalkNumberBase : SmalltalkValueBase, ISmalltalkNumber
// {
// 	protected SmalltalkNumberBase(SmalltalkClass owner)
// 		: base(owner)
// 	{
// 	}
//
// 	public override bool IsNumber()
// 	{
// 		return true;
// 	}
//
// 	public abstract int ToInteger();
// 	public abstract double ToDouble();
// }

// SmalltalkInteger objects are immutable.

import { ArgumentException } from 'thaw-interpreter-core';

import { objectClass } from '../bootstrap';

import { SmalltalkValueBase } from './base';

const typenameSmalltalkInteger = 'SmalltalkInteger';

export function isSmalltalkInteger(obj: unknown): obj is SmalltalkInteger {
	const v = obj as SmalltalkInteger;

	return (
		typeof v !== 'undefined' &&
		typeof v.typename !== 'undefined' &&
		v.typename === typenameSmalltalkInteger
	);
}

export class SmalltalkInteger extends SmalltalkValueBase {
	public readonly typename: string = typenameSmalltalkInteger;
	public readonly value: number;

	constructor(value: unknown, public readonly line = 0, public readonly column = 0) {
		super(objectClass);

		if (typeof value !== 'number') {
			throw new ArgumentException(
				`SmalltalkInteger constructor: typeof value is not 'number'; it is '${typeof value}'.`,
				'value'
			);
		} else if (Number.isNaN(value)) {
			throw new ArgumentException(
				'SmalltalkInteger constructor: value is not a number (NaN).',
				'value'
			);
		} else if (Math.floor(value) !== value) {
			throw new ArgumentException(
				'SmalltalkInteger constructor: value is not an integer.',
				'value'
			);
		}

		this.value = value as number;
	}

	public override toString(): string {
		// Do not allow the output to be formatted as scientific notation.

		return `${this.value}`;
	}

	public equals(other: unknown): boolean {
		return isSmalltalkInteger(other) && other.value === this.value;
	}

	public override getTypename(): string {
		return 'int';
	}

	public override isNumber(): boolean {
		return true;
	}

	public override get isInteger(): boolean {
		return true;
	}

	public override toInteger(): number | undefined {
		return this.value;
	}

	public override toFloat(): number | undefined {
		return this.value;
	}
}
