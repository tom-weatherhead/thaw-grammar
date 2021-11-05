// tom-weatherhead/thaw-grammar/src/languages/smalltalk/domain-object-model/array.ts

// SmalltalkArray objects are mutable.

import { ArgumentException } from '../../../common/exceptions/argument-exception';

import { ISmalltalkArray, ISmalltalkValue } from './interfaces/iexpression';

import { objectClass } from './bootstrap';

import { defaultValue } from './object-instance';

import { SmalltalkValueBase } from './value-base';

const typenameSmalltalkArray = 'SmalltalkArray';

export function isSmalltalkArray(obj: unknown): obj is SmalltalkArray {
	const v = obj as SmalltalkArray;

	return (
		typeof v !== 'undefined' &&
		typeof v.typename !== 'undefined' &&
		v.typename === typenameSmalltalkArray
	);
}

export class SmalltalkArray extends SmalltalkValueBase implements ISmalltalkArray {
	public readonly typename: string = typenameSmalltalkArray;
	public readonly value: ISmalltalkValue[]; // = [];

	constructor(size: number) {
		super(objectClass);

		if (size < 0) {
			throw new ArgumentException('SmalltalkStringValue constructor: size < 0', 'size');
		}

		// for (let i = 0; i < size; ++i) {
		// 	this.value.push(defaultValue);
		// }

		this.value = new Array(size).fill(defaultValue);
	}

	public override toString(): string {
		// return string.Join(" ", (IEnumerable<ISmalltalkValue>)Value);

		return '<array>';
	}

	// public override bool Equals(object obj)
	// {
	//     return object.ReferenceEquals(this, obj);
	// }

	// // Uncomment this method once ISmalltalkValue extends IEqualityComparable from common-utils.

	// eslint-disable-next-line @typescript-eslint/no-unused-vars
	public equals(other: unknown): boolean {
		// return isSmalltalkArray(other) && other.value.length === this.value.length && other.value.every((element: ISmalltalkValue, i: number) => element.equals(this.value[i]));

		return (
			isSmalltalkArray(other) &&
			other.value.length === this.value.length &&
			other.value.every((element, i) => element.equals(this.value[i]))
		);

		// return false;
	}

	// public override int GetHashCode()
	// {
	//     return 0;
	// }

	public override getTypename(): string {
		return 'array';
	}

	public override isArray(): boolean {
		return true;
	}

	public getElement(i: number): ISmalltalkValue {
		// Array indexing starts at 1, not 0.

		if (Number.isNaN(i) || Math.round(i) !== i) {
			throw new ArgumentException('SmalltalkArray.getElement() : i is not an integer.', 'i');
		} else if (i <= 0 || i > this.value.length) {
			throw new ArgumentException(
				`SmalltalkArray.getElement() : i is not in the range from 1 to ${this.value.length}.`,
				'i'
			);
		}

		return this.value[i - 1];
	}

	public setElement(i: number, elementValue: ISmalltalkValue): ISmalltalkValue {
		// Indexing starts at 1

		if (Number.isNaN(i) || Math.round(i) !== i) {
			throw new ArgumentException('SmalltalkArray.setElement() : i is not an integer.', 'i');
		} else if (i <= 0 || i > this.value.length) {
			throw new ArgumentException(
				`SmalltalkArray.setElement() : i is not in the range from 1 to ${this.value.length}.`,
				'i'
			);
		}

		this.value[i - 1] = elementValue;

		return elementValue;
	}
}
