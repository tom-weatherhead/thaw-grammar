// tom-weatherhead/thaw-grammar/src/languages/smalltalk/domain-object-model/string.ts

// SmalltalkString objects are immutable.

import { ArgumentException } from '../../../common/exceptions/argument-exception';

import { ISmalltalkString, ISmalltalkValue } from './interfaces/iexpression';

import { objectClass } from './bootstrap';

import { SmalltalkCharacter } from './character';

import { SmalltalkValueBase } from './value-base';

// TODO: Is this class identical to SmalltalkSymbolValue?

export class SmalltalkString extends SmalltalkValueBase implements ISmalltalkString {
	public readonly value: string;

	constructor(value: unknown, public readonly line = 0, public readonly column = 0) {
		super(objectClass);

		// if (value == null) // 2013/12/05 : We will allow an empty string, but not a null reference.
		// {
		//     throw new ArgumentException("SmalltalkString constructor: value is null.", "value");
		// }
		//
		// Value = value;

		if (typeof value !== 'string') {
			throw new ArgumentException(
				`SmalltalkString constructor: typeof value is not 'string'; it is '${typeof value}'.`,
				'value'
			);
		}

		this.value = value as string;
	}

	public override toString(): string {
		return this.value;
	}

	// public override bool Equals(object obj)
	// {
	//
	//     if (object.ReferenceEquals(this, obj))
	//     {
	//         return true;
	//     }
	//
	//     SmalltalkString otherStringVal = obj as SmalltalkString;
	//
	//     return otherStringVal != null && Value == otherStringVal.Value;
	// }
	//
	// public override int GetHashCode()
	// {
	//     return Value.GetHashCode();
	// }

	public override getTypename(): string {
		return 'string';
	}

	public override isString(): boolean {
		return true;
	}

	// Use ISmalltalkCharacter as the return value type?

	public index(idx: ISmalltalkValue): ISmalltalkValue {
		// Array indexing starts at 1, not 0.

		const i = idx.toInteger();

		if (typeof i === 'undefined') {
			throw new ArgumentException('SmalltalkString.index() : i is undefined.', 'i');
		} else if (Number.isNaN(i) || Math.round(i) !== i) {
			throw new ArgumentException('SmalltalkString.index() : i is not an integer.', 'i');
		} else if (i <= 0 || i > this.value.length) {
			throw new ArgumentException(
				`SmalltalkString.index() : i is not in the range from 1 to ${this.value.length}.`,
				'i'
			);
		}

		return new SmalltalkCharacter(this.value[i - 1]);
	}
}
