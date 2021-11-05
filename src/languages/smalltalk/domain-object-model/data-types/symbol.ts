// tom-weatherhead/thaw-grammar/src/languages/smalltalk/domain-object-model/symbol.ts

// SmalltalkSymbol objects are immutable.

import { ArgumentException } from '../../../../common/exceptions/argument-exception';

import { objectClass } from '../bootstrap';

import { SmalltalkValueBase } from './base';

const typenameSmalltalkSymbol = 'SmalltalkSymbol';

export function isSmalltalkSymbol(obj: unknown): obj is SmalltalkSymbol {
	const v = obj as SmalltalkSymbol;

	return (
		typeof v !== 'undefined' &&
		typeof v.typename !== 'undefined' &&
		v.typename === typenameSmalltalkSymbol
	);
}

export class SmalltalkSymbol extends SmalltalkValueBase /* implements ISmalltalkSymbol */ {
	public readonly typename: string = typenameSmalltalkSymbol;
	public readonly value: string;

	constructor(value: unknown, public readonly line = 0, public readonly column = 0) {
		super(objectClass);

		// if (value == null) // 2013/12/05 : We will allow an empty string, but not a null reference.
		// {
		//     throw new ArgumentException("SmalltalkSymbol constructor: value is null.", "value");
		// }
		//

		if (typeof value !== 'string') {
			throw new ArgumentException(
				`SmalltalkSymbol constructor: typeof value is not 'string'; it is '${typeof value}'.`,
				'value'
			);
		} else if (!value) {
			throw new ArgumentException('SmalltalkSymbol constructor: value is falsy.', 'value');
		}

		this.value = value as string;
	}

	public override toString(): string {
		//return "#" + Value;
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
	//     var otherSymbol = obj as SmalltalkSymbol;
	//
	//     return otherSymbol != null && Value == otherSymbol.Value;
	// }

	public equals(other: unknown): boolean {
		return isSmalltalkSymbol(other) && other.value === this.value;
	}

	// public override int GetHashCode()
	// {
	//     return Value.GetHashCode();
	// }

	public override getTypename(): string {
		return 'symbol';
	}

	public override isSymbol(): boolean {
		return true;
	}
}
