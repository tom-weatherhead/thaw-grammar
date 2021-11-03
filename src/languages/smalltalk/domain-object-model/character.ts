// tom-weatherhead/thaw-grammar/src/languages/smalltalk/domain-object-model/character.ts

// SmalltalkCharacterValue objects are immutable.

import { objectClass } from './bootstrap';

import { SmalltalkValueBase } from './value-base';

export class SmalltalkCharacterValue extends SmalltalkValueBase /* implements ISmalltalkCharacterValue */ {
	// public readonly char Value;

	constructor(public readonly value: string) {
		super(objectClass);

		// if (value == '\0') {
		//     throw new ArgumentException("SmalltalkCharacterValue constructor: value is the null character.", "value");
		// }

		if (value.length !== 1) {
			throw new Error(
				`SmalltalkCharacterValue constructor: Length of '${this.value}' is not 1.`
			);
		}

		// Value = value;
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
	//     SmalltalkCharacterValue otherCharVal = obj as SmalltalkCharacterValue;
	//
	//     return otherCharVal != null && Value == otherCharVal.Value;
	// }
	//
	// public override int GetHashCode()
	// {
	//     return Value.GetHashCode();
	// }

	public override getTypename(): string {
		return 'char';
	}

	public override isCharacter(): boolean {
		return true;
	}
}
