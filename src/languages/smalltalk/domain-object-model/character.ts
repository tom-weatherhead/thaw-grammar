// tom-weatherhead/thaw-grammar/src/languages/smalltalk/domain-object-model/character.ts

// SmalltalkCharacter objects are immutable.

import { ArgumentException } from '../../../common/exceptions/argument-exception';

import { objectClass } from './bootstrap';

import { SmalltalkValueBase } from './value-base';

export class SmalltalkCharacter extends SmalltalkValueBase /* implements ISmalltalkCharacter */ {
	constructor(public readonly value: string) {
		super(objectClass);

		// if (value == '\0') {
		//     throw new ArgumentException("SmalltalkCharacter constructor: value is the null character.", "value");
		// }

		if (this.value.length !== 1) {
			throw new ArgumentException(
				`SmalltalkCharacter constructor: Length of '${this.value}' is not 1.`,
				'value'
			);
		}
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
	//     SmalltalkCharacter otherCharVal = obj as SmalltalkCharacter;
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
