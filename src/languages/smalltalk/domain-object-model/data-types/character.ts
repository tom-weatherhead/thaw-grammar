// tom-weatherhead/thaw-grammar/src/languages/smalltalk/domain-object-model/character.ts

// SmalltalkCharacter objects are immutable.

import { ArgumentException } from '../../../../common/exceptions/argument-exception';

import { objectClass } from '../bootstrap';

import { SmalltalkValueBase } from './base';

const typenameSmalltalkCharacter = 'SmalltalkCharacter';

export function isSmalltalkCharacter(obj: unknown): obj is SmalltalkCharacter {
	const v = obj as SmalltalkCharacter;

	return (
		typeof v !== 'undefined' &&
		typeof v.typename !== 'undefined' &&
		v.typename === typenameSmalltalkCharacter
	);
}

export class SmalltalkCharacter extends SmalltalkValueBase /* implements ISmalltalkCharacter */ {
	public readonly typename: string = typenameSmalltalkCharacter;

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

	public equals(other: unknown): boolean {
		return isSmalltalkCharacter(other) && other.value === this.value;
	}

	public override getTypename(): string {
		return 'char';
	}

	public override isCharacter(): boolean {
		return true;
	}
}
