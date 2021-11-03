// tom-weatherhead/thaw-grammar/src/languages/smalltalk/domain-object-model/symbol.ts

// SmalltalkSymbolValue objects are immutable.

import { objectClass } from './bootstrap';

import { SmalltalkValueBase } from './value-base';

export class SmalltalkSymbolValue extends SmalltalkValueBase /* implements ISmalltalkSymbolValue */ {
	constructor(public readonly value: string) {
		super(objectClass);

		// if (string.IsNullOrEmpty(value)) {
		if (!this.value) {
			// throw new ArgumentException("SmalltalkSymbolValue constructor: value is null or empty.", "value");
			throw new Error('SmalltalkSymbolValue constructor: value is falsy.');
		}

		// Value = value;
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
	//     var otherSymbol = obj as SmalltalkSymbolValue;
	//
	//     return otherSymbol != null && Value == otherSymbol.Value;
	// }
	//
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
