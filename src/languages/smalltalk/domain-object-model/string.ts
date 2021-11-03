// tom-weatherhead/thaw-grammar/src/languages/smalltalk/domain-object-model/string.ts

// SmalltalkStringValue objects are immutable.

import { objectClass } from './bootstrap';

import { SmalltalkValueBase } from './value-base';

// TODO: Is this class identical to SmalltalkSymbolValue?

export class SmalltalkStringValue extends SmalltalkValueBase /* implements ISmalltalkStringValue */ {
	constructor(public readonly value: string) {
		super(objectClass);

		// if (value == null) // 2013/12/05 : We will allow an empty string, but not a null reference.
		// {
		//     throw new ArgumentException("SmalltalkStringValue constructor: value is null.", "value");
		// }
		//
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
	//     SmalltalkStringValue otherStringVal = obj as SmalltalkStringValue;
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

	// public ISmalltalkValue Index(ISmalltalkValue idx)
	// {
	//     var i = ((ISmalltalkNumber)idx).ToInteger();
	//
	//     if (i <= 0 || i > Value.Length)
	//     {
	//         throw new Exception(string.Format("SmalltalkStringValue.Index(): Index {0} is not in the range from 1 to {1}",
	//             i, Value.Length));
	//     }
	//
	//     return new SmalltalkCharacterValue(Value[i - 1]);
	// }
}
