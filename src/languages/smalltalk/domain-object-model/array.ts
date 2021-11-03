// tom-weatherhead/thaw-grammar/src/languages/smalltalk/domain-object-model/array.ts

// SmalltalkArrayValue objects are mutable.

import { ISmalltalkValue } from './interfaces/iexpression';

import { objectClass, zeroValue } from './bootstrap';

import { SmalltalkValueBase } from './value-base';

export class SmalltalkArrayValue extends SmalltalkValueBase /* implements ISmalltalkArrayValue */ {
	public readonly value: ISmalltalkValue[] = [];

	constructor(size: number) {
		super(objectClass);

		if (size < 0) {
			// throw new ArgumentException("SmalltalkStringValue constructor: size < 0", "size");
			throw new Error('SmalltalkStringValue constructor: size < 0');
		}

		// Value = new ISmalltalkValue[size];

		// const zero = new SmalltalkIntegerValue(0);

		for (let i = 0; i < size; ++i) {
			this.value.push(zeroValue);
		}
	}

	// public override string ToString()
	// {
	//     return string.Join(" ", (IEnumerable<ISmalltalkValue>)Value);
	// }
	//
	// public override bool Equals(object obj)
	// {
	//     return object.ReferenceEquals(this, obj);
	// }
	//
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

	// public ISmalltalkValue GetElement(int i) // Indexing starts at 1
	// {
	//
	//     if (i <= 0 || i > Value.Length)
	//     {
	//         throw new ArgumentException(
	//             string.Format("SmalltalkArrayValue.GetElement() : i is not in the range from 1 to {0}", Value.Length),
	//             "i");
	//     }
	//
	//     return Value[i - 1];
	// }
	//
	// public ISmalltalkValue SetElement(int i, ISmalltalkValue elementValue) // Indexing starts at 1
	// {
	//
	//     if (i <= 0 || i > Value.Length)
	//     {
	//         throw new ArgumentException(
	//             string.Format("SmalltalkArrayValue.GetElement() : i is not in the range from 1 to {0}", Value.Length),
	//             "i");
	//     }
	//
	//     Value[i - 1] = elementValue;
	//     return elementValue;
	// }
}
