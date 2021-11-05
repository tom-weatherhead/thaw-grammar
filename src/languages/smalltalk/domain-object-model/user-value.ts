// tom-weatherhead/thaw-grammar/src/languages/smalltalk/domain-object-model/user-value.ts

import {
	ISmalltalkClass,
	ISmalltalkEnvironmentFrame,
	ISmalltalkUserValue
} from './interfaces/iexpression';

import { SmalltalkValueBase } from './value-base';

const typenameSmalltalkUserValue = 'SmalltalkUserValue';

export function isSmalltalkUserValue(obj: unknown): obj is SmalltalkUserValue {
	const v = obj as SmalltalkUserValue;

	return (
		typeof v !== 'undefined' &&
		typeof v.typename !== 'undefined' &&
		v.typename === typenameSmalltalkUserValue
	);
}

export class SmalltalkUserValue extends SmalltalkValueBase implements ISmalltalkUserValue {
	public readonly typename: string = typenameSmalltalkUserValue;
	public readonly value: ISmalltalkEnvironmentFrame;

	constructor(owner: ISmalltalkClass, environmentFrame: ISmalltalkEnvironmentFrame) {
		super(owner);

		this.value = environmentFrame;
	}

	public override toString(): string {
		// Avoid looking up the value of "self", as that would cause an infinite loop.
		// return string.Join("\r\n", Owner.ClRep.Where(v => !v.Equals(SmalltalkObjectClassKeeper.SelfVar)).Select(v => Value.Lookup(v)));

		return `<SmalltalkUserValue of type ${this.getTypename()}>`;
	}

	//     public override bool Equals(object obj)
	//     {
	// #if DEAD_CODE
	//         if (object.ReferenceEquals(this, obj))
	//         {
	//             return true;
	//         }
	//
	//         var otherUserValue = obj as SmalltalkUserValue;
	//
	//         return otherUserValue != null && Owner.Equals(otherUserValue.Owner) && Value.Equals(otherUserValue.Value);
	// #else
	//         // 2014/01/30: The book (at the top of page 278) recommends that this always return false.
	//         return object.ReferenceEquals(this, obj);
	// #endif
	//     }

	// eslint-disable-next-line @typescript-eslint/no-unused-vars
	public equals(other: unknown): boolean {
		return false; // 2021-11-04 : Temporary hack.
	}

	//     public override int GetHashCode()
	//     {
	// #if DEAD_CODE
	//         return Owner.GetHashCode() * 101 + Value.GetHashCode();
	// #else
	//         // 2014/01/30
	//         return 0;
	// #endif
	//     }

	public override getTypename(): string {
		return typeof this.owner !== 'undefined' ? this.owner.className : '<No owner>';
	}

	public override isObject(): boolean {
		return true;
	}

	public override toUserValue(): ISmalltalkUserValue | undefined {
		return this;
	}
}
