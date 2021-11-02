// user-value.ts

import {
	ISmalltalkClass,
	ISmalltalkEnvironmentFrame,
	ISmalltalkUserValue
} from './interfaces/iexpression';

// import { SmalltalkEnvironmentFrame } from './environment-frame';

import { SmalltalkValueBase } from './value-base';

export class SmalltalkUserValue extends SmalltalkValueBase implements ISmalltalkUserValue {
	public readonly value: ISmalltalkEnvironmentFrame;

	constructor(owner: ISmalltalkClass, environmentFrame: ISmalltalkEnvironmentFrame) {
		super(owner);

		this.value = environmentFrame;
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

	//     public override int GetHashCode()
	//     {
	// #if DEAD_CODE
	//         return Owner.GetHashCode() * 101 + Value.GetHashCode();
	// #else
	//         // 2014/01/30
	//         return 0;
	// #endif
	//     }

	public override toString(): string {
		// Avoid looking up the value of "self", as that would cause an infinite loop.
		// return string.Join("\r\n", Owner.ClRep.Where(v => !v.Equals(SmalltalkObjectClassKeeper.SelfVar)).Select(v => Value.Lookup(v)));

		return `<SmalltalkUserValue of type ${this.getTypename()}>`;
	}

	public override getTypename(): string {
		return this.owner.className;
	}

	public override isObject(): boolean {
		return true;
	}

	public toInteger(): number | undefined {
		return undefined;
	}

	public toFloat(): number | undefined {
		return undefined;
	}

	public toStringX(): string | undefined {
		return undefined;
	}

	public toUserValue(): ISmalltalkUserValue | undefined {
		return this;
	}
}
