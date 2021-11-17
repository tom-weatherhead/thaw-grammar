// tom-weatherhead/thaw-grammar/src/languages/smalltalk/domain-object-model/user-value.ts

import {
	ISmalltalkClass,
	ISmalltalkEnvironmentFrame,
	ISmalltalkUserValue
} from '../interfaces/iexpression';

import { selfVariableName } from '../bootstrap';

import { SmalltalkValueBase } from './base';

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
		// Avoid looking up the value of 'self', as that would cause an infinite loop.

		// return string.Join("\r\n", Owner.ClRep.Where(v => !v.Equals(SmalltalkObjectClassKeeper.SelfVar)).Select(v => Value.Lookup(v)));

		const variableNames = Array.from(this.value.dict.keys()).filter(
			(v) => v !== selfVariableName
		);

		variableNames.sort();

		const values = variableNames.map((v) => `${v} = ${this.value.dict.get(v)}`);

		return `${this.getTypename()}: ${values.join('; ')}`;
	}

	public equals(other: unknown): boolean {
		// 2021-11-16 : Is this next comment still valid? :
		// 2014/01/30: The book (at the top of page 278) recommends that this always return false.

		if (!isSmalltalkUserValue(other)) {
			return false;
		}

		if (typeof this.owner === 'undefined' || typeof other.owner === 'undefined') {
			throw new Error('SmalltalkUserValue.equals() : An owner is undefined.');
		}

		return this.toString() === other.toString();
	}

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
