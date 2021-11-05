// tom-weatherhead/thaw-grammar/src/languages/smalltalk/domain-object-model/value-base.ts

import {
	ISmalltalkClass,
	ISmalltalkEnvironmentFrame,
	ISmalltalkExpression,
	ISmalltalkGlobalInfo,
	ISmalltalkUserValue,
	ISmalltalkValue
} from '../interfaces/iexpression';

export abstract class SmalltalkValueBase implements ISmalltalkValue, ISmalltalkExpression {
	public abstract readonly typename: string;

	constructor(public readonly owner: ISmalltalkClass | undefined = undefined) {}

	public abstract getTypename(): string;

	public abstract toString(): string;

	public abstract equals(other: unknown): boolean;

	public isArray(): boolean {
		return false;
	}

	public isCharacter(): boolean {
		return false;
	}

	public get isInteger(): boolean {
		return false;
	}

	public isNumber(): boolean {
		return false;
	}

	public isObject(): boolean {
		return false;
	}

	public isString(): boolean {
		return false;
	}

	public isSymbol(): boolean {
		return false;
	}

	public toArray(): ISmalltalkValue[] | undefined {
		return undefined;
	}

	public toFloat(): number | undefined {
		return undefined;
	}

	public toInteger(): number | undefined {
		return undefined;
	}

	public toStringX(): string | undefined {
		return undefined;
	}

	public toUserValue(): ISmalltalkUserValue | undefined {
		return undefined;
	}

	/* eslint-disable @typescript-eslint/no-unused-vars */
	public evaluate(
		localEnvironment: ISmalltalkEnvironmentFrame | undefined,
		receiver: ISmalltalkValue,
		c: ISmalltalkClass | undefined,
		globalInfo: ISmalltalkGlobalInfo
	): ISmalltalkValue {
		return this;
	}
	/* eslint-enable @typescript-eslint/no-unused-vars */
}
