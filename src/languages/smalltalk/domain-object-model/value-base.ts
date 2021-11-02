// value-base.ts

import {
	ISmalltalkClass,
	ISmalltalkEnvironmentFrame,
	ISmalltalkExpression,
	// ISmalltalkFunctionDefinition,
	ISmalltalkGlobalInfo,
	ISmalltalkUserValue,
	ISmalltalkValue
	// , ISmalltalkVariable
} from './interfaces/iexpression';
// import { ISmalltalkValue } from './interfaces/ivalue';

// import { SmalltalkEnvironmentFrame } from './environment-frame';

export abstract class SmalltalkValueBase implements ISmalltalkValue, ISmalltalkExpression {
	// public SmalltalkClass owner { get; private set; }

	constructor(public owner: ISmalltalkClass | undefined = undefined) {}

	public abstract getTypename(): string;

	public isNumber(): boolean {
		return false;
	}

	public get isInteger(): boolean {
		return false;
	}

	public isSymbol(): boolean {
		return false;
	}

	public isCharacter(): boolean {
		return false;
	}

	public isString(): boolean {
		return false;
	}

	public isObject(): boolean {
		return false;
	}

	public isArray(): boolean {
		return false;
	}

	public abstract toInteger(): number | undefined;

	public abstract toFloat(): number | undefined;

	public abstract toStringX(): string | undefined;

	public abstract toUserValue(): ISmalltalkUserValue | undefined;

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
