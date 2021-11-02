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

// import { SmalltalkEnvironmentFrame } from './environment-frame';

export abstract class SmalltalkValueBase implements ISmalltalkValue, ISmalltalkExpression {
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