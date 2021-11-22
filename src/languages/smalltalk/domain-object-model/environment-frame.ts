// tom-weatherhead/thaw-grammar/src/languages/smalltalk/domain-object-model/environment-frame.ts

import { IVariable } from '../../../common/domain-object-model/variable';

import {
	ISmalltalkEnvironmentFrame,
	ISmalltalkValue,
	ISmalltalkVariable
} from './interfaces/iexpression';

export class SmalltalkEnvironmentFrame implements ISmalltalkEnvironmentFrame {
	public readonly dict = new Map<string, ISmalltalkValue>();

	constructor(public readonly next: ISmalltalkEnvironmentFrame | undefined = undefined) {}

	public isDefined(key: ISmalltalkVariable): boolean {
		if (this.dict.has(key.name)) {
			return true;
		}

		if (typeof this.next !== 'undefined') {
			return this.next.isDefined(key);
		}

		return false;
	}

	public has(key: ISmalltalkVariable): boolean {
		if (this.dict.has(key.name)) {
			return true;
		}

		if (typeof this.next !== 'undefined') {
			return this.next.has(key);
		}

		return false;
	}

	public lookup(key: ISmalltalkVariable): ISmalltalkValue {
		const value = this.dict.get(key.name);

		if (typeof value !== 'undefined') {
			return value;
		}

		if (typeof this.next !== 'undefined') {
			return this.next.lookup(key);
		}

		// throw new KeyNotFoundException(string.Format("SmalltalkEnvironmentFrame.Lookup() : No value found for variable '{0}'.", key.Name));
		throw new Error(
			`SmalltalkEnvironmentFrame.Lookup() : No value found for variable '${key.name}'`
		);
	}

	public add(key: ISmalltalkVariable, value: ISmalltalkValue): void {
		this.dict.set(key.name, value);
	}

	public addBubbleDown(key: ISmalltalkVariable, value: ISmalltalkValue): void {
		if (!this.dict.has(key.name) && typeof this.next !== 'undefined') {
			this.next.addBubbleDown(key, value); // Bubble down towards the global environment.
		} else {
			this.add(key, value);
		}
	}

	public compose(keys: IVariable<ISmalltalkValue>[], values: ISmalltalkValue[]): void {
		if (keys.length !== values.length) {
			throw new Error(
				'SmalltalkEnvironmentFrame.Compose() : The keys list and the values list have different lengths.'
			);
		}

		for (let i = 0; i < keys.length; ++i) {
			this.add(keys[i] as ISmalltalkVariable, values[i]);
		}
	}
}
