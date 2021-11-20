// tom-weatherhead/thaw-grammar/src/common/domain-object-model/environment-frame.ts

import { ArgumentException } from 'thaw-interpreter-core';

import { KeyNotFoundException } from '../exceptions/key-not-found-exception';

import { IVariable } from './variable';

export interface IEnvironmentFrame<T> {
	readonly dict: Map<string, T>;
	readonly next?: IEnvironmentFrame<T>;

	isDefined(key: IVariable<T>): boolean; // Deprecated. Renamed to has() :
	has(key: IVariable<T>): boolean;

	lookup(key: IVariable<T>): T;
	add(key: IVariable<T>, value: T): void;
	addBubbleDown(key: IVariable<T>, value: T): void;
	compose(keys: IVariable<T>[], values: T[]): void;
}

export class EnvironmentFrame<T> implements IEnvironmentFrame<T> {
	public readonly dict: Map<string, T>;
	public readonly next: IEnvironmentFrame<T> | undefined;

	constructor(next?: IEnvironmentFrame<T>) {
		this.dict = new Map<string, T>();
		this.next = next;
	}

	public isDefined(key: IVariable<T>): boolean {
		return (
			this.dictionaryContainsKey(key) ||
			(typeof this.next !== 'undefined' && this.next.isDefined(key))
		);
	}

	public has(key: IVariable<T>): boolean {
		return (
			this.dictionaryContainsKey(key) ||
			(typeof this.next !== 'undefined' && this.next.has(key))
		);
	}

	public lookup(key: IVariable<T>): T {
		// console.log(`EnvironmentFrame.lookup() : Looking up the value of ${key.name}...`);

		if (this.dictionaryContainsKey(key)) {
			// console.log(`EnvironmentFrame.lookup() : Found the value of ${key.name}.`);

			// Console.WriteLine("Lookup: The value of {0} in {1} environment frame is {2}",
			// 	key, (next != null) ? "a local" : "the global", dict[key]);

			// return this.dict.get(key.name) as T;
			const lookupResult = this.dict.get(key.name); // as T;

			// if (lookupResult === undefined) {
			// 	console.error('Error in EnvironmentFrame.lookup() : lookupResult is defined, but is not of the expected type.');
			// }

			if (typeof lookupResult === 'undefined') {
				throw new Error(
					`EnvironmentFrame.lookup('${key.name}') : Value was found but is undefined.`
				);
			}

			return lookupResult;
		}

		if (typeof this.next !== 'undefined') {
			return this.next.lookup(key);
		}

		throw new KeyNotFoundException(
			`EnvironmentFrame.lookup() : No value found for variable ${key.name}.`
		);
	}

	public add(key: IVariable<T>, value: T): void {
		this.dict.set(key.name, value);
	}

	public addBubbleDown(key: IVariable<T>, value: T): void {
		// console.log(`EnvironmentFrame<T>.AddBubbleDown() : var is ${key.name}; value is ${value}`);

		// if (value === undefined) {
		// 	console.log('Warning in EnvironmentFrame.addBubbleDown() : The value being added is falsy.');
		// }

		if (!this.dictionaryContainsKey(key) && typeof this.next !== 'undefined') {
			this.next.addBubbleDown(key, value); // Bubble down towards the global environment.
		} else {
			// Bug fix: Before 2013/12/04, the "else" above was absent, and the code below was executed unconditionally.
			// Console.WriteLine("AddBubbleDown: The new value of {0} in {1} environment frame is {2}",
			// 	key, (next != null) ? "a local" : "the global", value);
			this.add(key, value);
		}
	}

	public compose(keys: IVariable<T>[], values: T[]): void {
		if (keys.length !== values.length) {
			throw new ArgumentException(
				'Environment.Compose() : The keys list and the values list have different lengths.',
				''
			);
		}

		for (let i = 0; i < keys.length; i++) {
			this.add(keys[i], values[i]);
		}
	}

	private dictionaryContainsKey(key: IVariable<T>): boolean {
		return this.dict.has(key.name); // This calls the .has() of the Map class, not this class.
	}
}
