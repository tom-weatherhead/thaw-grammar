// tom-weatherhead/thaw-grammar/src/common/domain-object-model/environment-frame.ts

import { ArgumentException } from 'thaw-interpreter-core';

import { KeyNotFoundException } from '../exceptions/key-not-found-exception';

import { IVariable } from './variable';

export interface IEnvironmentFrame<T> {
	readonly dict: Map<string, T>;
	readonly next?: IEnvironmentFrame<T>;
	readonly frameNumber: number;

	isDefined(key: IVariable<T>): boolean; // Deprecated. Renamed to has()
	// hasInThisFrame(key: IVariable<T>): boolean;
	has(key: IVariable<T>): boolean;

	lookup(key: IVariable<T>): T;
	setInThisFrame(key: IVariable<T>, value: T): void;
	add(key: IVariable<T>, value: T): void; // Deprecated
	addBubbleDown(key: IVariable<T>, value: T): void;
	findAndSet(key: IVariable<T>, value: T): T;
	findSetAndReturnBoolean(key: IVariable<T>, value: T): boolean;
	upsert(key: IVariable<T>, value: T): T;
	compose(keys: IVariable<T>[], values: T[]): void;
	stackDump(): void;
}

export class EnvironmentFrame<T> implements IEnvironmentFrame<T> {
	public readonly dict: Map<string, T>;
	public readonly next: IEnvironmentFrame<T> | undefined;
	public readonly frameNumber: number;

	constructor(next?: IEnvironmentFrame<T>) {
		this.dict = new Map<string, T>();
		this.next = next;
		this.frameNumber = typeof next !== 'undefined' ? next.frameNumber + 1 : 0;
	}

	public isDefined(key: IVariable<T>): boolean {
		return (
			this.dictionaryContainsKey(key) ||
			(typeof this.next !== 'undefined' && this.next.isDefined(key))
		);
	}

	// TODO?
	// public hasInThisFrame(key: IVariable<T>): boolean {
	// 	return this.dictionaryContainsKey(key);
	// }

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
					`EnvironmentFrame<T>.lookup('${key.name}') : Value was found but is undefined.`
				);
			}

			return lookupResult;
		}

		if (typeof this.next !== 'undefined') {
			return this.next.lookup(key);
		}

		throw new KeyNotFoundException(
			`EnvironmentFrame<T>.lookup() : No value found for variable ${key.name}.`
		);
	}

	public setInThisFrame(key: IVariable<T>, value: T): void {
		this.dict.set(key.name, value);
	}

	// add() is deprecated; use setInThisFrame() instead.
	public add(key: IVariable<T>, value: T): void {
		this.setInThisFrame(key, value);
	}

	public addBubbleDown(key: IVariable<T>, value: T): void {
		// I.e. update the key's value in this frame or in any frame below it.
		// If the key is not found, add the key and value to the global env.

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

	public findSetAndReturnBoolean(key: IVariable<T>, value: T): boolean {
		// Returns false if the key is not found in any env frame.
		// Does not create a new entry in any env frame.

		if (this.dictionaryContainsKey(key)) {
			this.dict.set(key.name, value);

			return true;
		} else if (typeof this.next !== 'undefined') {
			return this.next.findSetAndReturnBoolean(key, value);
		} else {
			return false;
		}
	}

	public findAndSet(key: IVariable<T>, value: T): T {
		if (!this.findSetAndReturnBoolean(key, value)) {
			throw new KeyNotFoundException(
				`EnvironmentFrame<T>.findAndSet() : No value found for variable ${key.name}.`
			);
		}

		return value;
	}

	// if the key exists in the env stack,
	// then update its value in the topmost env in which key exists
	// else add key and value to this env, which is the topmost env.
	// (The global env is always at the bottom of the stack.)

	public upsert(key: IVariable<T>, value: T): T {
		if (!this.findSetAndReturnBoolean(key, value)) {
			this.setInThisFrame(key, value);
		}

		return value;
	}

	public compose(keys: IVariable<T>[], values: T[]): void {
		if (keys.length !== values.length) {
			throw new ArgumentException(
				'EnvironmentFrame<T>.compose() : The keys list and the values list have different lengths.',
				'keys, values'
			);
		}

		for (let i = 0; i < keys.length; i++) {
			this.add(keys[i], values[i]);
		}
	}

	public stackDump(): void {
		console.log(`**** Environment Frame ${this.frameNumber} ****\n`);

		for (const [key, value] of this.dict.entries()) {
			console.log(`    ${key} = ${value}`);
		}

		console.log('\n');

		if (typeof this.next !== 'undefined') {
			this.next.stackDump();
		}
	}

	private dictionaryContainsKey(key: IVariable<T>): boolean {
		return this.dict.has(key.name); // This calls the .has() of the Map class, not this class.
	}
}
