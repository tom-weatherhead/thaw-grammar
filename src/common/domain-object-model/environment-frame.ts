// tom-weatherhead/thaw-grammar/src/common/domain-object-model/environment-frame.ts

import { ArgumentException } from '../exceptions/argument-exception';
import { KeyNotFoundException } from '../exceptions/key-not-found-exception';

import { Variable } from './variable';

export class EnvironmentFrame<T> {
	public readonly dict: Map<string, T>;
	public readonly next: EnvironmentFrame<T> | undefined;

	constructor(next?: EnvironmentFrame<T>) {
		this.dict = new Map<string, T>();
		this.next = next;
	}

	public isDefined(key: Variable<T>): boolean {
		// if (this.dictionaryContainsKey(key)) {
		// 	return true;
		// }
		//
		// if (typeof this.next !== 'undefined') {
		// 	return this.next.isDefined(key);
		// }
		//
		// return false;

		return (
			this.dictionaryContainsKey(key) ||
			(typeof this.next !== 'undefined' && this.next.isDefined(key))
		);
	}

	public lookup(key: Variable<T>): T {
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
				throw new Error(`EnvironmentFrame.lookup('${key.name}') : Value is undefined`);
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

	public add(key: Variable<T>, value: T): void {
		this.dict.set(key.name, value);
	}

	public addBubbleDown(key: Variable<T>, value: T): void {
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

	public compose(keys: Variable<T>[], values: T[]): void {
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

	private dictionaryContainsKey(key: Variable<T>): boolean {
		// return typeof this.dict.get(key.name) !== 'undefined';

		return this.dict.has(key.name);
	}
}
