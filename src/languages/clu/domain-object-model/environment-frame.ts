// clu/domain-object-model/environment-frame.ts

import { ICLUEnvironmentFrame, ICLUValue, ICLUVariable } from './interfaces/ivalue';

export class CLUEnvironmentFrame {
	public readonly dict = new Map<string, ICLUValue>();

	constructor(public readonly next?: ICLUEnvironmentFrame) {}

	// public override string ToString()
	// {
	// 	return string.Format("({0})", string.Join("; ", Dict.Keys.Select(key => string.Format("{0} = {1}", key, Dict[key]))));
	// }

	public has(key: ICLUVariable): boolean {
		return this.dict.has(key.name) || (typeof this.next !== 'undefined' && this.next.has(key));
	}

	public lookup(key: ICLUVariable): ICLUValue {
		const value = this.dict.get(key.name);

		if (typeof value !== 'undefined') {
			return value;
		}

		if (typeof this.next !== 'undefined') {
			return this.next.lookup(key);
		}

		// throw new KeyNotFoundException(string.Format("CLUEnvironmentFrame.Lookup() : No value found for variable '{0}'.", key.name));

		throw new Error(
			`CLUEnvironmentFrame.Lookup() : No value found for variable '${key.name}'.`
		);
	}

	public add(key: ICLUVariable, value: ICLUValue): void {
		this.dict.set(key.name, value);
	}

	public addBubbleDown(key: ICLUVariable, value: ICLUValue): void {
		if (!this.dict.has(key.name) && typeof this.next !== 'undefined') {
			this.next.addBubbleDown(key, value); // Bubble down towards the global environment.
		} else {
			// Bug fix: Before 2013/12/05, the "else" above was absent, and the code below was executed unconditionally.

			// #if DEAD_CODE
			// Console.WriteLine("AddBubbleDown: The new value of {0} in {1} environment frame is {2}",
			// 	key, (Next != null) ? "a local" : "the global", value);
			// #endif

			this.add(key, value);
		}
	}

	public compose(keys: ICLUVariable[], values: ICLUValue[]): void {
		if (keys.length !== values.length) {
			// throw new ArgumentException("CLUEnvironmentFrame.Compose() : The keys list and the values list have different lengths.");
			throw new Error(
				'CLUEnvironmentFrame.Compose() : The keys list and the values list have different lengths.'
			);
		}

		for (let i = 0; i < keys.length; ++i) {
			this.add(keys[i], values[i]);
		}
	}
}
