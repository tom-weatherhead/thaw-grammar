// tom-weatherhead/thaw-grammar/src/languages/smalltalk/domain-object-model/environment-frame.ts

import {
	ISmalltalkEnvironmentFrame,
	ISmalltalkValue,
	ISmalltalkVariable
} from './interfaces/iexpression';

// import { ISmalltalkValue } from './interfaces/ivalue';

// import { SmalltalkVariable } from './variable';

export class SmalltalkEnvironmentFrame implements ISmalltalkEnvironmentFrame {
	public readonly dict = new Map<string, ISmalltalkValue>();
	// public readonly next: ISmalltalkEnvironmentFrame;

	constructor(public readonly next: ISmalltalkEnvironmentFrame | undefined = undefined) {}

	// private HashSet<SmalltalkVariable> GetAllVariables()
	// {
	//     var result = new HashSet<SmalltalkVariable>(Dict.Keys.Where(key => !key.Equals(SmalltalkObjectClassKeeper.SelfVar)));
	//
	//     if (Next != null)
	//     {
	//         result.UnionWith(Next.GetAllVariables());
	//     }
	//
	//     return result;
	// }

	// public override bool Equals(object obj)
	// {
	//
	//     if (object.ReferenceEquals(this, obj))
	//     {
	//         return true;
	//     }
	//
	//     var otherEnvFrame = obj as SmalltalkEnvironmentFrame;
	//
	//     if (otherEnvFrame == null)
	//     {
	//         return false;
	//     }
	//
	//     var thisVars = GetAllVariables();
	//     var otherVars = otherEnvFrame.GetAllVariables();
	//
	//     if (!thisVars.IsSubsetOf(otherVars) || !otherVars.IsSubsetOf(thisVars))
	//     {
	//         return false;
	//     }
	//
	//     return thisVars.All(v => Lookup(v).Equals(otherEnvFrame.Lookup(v)));
	// }

	// public override int GetHashCode()
	// {
	//     return GetAllVariables()
	//         .Select(v => Lookup(v).GetHashCode())
	//         .Aggregate(0, (accumulator, hashCode) => accumulator + hashCode);
	// }

	// private string ToStringLocalFrame()
	// {
	//     return string.Format("({0})", string.Join("; ",
	//         Dict.Keys.Select(key => string.Format("{0} = {1}", key, key.Equals(SmalltalkObjectClassKeeper.SelfVar) ? "<self>" : Dict[key].ToString()))));
	// }

	// public override string ToString()
	// {
	//     var result = ToStringLocalFrame();
	//
	//     if (Next != null)
	//     {
	//         result = result + "; " + Next.ToString();
	//     }
	//
	//     return result;
	// }

	public isDefined(key: ISmalltalkVariable): boolean {
		if (this.dict.has(key.name)) {
			return true;
		}

		if (typeof this.next !== 'undefined') {
			return this.next.isDefined(key);
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

	public compose(keys: ISmalltalkVariable[], values: ISmalltalkValue[]): void {
		if (keys.length !== values.length) {
			throw new Error(
				'SmalltalkEnvironmentFrame.Compose() : The keys list and the values list have different lengths.'
			);
		}

		for (let i = 0; i < keys.length; ++i) {
			this.add(keys[i], values[i]);
		}
	}
}
