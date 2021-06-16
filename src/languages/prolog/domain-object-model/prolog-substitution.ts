// tom-weatherhead/thaw-grammar/src/languages/prolog/domain-object-model/prolog-substitution.ts

import { Set } from 'thaw-common-utilities.ts';

import { IPrologExpression } from './iprolog-expression';
import { PrologVariable } from './prolog-variable';

export class PrologSubstitution {
	public readonly SubstitutionList = new Map<string, IPrologExpression>();

	constructor(
		v: PrologVariable | undefined = undefined,
		expr: IPrologExpression | undefined = undefined // To conveniently make a substitution with a single entry.
	) {
		if (typeof v !== 'undefined' && typeof expr !== 'undefined') {
			this.SubstitutionList.set(v.Name, expr);
		} else if (typeof v !== 'undefined' || typeof expr !== 'undefined') {
			throw new Error(
				'PrologSubstitution constructor: One of the two args is defined; the other is undefined.'
			);
		}
	}

	public get length(): number {
		return this.SubstitutionList.size;
	}

	public toString(): string {
		// return 'PrologSubstitution.toString()';

		const result: string[] = [];

		for (const v of this.SubstitutionList.keys()) {
			const expr = this.SubstitutionList.get(v);

			if (typeof v !== 'undefined') {
				result.push(`?${v} -> ${expr}`);
			}
		}

		return '[' + result.join(';') + ']';
	}
	// public string ToString()
	// {
	//     return string.Join("; ", SubstitutionList.Keys.Select(key => string.Format("{0} <= {1}", key, SubstitutionList[key])));
	// }

	public Compose(otherSub: PrologSubstitution): PrologSubstitution {
		const newSub = new PrologSubstitution();

		// 1) Apply the Src substitution to this's terms.

		for (const key of this.SubstitutionList.keys()) {
			const sub = this.SubstitutionList.get(key);

			if (typeof sub === 'undefined') {
				throw new Error(
					'PrologSubstitution.Compose() : sub is undefined.'
				);
			}

			const newUnifiable = sub.ApplySubstitution(
				otherSub
			) as IPrologExpression;

			if (typeof newUnifiable === 'undefined') {
				throw new Error(
					'PrologSubstitution.Compose() : The result of applying a substitution to an IUnifiable is not an IUnifiable.'
				);
			}

			newSub.SubstitutionList.set(key, newUnifiable);
		}

		// 2) Remove identities.
		const varsToRemove: PrologVariable[] = [];

		for (const key of newSub.SubstitutionList.keys()) {
			const v = new PrologVariable(key);

			if (v.Equals(newSub.SubstitutionList.get(key))) {
				varsToRemove.push(new PrologVariable(key));
			}
		}

		for (const v of varsToRemove) {
			newSub.SubstitutionList.delete(v.Name);
		}

		// 3) Remove duplicate variables; i.e. add substitutions from keys in otherSub that are not keys in the "this" Substitution.

		for (const key of otherSub.SubstitutionList.keys()) {
			//if (!newSub.SubstitutionList.ContainsKey(key))    // In error.
			if (!this.SubstitutionList.has(key)) {
				// Correct, according to the CS 486 course notes.
				const v = otherSub.SubstitutionList.get(key);

				if (typeof v !== 'undefined') {
					newSub.SubstitutionList.set(key, v);
				}
			}
		}

		// #if SUBSTITUTION_COMPOSITION_VERIFICATION
		// According to Kamin, we should ensure that no member of newSub.SubstitutionList.Keys appears in newSub.SubstitutionList.Values .
		const variablesInValues = new Set<PrologVariable>();

		for (const value of newSub.SubstitutionList.values()) {
			variablesInValues.unionInPlace(value.FindBindingVariables());
		}

		for (const key of newSub.SubstitutionList.keys()) {
			if (variablesInValues.contains(new PrologVariable(key))) {
				// #if CONSOLE_WRITELINE
				// console.log(
				// 	'PrologSubstitution.Compose() : Unacceptable substitution; returning null.'
				// );
				// #endif
				throw new Error(
					`Unacceptable substitution; key == ${key}; this == ${this}; otherSub == ${otherSub}; newSub = ${newSub}`
				);
			}
		}
		// #endif

		return newSub;
	}

	public ContainsOnlyVariables(): boolean {
		return Array.from(this.SubstitutionList.values()).every(
			(v: IPrologExpression) => v.constructor.name === PrologVariable.name
		);
	}

	public FindBindingVariables(): Set<PrologVariable> {
		const result = new Set<PrologVariable>();

		for (const key of this.SubstitutionList.keys()) {
			result.add(new PrologVariable(key));

			const v = this.SubstitutionList.get(key);

			if (typeof v !== 'undefined') {
				result.unionInPlace(v.FindBindingVariables());
			}
		}

		return result;
	}

	public get IsOneToOne(): boolean {
		const values: PrologVariable[] = [];

		for (const value of this.SubstitutionList.values()) {
			const vv = value as PrologVariable;

			if (typeof vv === 'undefined') {
				return false;
			}

			values.push(vv);
		}

		return (
			values.length === Array.from(this.SubstitutionList.keys()).length
		);
	}
}
