// tom-weatherhead/thaw-grammar/src/languages/lambda-calculus/domain-object-model/substitution.ts

import { createSet } from 'thaw-common-utilities.ts';

// import { IPrologExpression } from './interfaces/iprolog-expression';
//
// import { ISubstitution } from './interfaces/isubstitution';
//
// import { isIVariable, IVariable } from './interfaces/ivariable';

// 2021-07-13: Warning: Circular dependency: caused by reference to prolog-variable.ts :
// prolog-substitution.js -> prolog-variable.js -> prolog-substitution.js

import { ILCSubstitution, ILCExpression, isLCVariable } from './interfaces/expression';

// import { isLCVariable } from './variable';

class LCSubstitution implements ILCSubstitution {
	public readonly SubstitutionList = new Map<string, ILCExpression>();

	constructor(
		v: string | undefined = undefined,
		expr: ILCExpression | undefined = undefined // To conveniently make a substitution with a single entry.
	) {
		if (typeof v !== 'undefined' && typeof expr !== 'undefined') {
			if (v === '') {
				throw new Error(
					'PrologSubstitution constructor: The variable name is the empty string.'
				);
			}

			this.SubstitutionList.set(v, expr);
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
		const entries = [...this.SubstitutionList.entries()];

		// e1[0] is e1.key; e1[1] would be e1.value
		// entries.sort((e1: [string, IPrologExpression], e2: [string, IPrologExpression]) => e1[0].localeCompare(e2[0]));
		entries.sort((e1, e2) => e1[0].localeCompare(e2[0]));

		// return `[${entries.map(([key, value]: [string, IPrologExpression]) => `${key} -> ${value}`).join('; ')}]`;
		return `[${entries.map(([key, value]) => `${key} -> ${value}`).join('; ')}]`;
	}

	public compose(otherSub: ILCSubstitution): ILCSubstitution {
		const newSub = new LCSubstitution();

		// 1) Apply the Src substitution to this's terms.

		// for (const key of this.SubstitutionList.keys()) {
		for (const [key, expr] of this.SubstitutionList.entries()) {
			const newUnifiable = expr.applySubstitution(otherSub) as ILCExpression;

			if (typeof newUnifiable === 'undefined') {
				throw new Error(
					'PrologSubstitution.Compose() : The result of applying a substitution to an IUnifiable is not an IUnifiable.'
				);
			}

			newSub.SubstitutionList.set(key, newUnifiable);
		}

		// 2) Remove identities.
		const varsToRemove: string[] = [];

		for (const [key, value] of newSub.SubstitutionList.entries()) {
			if (isLCVariable(value) && value.name === key) {
				throw new Error(
					'PrologSubstitution: An identity should have been removed from the substitution, but was not.'
				);
			}

			// if (v.equals(newSub.SubstitutionList.get(key))) {
			if (typeof value === 'string' && value === key) {
				varsToRemove.push(key);
			}
		}

		for (const v of varsToRemove) {
			newSub.SubstitutionList.delete(v);
		}

		// 3) Remove duplicate variables; i.e. add substitutions from keys in otherSub that are not keys in the "this" Substitution.

		for (const [key, v] of otherSub.SubstitutionList.entries()) {
			if (!this.SubstitutionList.has(key)) {
				// Correct, according to the CS 486 course notes.
				newSub.SubstitutionList.set(key, v);
			}
		}

		// #if SUBSTITUTION_COMPOSITION_VERIFICATION
		// According to Kamin, we should ensure that no member of newSub.SubstitutionList.Keys appears in newSub.SubstitutionList.Values .
		const variablesInValues = createSet<string>();

		for (const value of newSub.SubstitutionList.values()) {
			// variablesInValues.unionInPlace(value.FindBindingVariables());

			const bindingVariables = value.getSetOfAllVariableNames();

			for (const bv of bindingVariables) {
				variablesInValues.add(bv);
			}

			// variablesInValues.unionInPlace(bindingVariables);
		}

		for (const key of newSub.SubstitutionList.keys()) {
			if (variablesInValues.contains(key)) {
				throw new Error(
					`Unacceptable substitution; key == ${key}; this == ${this}; otherSub == ${otherSub}; newSub = ${newSub}`
				);
			}
		}
		// #endif

		return newSub;
	}

	public containsOnlyVariables(): boolean {
		return [...this.SubstitutionList.values()].every(isLCVariable);
	}

	// public findBindingVariables(): Set<PrologVariable> {
	// 	const result = new Set<PrologVariable>();

	// 	for (const key of this.SubstitutionList.keys()) {
	// 		result.add(new PrologVariable(key));

	// 		const v = this.SubstitutionList.get(key);

	// 		if (typeof v !== 'undefined') {
	// 			result.unionInPlace(v.FindBindingVariables());
	// 		}
	// 	}

	// 	return result;
	// }

	// public get isOneToOne(): boolean {
	// 	const values: PrologVariable[] = [];

	// 	for (const value of this.SubstitutionList.values()) {
	// 		const vv = value as PrologVariable;

	// 		if (typeof vv === 'undefined') {
	// 			return false;
	// 		}

	// 		values.push(vv);
	// 	}

	// 	return values.length === Array.from(this.SubstitutionList.keys()).length;
	// }
}

export function createSubstitution(
	v: string | undefined = undefined,
	expr: ILCExpression | undefined = undefined // To conveniently make a substitution with a single entry.
): ILCSubstitution {
	return new LCSubstitution(v, expr);
}
