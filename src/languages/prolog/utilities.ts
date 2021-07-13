// tom-weatherhead/thaw-grammar/src/languages/prolog/utilities.ts

import { Set } from 'thaw-common-utilities.ts';

import { PrologFunctorExpression } from './domain-object-model/prolog-functor-expression';
import { PrologGoal } from './domain-object-model/prolog-goal';
import { PrologSubstitution } from './domain-object-model/prolog-substitution';
import { PrologVariable } from './domain-object-model/prolog-variable';

export function createGoalFromFunctorExpression(fe: PrologFunctorExpression): PrologGoal {
	return new PrologGoal(fe.gs, fe.Name, fe.ExpressionList);
}

export function createFunctorExpressionFromGoal(goal: PrologGoal): PrologFunctorExpression {
	return new PrologFunctorExpression(goal.gs, goal.Name, goal.ExpressionList);
}

// UUIDs: Use the npm module 'uuid' instead of the code below.

// From https://stackoverflow.com/questions/105034/how-to-create-a-guid-uuid :

// export function uuidv4(): string {
// 	return ([1e7]+-1e3+-4e3+-8e3+-1e11).replace(/[018]/g, c =>
// 		(c ^ crypto.getRandomValues(new Uint8Array(1))[0] & 15 >> c / 4).toString(16)
// 	);
// }

// **** BEGIN Utilities for substitutions ****
// Placed here to avoid a circular reference between prolog-substitution.js and prolog-variable.js

// public ContainsOnlyVariables(): boolean {
// 	return Array.from(this.SubstitutionList.values()).every(
// 		(v: IPrologExpression) => v.constructor.name === PrologVariable.name
// 	);
// }

export function findBindingVariablesInSubstitution(s: PrologSubstitution): Set<PrologVariable> {
	const result = new Set<PrologVariable>();

	for (const key of s.SubstitutionList.keys()) {
		result.add(new PrologVariable(key));

		const v = s.SubstitutionList.get(key);

		if (typeof v !== 'undefined') {
			result.unionInPlace(v.FindBindingVariables());
		}
	}

	return result;
}

// public get IsOneToOne(): boolean {
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

// **** END Utilities for substitutions ****
