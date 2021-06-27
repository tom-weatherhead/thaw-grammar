// tom-weatherhead/thaw-grammar/src/languages/prolog/utilities.ts

import { LanguageSelector } from 'thaw-lexical-analyzer';

// import { PrologFunctor } from './domain-object-model/prolog-functor';
import { PrologFunctorExpression } from './domain-object-model/prolog-functor-expression';
import { PrologGoal } from './domain-object-model/prolog-goal';
// import { PrologNameExpression } from './domain-object-model/prolog-name-expression';
// import { PrologPredicate } from './domain-object-model/prolog-predicate';

export function createGoalFromFunctorExpression(
	fe: PrologFunctorExpression
): PrologGoal {
	return new PrologGoal(fe.gs, fe.Name, fe.ExpressionList);
}

export function createFunctorExpressionFromGoal(
	goal: PrologGoal
): PrologFunctorExpression {
	return new PrologFunctorExpression(goal.gs, goal.Name, goal.ExpressionList);
}

// TODO: Move deepEquals() to thaw-common-utilities.ts

// From https://stackoverflow.com/questions/201183/how-to-determine-equality-for-two-javascript-objects :

export function deepEquals(x: any, y: any): boolean {
	const ok = Object.keys;
	const tx = typeof x;
	const ty = typeof y;

	return x &&
		y &&
		tx === 'object' &&
		tx === ty &&
		x.constructor === y.constructor // Fixes the error where deepEquals({}, []) was returning true.
		? ok(x).length === ok(y).length &&
				ok(x).every((key) => deepEquals(x[key], y[key]))
		: x === y;
}

// Some comments:

// yes, if you care for such corner case, ugly solution is to replace : (x === y) with : (x === y && (x != null && y != null  || x.constructor === y.constructor)) – atmin Jul 16 '18 at 7:42

// Even when replacing (x === y) with (x === y && (x != null && y != null || x.constructor === y.constructor)), the function(v2) still returns true(in NodeJS) when comparing {} and []. – derekbaker783 Aug 7 '19 at 16:37

// : (x === y) is not the place to try to fix the {} vs [] comparison. Instead put && a.constructor === b.constructor at the end of the main condition (i.e. before the ?). – James Clark Nov 24 '19 at 2:52
