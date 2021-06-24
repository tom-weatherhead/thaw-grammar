// tom-weatherhead/thaw-grammar/src/languages/prolog/utilities.ts

import { LanguageSelector } from 'thaw-lexical-analyzer';

import { PrologFunctor } from './domain-object-model/prolog-functor';
import { PrologGoal } from './domain-object-model/prolog-goal';
import { PrologNameExpression } from './domain-object-model/prolog-name-expression';
import { PrologPredicate } from './domain-object-model/prolog-predicate';

export function createGoalFromFunctorExpression(
	fe: PrologNameExpression<PrologFunctor>
): PrologGoal {
	return new PrologGoal(
		fe.gs,
		new PrologPredicate(fe.Name.Name),
		fe.ExpressionList
	);
}

export function createFunctorExpressionFromGoal(
	goal: PrologGoal
): PrologNameExpression<PrologFunctor> {
	return new PrologNameExpression<PrologFunctor>(
		goal.gs,
		new PrologFunctor(goal.Name.Name),
		goal.ExpressionList
	);
}
