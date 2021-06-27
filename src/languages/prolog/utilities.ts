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
