// prolog-functor-expression.ts

import { LanguageSelector } from 'thaw-lexical-analyzer';

import { IPrologExpression } from './iprolog-expression';
import { PrologFunctor } from './prolog-functor';
import { PrologNameExpression } from './prolog-name-expression';
import { PrologSubstitution } from './prolog-substitution';

export function isPrologFunctorExpression(
	obj: unknown
): obj is PrologNameExpression<PrologFunctor> {
	const fe = obj as PrologFunctorExpression;

	return (
		fe instanceof PrologFunctorExpression &&
		fe.Name instanceof PrologFunctor
	);
}

export class PrologFunctorExpression extends PrologNameExpression<PrologFunctor> {
	constructor(
		gsParam: LanguageSelector,
		functor: PrologFunctor,
		expressionList: IPrologExpression[]
	) {
		super(gsParam, functor, expressionList);
	}

	// public toString(): string {
	// 	return 'PrologFunctorExpression.toString()';
	// }

	public ApplySubstitution(
		substitution: PrologSubstitution
	): IPrologExpression {
		return new PrologFunctorExpression(
			this.gs,
			this.Name,
			this.ExpressionList.map((expr: IPrologExpression) =>
				expr.ApplySubstitution(substitution)
			)
		);
	}
}
