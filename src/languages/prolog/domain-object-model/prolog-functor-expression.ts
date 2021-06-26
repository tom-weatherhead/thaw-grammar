// tom-weatherhead/thaw-grammar/src/languages/prolog/domain-object-model/prolog-functor-expression.ts

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

	// TODO:

	// public equals(otherExpr: IPrologExpression): boolean {
	// 	const otherFunctorExpression = otherExpr as PrologFunctorExpression;

	// 	if (
	// 		typeof otherFunctorExpression === 'undefined' ||
	// 		this.gs !== otherFunctorExpression.gs ||
	// 		this.Name.Name !== otherFunctorExpression.Name.Name ||
	// 		this.ExpressionList.length !== otherFunctorExpression.ExpressionList.length
	// 	) {
	// 		return false;
	// 	}

	// 	for (let i = 0; i < this.ExpressionList.length; i++) {

	// 		if (!this.ExpressionList[i].equals(otherFunctorExpression.ExpressionList[i])) {
	// 			return false;
	// 		}
	// 	}

	// 	return true;
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
