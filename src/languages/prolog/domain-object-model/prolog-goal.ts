// tom-weatherhead/thaw-grammar/src/languages/prolog/domain-object-model/prolog-goal.ts

import { LanguageSelector } from 'thaw-lexical-analyzer';

import { IPrologExpression } from './iprolog-expression';
import { IPrologNumber } from './iprolog-number';
// import { PrologFunctor } from './prolog-functor';
import { PrologNameExpression } from './prolog-name-expression';
// import { PrologPredicate } from './prolog-predicate';
import { PrologSubstitution } from './prolog-substitution';

// ReferenceError: Cannot access 'PrologNameExpression' before initialization
// -> Circular dependency? See e.g. https://github.com/webpack/webpack/issues/12724

export function isPrologGoal(obj: unknown): obj is PrologGoal {
	const goal = obj as PrologGoal;

	// return typeof ic !== 'undefined' && typeof ic.compareTo === 'function';

	return goal instanceof PrologGoal; // && goal.Name instanceof PrologPredicate;
}

export class PrologGoal extends PrologNameExpression /* implements IPrologExpression */ {
	// public static fromFunctorExpression(
	// 	fe: PrologNameExpression<PrologFunctor>
	// ): PrologGoal {
	// 	return new PrologGoal(
	// 		fe.gs,
	// 		fe.Name,
	// 		fe.ExpressionList
	// 	);
	// }

	//public bool DCGDoNotAddExtraArguments = false; // Part of Definite Clause Grammar support.

	constructor(gsParam: LanguageSelector, predicate: string, expressionList: IPrologExpression[]) {
		super(gsParam, predicate, expressionList);
	}

	// public toString(): string {
	// 	return 'PrologGoal.toString()';
	// }

	// TODO:

	// public equals(otherExpr: IPrologExpression): boolean {
	// 	const otherGoal = otherExpr as PrologGoal;

	// 	if (
	// 		typeof otherGoal === 'undefined' ||
	// 		this.gs !== otherGoal.gs ||
	// 		this.Name !== otherGoal.Name ||
	// 		this.ExpressionList.length !== otherGoal.ExpressionList.length
	// 	) {
	// 		return false;
	// 	}

	// 	for (let i = 0; i < this.ExpressionList.length; i++) {

	// 		if (!this.ExpressionList[i].equals(otherGoal.ExpressionList[i])) {
	// 			return false;
	// 		}
	// 	}

	// 	return true;
	// }

	public ApplySubstitution(substitution: PrologSubstitution): IPrologExpression {
		return new PrologGoal(
			this.gs,
			this.Name,
			this.ExpressionList.map((expr: IPrologExpression) => expr.ApplySubstitution(substitution))
		);
	}

	public EvaluateToNumber(): IPrologNumber | undefined {
		return undefined;
	}

	public get isCut(): boolean {
		return this.Name === '!';
	}

	// #if DEAD_CODE
	//    public bool IsIsomorphicTo(PrologGoal otherGoal)
	//    {
	//        // TODO: If we are going to use this function, we should ensure that the two goals have no variables in common.
	//        var unifier = Unify(otherGoal);

	//        return unifier != null && unifier.IsOneToOne;
	//    }
	// #endif
}
