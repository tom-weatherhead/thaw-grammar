// tom-weatherhead/thaw-grammar/src/languages/prolog/domain-object-model/prolog-goal.ts

import { LanguageSelector } from 'thaw-interpreter-types';

import { IPrologExpression } from './interfaces/iprolog-expression';
import { IPrologNumber } from './interfaces/iprolog-number';
import { PrologNameExpression } from './prolog-name-expression';
import { createSubstitution } from './prolog-substitution';

import { ISubstitution } from './interfaces/isubstitution';

const typenamePrologGoal = 'PrologGoal';

export function isPrologGoal(obj: unknown): obj is PrologGoal {
	const goal = obj as PrologGoal;

	return typeof goal !== 'undefined' && goal.typename === typenamePrologGoal;
}

export class PrologGoal extends PrologNameExpression /* implements IPrologExpression */ {
	public readonly typename: string = typenamePrologGoal;

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

	public equals(other: unknown): boolean {
		return (
			isPrologGoal(other) &&
			other.gs === this.gs &&
			other.Name === this.Name &&
			other.ExpressionList.length === this.ExpressionList.length &&
			this.ExpressionList.every((expr, i) => other.ExpressionList[i].equals(expr))
		);
	}

	public ApplySubstitution(substitution: ISubstitution): PrologGoal {
		return new PrologGoal(
			this.gs,
			this.Name,
			this.ExpressionList.map((expr: IPrologExpression) =>
				expr.ApplySubstitution(substitution)
			)
		);
	}

	public Unify(otherExpr: IPrologExpression): ISubstitution | undefined {
		const otherNameExpression = otherExpr as PrologGoal;

		if (
			!isPrologGoal(otherExpr) ||
			this.Name !== otherNameExpression.Name ||
			this.ExpressionList.length !== otherNameExpression.ExpressionList.length
		) {
			return undefined;
		}

		let substitution = createSubstitution();

		for (let i = 0; i < this.ExpressionList.length; ++i) {
			const newExpr1 = this.ExpressionList[i].ApplySubstitution(substitution);
			const newExpr2 = otherNameExpression.ExpressionList[i].ApplySubstitution(substitution);
			const substitution2 = newExpr1.Unify(newExpr2);

			if (typeof substitution2 === 'undefined') {
				return undefined;
			}

			substitution = substitution.compose(substitution2);
		}

		return substitution;
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
