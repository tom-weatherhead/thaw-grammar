// tom-weatherhead/thaw-grammar/src/languages/prolog/domain-object-model/prolog-functor-expression.ts

import { LanguageSelector } from 'thaw-interpreter-types';

import { IPrologExpression } from './interfaces/iprolog-expression';
import { IPrologNumber } from './interfaces/iprolog-number';
// import { PrologFunctor } from './prolog-functor';
import { PrologFloatLiteral } from './prolog-float-literal';
import { PrologIntegerLiteral } from './prolog-integer-literal';
import { PrologNameExpression } from './prolog-name-expression';
import { createSubstitution } from './prolog-substitution';
// import { PrologVariable } from './prolog-variable';

import { ISubstitution } from './interfaces/isubstitution';
import { isIVariable } from './interfaces/ivariable';

export function isPrologFunctorExpression(obj: unknown): obj is PrologFunctorExpression {
	// const fe = obj as PrologFunctorExpression;

	// return (
	// 	fe instanceof PrologFunctorExpression &&
	// 	fe.Name instanceof PrologFunctor
	// );

	return obj instanceof PrologFunctorExpression;
}

export class PrologFunctorExpression extends PrologNameExpression implements IPrologExpression {
	constructor(gsParam: LanguageSelector, functor: string, expressionList: IPrologExpression[]) {
		super(gsParam, functor, expressionList);
	}

	private ListToString(expr: IPrologExpression): string {
		const functorExpression = expr as PrologFunctorExpression;

		if (
			expr instanceof PrologFunctorExpression &&
			typeof functorExpression !== 'undefined' &&
			functorExpression.Name === '[]' &&
			functorExpression.ExpressionList.length === 0
		) {
			return '';
		} else if (
			expr instanceof PrologFunctorExpression &&
			typeof functorExpression !== 'undefined' &&
			functorExpression.Name === '.' &&
			functorExpression.ExpressionList.length === 2
		) {
			return `, ${functorExpression.ExpressionList[0]}${this.ListToString(
				functorExpression.ExpressionList[1]
			)}`;
		} else {
			return ` | ${expr}`;
		}
	}

	private SequenceToString(expr: IPrologExpression): string {
		const functorExpression = expr as PrologFunctorExpression;

		if (
			expr instanceof PrologFunctorExpression &&
			typeof functorExpression !== 'undefined' &&
			functorExpression.Name === 'consSeq' &&
			functorExpression.ExpressionList.length === 2
		) {
			return `${functorExpression.ExpressionList[0]}, ${this.SequenceToString(
				functorExpression.ExpressionList[1]
			)}`;
		} else {
			return `${expr}`;
		}
	}

	public override toString(): string {
		if (this.gs === LanguageSelector.Prolog2 && this.ExpressionList.length === 2) {
			if (this.Name === '.') {
				return `[${this.ExpressionList[0]}${this.ListToString(this.ExpressionList[1])}]`;
			} else if (this.Name === 'consSeq') {
				// ThAW 2014/03/28 : I added the brackets here because without them, ?- X = [(1, 2), (3, 4)], print(X). yielded [1, 2, 3, 4],
				// which was misleading.
				return `(${this.ExpressionList[0]}, ${this.SequenceToString(
					this.ExpressionList[1]
				)})`;
			}
		}

		return super.toString(); // 'PrologFunctorExpression.toString()';
	}

	public equals(other: unknown): boolean {
		return (
			isPrologFunctorExpression(other) &&
			other.gs === this.gs &&
			other.Name === this.Name &&
			other.ExpressionList.length === this.ExpressionList.length &&
			this.ExpressionList.every((expr, i) => other.ExpressionList[i].equals(expr))
		);
	}

	public ApplySubstitution(substitution: ISubstitution): IPrologExpression {
		return new PrologFunctorExpression(
			this.gs,
			this.Name,
			this.ExpressionList.map((expr: IPrologExpression) =>
				expr.ApplySubstitution(substitution)
			)
		);
	}

	public Unify(otherExpr: IPrologExpression): ISubstitution | undefined {
		// if (otherExpr.constructor.name === PrologVariable.name) {
		if (isIVariable(otherExpr)) {
			return otherExpr.Unify(this);
		}

		// if (!GetType().Equals(otherExpr.GetType()))
		// {
		//     // A PrologFunctorExpression can unify with a PrologFunctorExpression;
		//     // a PrologGoal can unify with a PrologGoal,
		//     // but a PrologFunctorExpression cannot unify with a PrologGoal.
		//     return null;
		// }

		const otherNameExpression = otherExpr as PrologFunctorExpression;

		if (
			this.constructor.name !== otherExpr.constructor.name ||
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

	private static EvaluateUnaryOperatorToNumber(
		thisFunctorExpression: PrologFunctorExpression
	): IPrologNumber | undefined {
		const arg1Evaluated = thisFunctorExpression.ExpressionList[0].EvaluateToNumber();

		if (typeof arg1Evaluated === 'undefined') {
			return undefined;
		}

		if (arg1Evaluated.constructor.name === PrologIntegerLiteral.name) {
			const arg1Value = arg1Evaluated.ToInteger();
			let result: number;

			switch (thisFunctorExpression.Name) {
				case '+':
					result = arg1Value;
					break;

				case '-':
					result = -arg1Value;
					break;

				default:
					return undefined;
			}

			return new PrologIntegerLiteral(result);
		} else {
			const arg1Value = arg1Evaluated.ToDouble();
			let result: number;

			switch (thisFunctorExpression.Name) {
				case '+':
					result = arg1Value;
					break;

				case '-':
					result = -arg1Value;
					break;

				default:
					return undefined;
			}

			return new PrologFloatLiteral(result);
		}
	}

	public EvaluateToNumber(): IPrologNumber | undefined {
		// const thisFunctorExpression =
		// 	this as PrologNameExpression<PrologFunctor>;

		// if (typeof thisFunctorExpression === 'undefined') {
		// 	return undefined;
		// } else
		if (this.ExpressionList.length === 1) {
			return PrologFunctorExpression.EvaluateUnaryOperatorToNumber(
				this // thisFunctorExpression
			);
		} else if (this.ExpressionList.length !== 2) {
			return undefined;
		}

		const arg1Evaluated = this.ExpressionList[0].EvaluateToNumber();
		const arg2Evaluated = this.ExpressionList[1].EvaluateToNumber();

		if (typeof arg1Evaluated === 'undefined' || typeof arg2Evaluated === 'undefined') {
			return undefined;
		}

		if (
			arg1Evaluated.constructor.name === PrologIntegerLiteral.name &&
			arg2Evaluated.constructor.name === PrologIntegerLiteral.name
		) {
			const arg1Value = arg1Evaluated.ToInteger();
			const arg2Value = arg2Evaluated.ToInteger();
			let result: number;

			switch (this.Name) {
				case '+':
					result = arg1Value + arg2Value;
					break;

				case '-':
					result = arg1Value - arg2Value;
					break;

				case '*':
					result = arg1Value * arg2Value;
					break;

				case '/':
					result = arg1Value / arg2Value;
					break;

				case 'mod':
					result = arg1Value % arg2Value;
					break;

				default:
					return undefined;
			}

			return new PrologIntegerLiteral(result);
		} else {
			const arg1Value = arg1Evaluated.ToDouble();
			const arg2Value = arg2Evaluated.ToDouble();
			let result: number;

			switch (this.Name) {
				case '+':
					result = arg1Value + arg2Value;
					break;

				case '-':
					result = arg1Value - arg2Value;
					break;

				case '*':
					result = arg1Value * arg2Value;
					break;

				case '/':
					result = arg1Value / arg2Value;
					break;

				case 'mod':
					result = arg1Value % arg2Value;
					break;

				default:
					return undefined;
			}

			return new PrologFloatLiteral(result);
		}
	}
}
