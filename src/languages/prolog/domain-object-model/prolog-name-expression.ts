// tom-weatherhead/thaw-grammar/src/languages/prolog/domain-object-model/prolog-name-expression.ts

import { createSet, IImmutableSet } from 'thaw-common-utilities.ts';

import { LanguageSelector } from 'thaw-interpreter-types';

import { IPrologExpression } from './interfaces/iprolog-expression';
import { IPrologNumber } from './interfaces/iprolog-number';
import { IVariable } from './interfaces/ivariable';

export abstract class PrologNameExpression /* implements IPrologExpression */ {
	public readonly gs: LanguageSelector;
	public readonly Name: string;
	public readonly ExpressionList: IPrologExpression[];
	// public DCGDoNotAddExtraArguments = false; // Part of Definite Clause Grammar support.

	constructor(
		gs: LanguageSelector,
		name: string, // T,
		expressionList: IPrologExpression[] | undefined = undefined
	) {
		this.gs = gs;
		this.Name = name;

		if (typeof expressionList !== 'undefined') {
			this.ExpressionList = expressionList;
		} else {
			this.ExpressionList = [];
		}
	}

	// private ListToString(expr: IPrologExpression): string {
	// 	const functorExpression = expr as PrologNameExpression<PrologFunctor>;

	// 	if (
	// 		typeof functorExpression !== 'undefined' &&
	// 		functorExpression.Name === '[]' &&
	// 		functorExpression.ExpressionList.length === 0
	// 	) {
	// 		return '';
	// 	} else if (
	// 		typeof functorExpression !== 'undefined' &&
	// 		functorExpression.Name === '.' &&
	// 		functorExpression.ExpressionList.length === 2
	// 	) {
	// 		return `, ${functorExpression.ExpressionList[0]}${this.ListToString(
	// 			functorExpression.ExpressionList[1]
	// 		)}`;
	// 	} else {
	// 		return ` | ${expr}`;
	// 	}
	// }

	// private SequenceToString(expr: IPrologExpression): string {
	// 	const functorExpression = expr as PrologNameExpression<PrologFunctor>;

	// 	if (
	// 		typeof functorExpression !== 'undefined' &&
	// 		functorExpression.Name === 'consSeq' &&
	// 		functorExpression.ExpressionList.length === 2
	// 	) {
	// 		return `${
	// 			functorExpression.ExpressionList[0]
	// 		}, ${this.SequenceToString(functorExpression.ExpressionList[1])}`;
	// 	} else {
	// 		return `${expr}`;
	// 	}
	// }

	public toString(): string {
		// const nameAsString = this.Name.toString();
		// const isProlog2FunctorExpression = this.gs === LanguageSelector.Prolog2;

		if (this.ExpressionList.length === 0) {
			// #if DEAD_CODE
			// if (isProlog2FunctorExpression && nameAsString == "nil")
			// {
			// 	return "[]";
			// }
			// #endif
			return this.Name;
			// } else if (gs === LanguageSelector.Prolog) {
			// 	return string.Format("({0} {1})", Name, string.Join(" ", ExpressionList.Select(expr => expr.ToString())));
		}
		// else if (
		// 	isProlog2FunctorExpression &&
		// 	this.ExpressionList.length === 2
		// ) {
		// 	if (this.Name === '.') {
		// 		return `[${this.ExpressionList[0]}${this.ListToString(
		// 			this.ExpressionList[1]
		// 		)}]`;
		// 		// } else if (this.Name === 'consSeq') {
		// 		// 	// #if DEAD_CODE
		// 		// 	// return string.Format("{0}, {1}", ExpressionList[0], SequenceToString(ExpressionList[1]));
		// 		// 	// #else
		// 		// 	// ThAW 2014/03/28 : I added the brackets here because without them, ?- X = [(1, 2), (3, 4)], print(X). yielded [1, 2, 3, 4],
		// 		// 	// which was misleading.
		// 		// 	return `(${this.ExpressionList[0]}, ${this.SequenceToString(
		// 		// 		this.ExpressionList[1]
		// 		// 	)})`;
		// 		// 	// #endif
		// 	}
		// }

		return `${this.Name}(${this.ExpressionList.map((expr: IPrologExpression) =>
			expr.toString()
		).join(', ')})`;
	}

	public FindBindingVariables(): IImmutableSet<IVariable> {
		const result = createSet<IVariable>();

		for (const expr of this.ExpressionList) {
			result.unionInPlace(expr.FindBindingVariables());
		}

		return result;
	}

	public GetListOfBindingVariables(): IVariable[] {
		return this.FindBindingVariables().toArray();
	}

	public ContainsVariable(v: IVariable): boolean {
		return this.ExpressionList.some((expr: IPrologExpression) => expr.ContainsVariable(v));
	}

	// public abstract ApplySubstitution(substitution: PrologSubstitution): IPrologExpression;

	// public ApplySubstitution(
	// 	substitution: PrologSubstitution
	// ): IPrologExpression {
	// 	return new PrologNameExpression<T>(
	// 		this.gs,
	// 		this.Name,
	// 		this.ExpressionList.map((expr: IPrologExpression) =>
	// 			expr.ApplySubstitution(substitution)
	// 		)
	// 	);
	// }

	// public Unify(otherExpr: IPrologExpression): PrologSubstitution | undefined {
	// 	// if (otherExpr.constructor.name === PrologVariable.name) {
	// 	if (otherExpr instanceof PrologVariable) {
	// 		return otherExpr.Unify(this);
	// 	}

	// 	// if (!GetType().Equals(otherExpr.GetType()))
	// 	// {
	// 	//     // A PrologFunctorExpression can unify with a PrologFunctorExpression;
	// 	//     // a PrologGoal can unify with a PrologGoal,
	// 	//     // but a PrologFunctorExpression cannot unify with a PrologGoal.
	// 	//     return null;
	// 	// }

	// 	const otherNameExpression = otherExpr as PrologNameExpression;

	// 	if (
	// 		this.constructor.name !== otherExpr.constructor.name ||
	// 		this.Name !== otherNameExpression.Name ||
	// 		this.ExpressionList.length !== otherNameExpression.ExpressionList.length
	// 	) {
	// 		return undefined;
	// 	}

	// 	let substitution = new PrologSubstitution();

	// 	for (let i = 0; i < this.ExpressionList.length; ++i) {
	// 		const newExpr1 = this.ExpressionList[i].ApplySubstitution(substitution);
	// 		const newExpr2 = otherNameExpression.ExpressionList[i].ApplySubstitution(substitution);
	// 		const substitution2 = newExpr1.Unify(newExpr2);

	// 		if (typeof substitution2 === 'undefined') {
	// 			return undefined;
	// 		}

	// 		substitution = substitution.Compose(substitution2);
	// 	}

	// 	return substitution;
	// }

	public get IsGround(): boolean {
		return this.ExpressionList.every((expr: IPrologExpression) => expr.IsGround);
	}

	// private static EvaluateUnaryOperatorToNumber(
	// 	thisFunctorExpression: PrologNameExpression<PrologFunctor>
	// ): IPrologNumber | undefined {
	// 	const arg1Evaluated =
	// 		thisFunctorExpression.ExpressionList[0].EvaluateToNumber();

	// 	if (typeof arg1Evaluated === 'undefined') {
	// 		return undefined;
	// 	}

	// 	if (arg1Evaluated.constructor.name === PrologIntegerLiteral.name) {
	// 		const arg1Value = arg1Evaluated.ToInteger();
	// 		let result: number;

	// 		switch (thisFunctorExpression.Name) {
	// 			case '+':
	// 				result = arg1Value;
	// 				break;

	// 			case '-':
	// 				result = -arg1Value;
	// 				break;

	// 			default:
	// 				return undefined;
	// 		}

	// 		return new PrologIntegerLiteral(result);
	// 	} else {
	// 		const arg1Value = arg1Evaluated.ToDouble();
	// 		let result: number;

	// 		switch (thisFunctorExpression.Name) {
	// 			case '+':
	// 				result = arg1Value;
	// 				break;

	// 			case '-':
	// 				result = -arg1Value;
	// 				break;

	// 			default:
	// 				return undefined;
	// 		}

	// 		return new PrologFloatLiteral(result);
	// 	}
	// }

	public abstract EvaluateToNumber(): IPrologNumber | undefined;

	// public EvaluateToNumber(): IPrologNumber | undefined {
	// 	const thisFunctorExpression =
	// 		this as PrologNameExpression<PrologFunctor>;

	// 	if (typeof thisFunctorExpression === 'undefined') {
	// 		return undefined;
	// 	} else if (this.ExpressionList.length === 1) {
	// 		return PrologNameExpression.EvaluateUnaryOperatorToNumber(
	// 			thisFunctorExpression
	// 		);
	// 	} else if (this.ExpressionList.length !== 2) {
	// 		return undefined;
	// 	}

	// 	const arg1Evaluated = this.ExpressionList[0].EvaluateToNumber();
	// 	const arg2Evaluated = this.ExpressionList[1].EvaluateToNumber();

	// 	if (
	// 		typeof arg1Evaluated === 'undefined' ||
	// 		typeof arg2Evaluated === 'undefined'
	// 	) {
	// 		return undefined;
	// 	}

	// 	if (
	// 		arg1Evaluated.constructor.name === PrologIntegerLiteral.name &&
	// 		arg2Evaluated.constructor.name === PrologIntegerLiteral.name
	// 	) {
	// 		const arg1Value = arg1Evaluated.ToInteger();
	// 		const arg2Value = arg2Evaluated.ToInteger();
	// 		let result: number;

	// 		switch (this.Name) {
	// 			case '+':
	// 				result = arg1Value + arg2Value;
	// 				break;

	// 			case '-':
	// 				result = arg1Value - arg2Value;
	// 				break;

	// 			case '*':
	// 				result = arg1Value * arg2Value;
	// 				break;

	// 			case '/':
	// 				result = arg1Value / arg2Value;
	// 				break;

	// 			case 'mod':
	// 				result = arg1Value % arg2Value;
	// 				break;

	// 			default:
	// 				return undefined;
	// 		}

	// 		return new PrologIntegerLiteral(result);
	// 	} else {
	// 		const arg1Value = arg1Evaluated.ToDouble();
	// 		const arg2Value = arg2Evaluated.ToDouble();
	// 		let result: number;

	// 		switch (this.Name) {
	// 			case '+':
	// 				result = arg1Value + arg2Value;
	// 				break;

	// 			case '-':
	// 				result = arg1Value - arg2Value;
	// 				break;

	// 			case '*':
	// 				result = arg1Value * arg2Value;
	// 				break;

	// 			case '/':
	// 				result = arg1Value / arg2Value;
	// 				break;

	// 			case 'mod':
	// 				result = arg1Value % arg2Value;
	// 				break;

	// 			default:
	// 				return undefined;
	// 		}

	// 		return new PrologFloatLiteral(result);
	// 	}
	// }

	// The ToGoal() method was causing a circular dependency:
	// ReferenceError: Cannot access 'PrologNameExpression' before initialization
	// See e.g. https://github.com/webpack/webpack/issues/12724

	// public ToGoal(): PrologGoal {
	// 	return new PrologGoal(
	// 		this.gs,
	// 		new PrologPredicate(this.Name),
	// 		this.ExpressionList
	// 	);
	// }
}
