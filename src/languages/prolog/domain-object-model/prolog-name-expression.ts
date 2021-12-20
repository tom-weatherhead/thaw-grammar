// tom-weatherhead/thaw-grammar/src/languages/prolog/domain-object-model/prolog-name-expression.ts

import { createSet, IImmutableSet } from 'thaw-common-utilities.ts';

import { LanguageSelector } from 'thaw-interpreter-types';

import { IPrologExpression } from './interfaces/iprolog-expression';
import { IPrologNumber } from './interfaces/iprolog-number';
import { IPrologVariable } from './interfaces/ivariable';

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

	public FindBindingVariables(): IImmutableSet<IPrologVariable> {
		const result = createSet<IPrologVariable>();

		for (const expr of this.ExpressionList) {
			result.unionInPlace(expr.FindBindingVariables());
		}

		return result;
	}

	public GetListOfBindingVariables(): IPrologVariable[] {
		return this.FindBindingVariables().toArray();
	}

	public ContainsVariable(v: IPrologVariable): boolean {
		return this.ExpressionList.some((expr: IPrologExpression) => expr.ContainsVariable(v));
	}

	public get IsGround(): boolean {
		return this.ExpressionList.every((expr: IPrologExpression) => expr.IsGround);
	}

	public abstract EvaluateToNumber(): IPrologNumber | undefined;
}
