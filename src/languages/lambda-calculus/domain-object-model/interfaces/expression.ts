// tom-weatherhead/thaw-grammar/src/languages/lambda-calculus/domain-object-model/interfaces/expression.ts

//! [β-reduction](https://en.wikipedia.org/wiki/Beta_normal_form) for lambda `Term`s

import { IEqualityComparable, IImmutableSet, IStringifiable } from 'thaw-common-utilities.ts';

export enum BetaReductionStrategy {
	CallByName,
	NormalOrder,
	CallByValue,
	ApplicativeOrder,
	HybridApplicativeOrder,
	HeadSpine,
	HybridNormalOrder,
	ThAWHackForYCombinator
}

// Type T is the language's expression type.
export interface ISubstitution<T> {
	readonly SubstitutionList: Map<string, T>;

	length: number;
	isOneToOne: boolean;

	toString(): string;
	compose(otherSub: ISubstitution<T>): ISubstitution<T>;
	containsOnlyVariables(): boolean;
	// findBindingVariables(): IImmutableSet<IVariable>;
}

export type ILCSubstitution = ISubstitution<ILCExpression>;

export interface ISubstitutable<T> {
	applySubstitution(substitution: ISubstitution<T>): T;
}

export interface IUnifiable<T> extends ISubstitutable<T> {
	unify(
		other: IUnifiable<T>,
		// I.e. if x is in this, and y is in other, then we don't want to
		// a unifier to replace x with y if y is in variablesInOriginalExpr1.
		variablesInOriginalExpr1?: IImmutableSet<string>,
		variablesInOriginalExpr2?: IImmutableSet<string>
	): ISubstitution<T> | undefined;
	isIsomorphicTo(other: IUnifiable<T>): boolean;
}

export type ILCUnifiable = IUnifiable<ILCExpression>;

// export interface ILCBetaReductionOptions {
// 	readonly reduceLeftmostChildFirst: boolean;
// 	readonly reduceRecessiveChild: boolean; // I.e. if reduceLeftmostChildFirst, then reduce the right child (of a function call) after reducing the left child.
//
// 	readonly reduceChildrenBeforeParents: boolean;
// 	readonly reduceRecessiveParentOrChild: boolean; // I.e. if reduceChildrenBeforeParents, then reduce the parent after reducing the child(ren);
// }

// An LCExpressionMapKey is a reference to an expression value in the map.
export type LCExpressionMapKey = number;
// export type LCExpressionMapKey = string;

export type LCExpressionMapType = Map<LCExpressionMapKey, ILCExpression>;

// ISubstitutable<ILCExpression>,
export interface ILCExpression extends IStringifiable, IUnifiable<ILCExpression> {
	readonly typename: string;
	// readonly mapKey: LCExpressionMapKey;
	// So whenever this expression is reduced, call:
	// expressionMap.set(this.mapKey, reducedExpr);
	// This by-reference paradigm should enable lazy evaluation (?)
	// as well as avoiding repeated evaluations of copies of the same expression.

	containsVariableNamed(name: string): boolean;
	containsBoundVariableNamed(name: string): boolean;
	containsUnboundVariableNamed(name: string, boundVariableNames: IImmutableSet<string>): boolean;
	substituteForUnboundVariable(name: string, value: ILCExpression): ILCExpression;
	getSetOfAllVariableNames(): IImmutableSet<string>;
	// TODO? : getArrayOfAllUnboundVariableNames(): string[];
	// applySubstitution(substitution: ILCSubstitution): ILCExpression;

	// α-conversion
	renameBoundVariable(newName: string, oldName: string): ILCExpression; // Alpha-conversion

	// β-reduction
	isBetaReducible(): boolean;
	betaReduce(
		strategy: BetaReductionStrategy,
		generateNewVariableName: () => string,
		maxDepth: number
	): ILCExpression;
	// betaReduceV2(
	// 	options: ILCBetaReductionOptions,
	// 	generateNewVariableName: () => string,
	// 	maxDepth: number
	// ): ILCExpression;

	// δ-reduction for extended Lambda calculus; e.g. ((+ 2) 3) δ-> 5
	deltaReduce(): ILCExpression;

	// η-reduction
	etaReduce(): ILCExpression;

	// κ-reduction is the reduction of the SKI combinators (?)
}

/* eslint-disable @typescript-eslint/no-empty-interface */
export interface ILCValue extends ILCExpression {}

export interface ILCFunctionCall extends ILCExpression {
	readonly callee: ILCExpression;
	readonly arg: ILCExpression;
}

export interface ILCLambdaExpression extends ILCValue {
	readonly arg: ILCVariable;
	readonly body: ILCExpression;
}

export interface ILCVariable extends IEqualityComparable, ILCExpression {
	readonly name: string;
}
