// tom-weatherhead/thaw-grammar/src/languages/lambda-calculus/domain-object-model/interfaces/expression.ts

import { IEqualityComparable, IImmutableSet, IStringifiable } from 'thaw-common-utilities.ts';

// Type T is the language's expression type.
export interface ISubstitution<T> {
	readonly SubstitutionList: Map<string, T>;

	length: number;
	// isOneToOne: boolean;

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
	unify(other: IUnifiable<T>): ISubstitution<T> | undefined;
	// isIsomorphicTo(other: IUnifiable<T>): boolean;
}

export type ILCUnifiable = IUnifiable<ILCExpression>;

// ISubstitutable<ILCExpression>,
export interface ILCExpression extends IStringifiable, IUnifiable<ILCExpression> {
	readonly typename: string;

	containsVariableNamed(name: string): boolean;
	containsBoundVariableNamed(name: string): boolean;
	// containsUnboundVariableNamed(name: string): boolean;
	renameBoundVariable(newName: string, oldName: string): ILCExpression;
	substituteForUnboundVariable(name: string, value: ILCExpression): ILCExpression;
	betaReduce(generateNewVariableName: () => string): ILCExpression;
	getSetOfAllVariableNames(): IImmutableSet<string>;
	// applySubstitution(substitution: ILCSubstitution): ILCExpression;
}

/* eslint-disable @typescript-eslint/no-empty-interface */
export interface ILCValue extends ILCExpression {}

export interface ILCVariable extends IEqualityComparable, ILCExpression {
	readonly name: string;
}

// ****

export const typenameLCVariable = 'LCVariable';

export function isLCVariable(obj: unknown): obj is ILCVariable {
	const otherLCVariable = obj as ILCExpression;

	return (
		typeof otherLCVariable !== 'undefined' && otherLCVariable.typename === typenameLCVariable
	);
}
