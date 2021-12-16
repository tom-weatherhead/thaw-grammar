// tom-weatherhead/thaw-grammar/src/languages/prolog/domain-object-model/interfaces/ivariable.ts

import { IImmutableSet } from 'thaw-common-utilities.ts';

import { IPrologExpression } from './iprolog-expression';

import { IPrologNumber } from './iprolog-number';

import { ISubstitution } from './isubstitution';

export const typenamePrologVariable = 'PrologVariable';

export interface IVariable extends IPrologExpression {
	readonly typename: string;
	readonly Name: string;

	IsNonBinding: boolean;
	IsGround: boolean;

	// toString(): string;
	// equals(obj: unknown): boolean;

	FindBindingVariables(): IImmutableSet<IVariable>; // or IImmutableSet<string>?
	GetListOfBindingVariables(): IVariable[]; // or IImmutableArray<string>?
	ContainsVariable(v: IVariable): boolean;
	ApplySubstitution(sub: ISubstitution): IPrologExpression;
	Unify(otherExpr: IPrologExpression): ISubstitution | undefined;
	EvaluateToNumber(): IPrologNumber | undefined;
}

export function isIVariable(obj: unknown): obj is IVariable {
	const otherIVariable = obj as IVariable;

	return (
		typeof otherIVariable !== 'undefined' &&
		otherIVariable.typename === typenamePrologVariable &&
		typeof otherIVariable.Name === 'string'
	);
}
