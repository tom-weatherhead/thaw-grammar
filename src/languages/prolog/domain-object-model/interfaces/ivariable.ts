// tom-weatherhead/thaw-grammar/src/languages/prolog/domain-object-model/interfaces/ivariable.ts

import { IImmutableSet } from 'thaw-common-utilities.ts';

import { IPrologExpression } from './iprolog-expression';

import { IPrologNumber } from './iprolog-number';

import { ISubstitution } from './isubstitution';

export const typenamePrologVariable = 'PrologVariable';

export interface IPrologVariable extends IPrologExpression {
	// readonly typename: string;
	readonly Name: string;

	IsNonBinding: boolean;
	IsGround: boolean;

	FindBindingVariables(): IImmutableSet<IPrologVariable>; // or IImmutableSet<string>?
	GetListOfBindingVariables(): IPrologVariable[]; // or IImmutableArray<string>?
	ContainsVariable(v: IPrologVariable): boolean;
	ApplySubstitution(sub: ISubstitution): IPrologExpression;
	Unify(otherExpr: IPrologExpression): ISubstitution | undefined;
	EvaluateToNumber(): IPrologNumber | undefined;
}

export function isIPrologVariable(obj: unknown): obj is IPrologVariable {
	const otherIVariable = obj as IPrologVariable;

	return (
		typeof otherIVariable !== 'undefined' &&
		otherIVariable.typename === typenamePrologVariable &&
		typeof otherIVariable.Name === 'string'
	);
}
