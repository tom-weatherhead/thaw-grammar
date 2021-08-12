// tom-weatherhead/thaw-grammar/src/languages/prolog/domain-object-model/interfaces/ivariable.ts

import { IEqualityComparable, IImmutableSet } from 'thaw-common-utilities.ts';

import { IPrologExpression } from './iprolog-expression';

import { IPrologNumber } from './iprolog-number';

import { ISubstitution } from './isubstitution';

export interface IVariable extends IEqualityComparable, IPrologExpression {
	readonly typename: string;
	readonly Name: string;

	toString(): string;
	equals(obj: unknown): boolean;
	IsNonBinding: boolean;
	FindBindingVariables(): IImmutableSet<IVariable>;
	GetListOfBindingVariables(): IVariable[];
	ContainsVariable(v: IVariable): boolean;
	ApplySubstitution(sub: ISubstitution): IPrologExpression;
	Unify(otherExpr: IPrologExpression): ISubstitution | undefined;
	IsGround: boolean;
	EvaluateToNumber(): IPrologNumber | undefined;
}
