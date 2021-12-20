// tom-weatherhead/thaw-grammar/src/languages/prolog/domain-object-model/interfaces/iprolog-expression.ts

import { IEqualityComparable, IImmutableSet, IStringifiable } from 'thaw-common-utilities.ts';

import { IPrologNumber } from './iprolog-number';

import { ISubstitution } from './isubstitution';
import { IPrologVariable } from './ivariable';

export interface IPrologExpression extends IEqualityComparable, IStringifiable {
	readonly typename: string;

	IsGround: boolean;
	// IsClauseOrGoal: boolean; // A hack to avoid a circular dependency

	FindBindingVariables(): IImmutableSet<IPrologVariable>; // Finds only binding variables; ignores non-binding variables such as _
	/* List<PrologVariable> */ GetListOfBindingVariables(): IPrologVariable[]; // As above, but this returns a list, which is ordered, and contains no duplicates
	ContainsVariable(v: IPrologVariable): boolean;
	ApplySubstitution(substitution: ISubstitution): IPrologExpression;
	Unify(otherExpr: IPrologExpression): ISubstitution | undefined;
	// bool IsGround { get; }  // True iff the expression contains no variables.
	EvaluateToNumber(): IPrologNumber | undefined;
}
