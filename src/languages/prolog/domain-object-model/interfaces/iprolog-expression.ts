// tom-weatherhead/thaw-grammar/src/languages/prolog/domain-object-model/interfaces/iprolog-expression.ts

import { IImmutableSet } from 'thaw-common-utilities.ts';

import { IPrologNumber } from './iprolog-number';
// import { PrologSubstitution } from '../prolog-substitution';
// import { PrologVariable } from './prolog-variable';

import { ISubstitution } from './isubstitution';
import { IVariable } from './ivariable';

export interface IPrologExpression {
	// TODO:
	// equals(otherExpr: IPrologExpression): boolean;

	IsGround: boolean;
	// IsClauseOrGoal: boolean; // A hack to avoid a circular dependency

	// /* HashSet<PrologVariable> */ FindBindingVariables(): PrologVariable[];     // Finds only binding variables; ignores non-binding variables such as _
	// FindBindingVariables(): Set<PrologVariable>; // Finds only binding variables; ignores non-binding variables such as _
	FindBindingVariables(): IImmutableSet<IVariable>; // Finds only binding variables; ignores non-binding variables such as _
	/* List<PrologVariable> */ GetListOfBindingVariables(): IVariable[]; // As above, but this returns a list, which is ordered, and contains no duplicates
	ContainsVariable(v: IVariable): boolean;
	ApplySubstitution(substitution: ISubstitution): IPrologExpression;
	Unify(otherExpr: IPrologExpression): ISubstitution | undefined;
	// bool IsGround { get; }  // True iff the expression contains no variables.
	EvaluateToNumber(): IPrologNumber | undefined;
}
