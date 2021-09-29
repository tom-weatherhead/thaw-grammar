// tom-weatherhead/thaw-grammar/src/languages/lambda-calculus/domain-object-model/variable.ts

import { createSet, ifDefinedThenElse, IImmutableSet } from 'thaw-common-utilities.ts';

import { ILCExpression, ILCSubstitution, ILCUnifiable, ILCVariable } from './interfaces/expression';

import { isLCVariable, typenameLCVariable } from '../type-guards';

import { createSubstitution } from './substitution';

import { LCValueBase } from './value-base';

// All variables are irreducible.

export class LCVariable extends LCValueBase implements ILCVariable {
	constructor(public readonly name: string) {
		super(typenameLCVariable);
	}

	public toString(): string {
		return this.name;
	}

	public override equals(obj: unknown): boolean {
		const otherVar = obj as LCVariable;

		// We can compare the Name members with == because Name is a string.
		return isLCVariable(otherVar) && this.name === otherVar.name;
	}

	public override containsVariableNamed(name: string): boolean {
		return name === this.name;
	}

	public override containsUnboundVariableNamed(
		name: string,
		boundVariableNames: IImmutableSet<string>
	): boolean {
		return name === this.name && !boundVariableNames.contains(name);
	}

	public override substituteForUnboundVariable(
		name: string,
		value: ILCExpression
	): ILCExpression {
		return name === this.name ? value : this;
	}

	public override getSetOfAllVariableNames(): IImmutableSet<string> {
		return createSet([this.name]);
	}

	public override applySubstitution(substitution: ILCSubstitution): ILCExpression {
		return ifDefinedThenElse(substitution.SubstitutionList.get(this.name), this);
	}

	public unify(
		other: ILCUnifiable,
		// eslint-disable-next-line @typescript-eslint/no-unused-vars
		variablesInOriginalExpr1Param?: IImmutableSet<string>,
		variablesInOriginalExpr2Param?: IImmutableSet<string>
	): ILCSubstitution | undefined {
		// const variablesInOriginalExpr1 = ifDefinedThenElse(
		// 	variablesInOriginalExpr1Param,
		// 	this.getSetOfAllVariableNames()
		// );
		const variablesInOriginalExpr2 = ifDefinedThenElse(
			variablesInOriginalExpr2Param,
			(other as ILCExpression).getSetOfAllVariableNames()
		);

		const otherExpr = other as ILCExpression;

		if (
			this.equals(otherExpr) // ||
			// this.isNonBinding ||
			// 2014/03/13 : Don't add the binding { X = _ } to any substitution.
			// But what about a binding such as { X = foo(_) } ?
			// (isLCVariable(other) && otherExpr.isNonBinding)
		) {
			return createSubstitution();
			// } else if (otherExpr.containsVariableNamed(this.name)) {
		} else if (variablesInOriginalExpr2.contains(this.name)) {
			// This is the 'occurs' check.

			return undefined; // This Variable and the Expression are not unifiable.
		} else {
			return createSubstitution(this.name, otherExpr);
		}
	}
}
