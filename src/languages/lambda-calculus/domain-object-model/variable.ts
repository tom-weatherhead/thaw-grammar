// tom-weatherhead/thaw-grammar/src/languages/lambda-calculus/domain-object-model/variable.ts

import { createSet, ifDefinedThenElse, IImmutableSet } from 'thaw-common-utilities.ts';

import {
	areIsomorphic,
	BetaReductionStrategy,
	ILCExpression,
	ILCSubstitution,
	ILCUnifiable,
	ILCVariable,
	isLCVariable,
	typenameLCVariable
} from './interfaces/expression';

import { createSubstitution } from './substitution';

export class LCVariable implements ILCVariable {
	public readonly typename: string = typenameLCVariable;

	constructor(public readonly name: string) {}

	public toString(): string {
		return this.name;
	}

	public equals(obj: unknown): boolean {
		const otherVar = obj as LCVariable;

		// We can compare the Name members with == because Name is a string.
		return isLCVariable(otherVar) && this.name === otherVar.name;
	}

	public containsVariableNamed(name: string): boolean {
		return name === this.name;
	}

	// eslint-disable-next-line @typescript-eslint/no-unused-vars
	public containsBoundVariableNamed(name: string): boolean {
		return false;
	}

	public containsUnboundVariableNamed(
		name: string,
		boundVariableNames: IImmutableSet<string>
	): boolean {
		return name === this.name && !boundVariableNames.contains(name);
	}

	// eslint-disable-next-line @typescript-eslint/no-unused-vars
	public renameBoundVariable(newName: string, oldName: string): ILCExpression {
		return this;
	}

	public substituteForUnboundVariable(name: string, value: ILCExpression): ILCExpression {
		return name === this.name ? value : this;
	}

	// eslint-disable-next-line @typescript-eslint/no-unused-vars
	public betaReduce(
		strategy: BetaReductionStrategy,
		generateNewVariableName: () => string,
		maxDepth: number
	): ILCExpression {
		return this;
	}

	public deltaReduce(): ILCExpression {
		return this;
	}

	public etaReduce(): ILCExpression {
		return this;
	}

	public getSetOfAllVariableNames(): IImmutableSet<string> {
		return createSet([this.name]);
	}

	public applySubstitution(substitution: ILCSubstitution): ILCExpression {
		return ifDefinedThenElse(substitution.SubstitutionList.get(this.name), this);
	}

	public unify(other: ILCUnifiable): ILCSubstitution | undefined {
		const otherExpr = other as ILCExpression;

		if (
			this.equals(otherExpr) // ||
			// this.isNonBinding ||
			// 2014/03/13 : Don't add the binding { X = _ } to any substitution.
			// But what about a binding such as { X = foo(_) } ?
			// (isLCVariable(other) && otherExpr.isNonBinding)
		) {
			return createSubstitution();
		} else if (otherExpr.containsVariableNamed(this.name)) {
			// This is the 'occurs' check.
			return undefined; // This Variable and the Expression are not unifiable.
		} else {
			return createSubstitution(this.name, otherExpr);
		}
	}

	public isIsomorphicTo(other: ILCExpression): boolean {
		return areIsomorphic(this, other);
	}
}
