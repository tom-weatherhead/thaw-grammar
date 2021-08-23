// tom-weatherhead/thaw-grammar/src/languages/lambda-calculus/domain-object-model/variable.ts

// ISet
import { createSet, ifDefinedThenElse, IImmutableSet } from 'thaw-common-utilities.ts';

import {
	ILCExpression,
	ILCSubstitution,
	ILCUnifiable,
	ILCVariable,
	isLCVariable,
	typenameLCVariable
} from './interfaces/expression';

import { createSubstitution } from './substitution';

// export const typenameLCVariable = 'LCVariable';
//
// export function isLCVariable(obj: unknown): obj is LCVariable {
// 	const otherLCVariable = obj as ILCExpression;
//
// 	return (
// 		typeof otherLCVariable !== 'undefined' && otherLCVariable.typename === typenameLCVariable
// 	);
// }

export class LCVariable implements ILCVariable {
	public readonly typename: string = typenameLCVariable;

	constructor(public readonly name: string) {
		// if (this.name.length !== 1) {
		// 	throw new Error(`LCVariable: Name '${this.name}' is not 1 character long.`);
		// }
	}

	public containsVariableNamed(name: string): boolean {
		return name === this.name;
	}

	// eslint-disable-next-line @typescript-eslint/no-unused-vars
	public containsBoundVariableNamed(name: string): boolean {
		return false;
	}

	// eslint-disable-next-line @typescript-eslint/no-unused-vars
	public renameBoundVariable(newName: string, oldName: string): ILCExpression {
		return this;
	}

	public substituteForUnboundVariable(name: string, value: ILCExpression): ILCExpression {
		return name === this.name ? value : this;
	}

	// eslint-disable-next-line @typescript-eslint/no-unused-vars
	public betaReduce(generateNewVariableName: () => string): ILCExpression {
		return this;
	}

	public toString(): string {
		return this.name;
	}

	public equals(obj: unknown): boolean {
		const otherVar = obj as LCVariable;

		// We can compare the Name members with == because Name is a string.
		return (
			// typeof otherVar !== 'undefined' &&
			// otherVar instanceof PrologVariable &&
			isLCVariable(otherVar) && this.name === otherVar.name
		);
	}

	public getSetOfAllVariableNames(): IImmutableSet<string> {
		return createSet([this.name]);
	}

	public applySubstitution(substitution: ILCSubstitution): ILCExpression {
		// const value = substitution.SubstitutionList.get(this.name);
		//
		// if (typeof value !== 'undefined') {
		// 	return value;
		// }
		//
		// return this;

		return ifDefinedThenElse(substitution.SubstitutionList.get(this.name), this);
	}

	public unify(other: ILCUnifiable): ILCSubstitution | undefined {
		const otherExpr = other as ILCExpression;
		// const otherVariable = otherExpr as LCVariable;

		if (
			this.equals(otherExpr) // ||
			// this.isNonBinding ||
			// 2014/03/13 : Don't add the binding { X = _ } to any substitution.
			// But what about a binding such as { X = foo(_) } ?
			// (isLCVariable(other) && otherExpr.isNonBinding)
		) {
			return createSubstitution();
		} else if (
			// [PrologClause.name, PrologGoal.name].indexOf(otherExpr.constructor.name) >= 0 ||
			// [PrologGoal.name].indexOf(otherExpr.constructor.name) >= 0 ||
			otherExpr.containsVariableNamed(this.name)
		) {
			// This is the "occurs" check.
			return undefined; // This PrologVariable and the IPrologExpression are not unifiable.
		} else {
			return createSubstitution(this.name, otherExpr);
		}
	}
}
