// tom-weatherhead/thaw-grammar/src/languages/lambda-calculus-integer-extension/domain-object-model/integer-literal.ts

import { createSet, IImmutableSet } from 'thaw-common-utilities.ts';

import { ArgumentException } from '../../../common/exceptions/argument-exception';

import {
	areIsomorphic,
	BetaReductionStrategy,
	// ILCBetaReductionOptions,
	ILCExpression,
	isLCVariable,
	ISubstitution,
	IUnifiable
} from '../../lambda-calculus/domain-object-model/interfaces/expression';

import { createSubstitution } from '../../lambda-calculus/domain-object-model/substitution';

const typenameIntegerLiteral = 'LCIntegerLiteral';

export function isLCIntegerLiteral(obj: unknown): obj is LCIntegerLiteral {
	const otherIntegerLiteral = obj as LCIntegerLiteral;

	return (
		typeof otherIntegerLiteral !== 'undefined' &&
		otherIntegerLiteral.typename === typenameIntegerLiteral
	);
}

// All integer literals are irreducible.

export class LCIntegerLiteral implements ILCExpression {
	public readonly typename = typenameIntegerLiteral;
	public readonly value: number;

	// constructor(public readonly value: number) {}

	constructor(value: unknown) {
		// super();

		// HACK ThAW 2020-01-06 : The value in an IntegerLiteral can be int or float.
		// In the future, we will need to properly support FloatLiterals
		// and distinguish them from IntegerLiterals.

		if (typeof value !== 'number') {
			throw new ArgumentException(
				`IntegerLiteral constructor: typeof value '${value}' is not 'number'; it is '${typeof value}'.`,
				'value'
			);
		} else if (Number.isNaN(value)) {
			throw new ArgumentException(
				'IntegerLiteral constructor: value is not a number (NaN).',
				'value'
			);
			// } else if (Math.floor(value) !== value) {
			// throw new ArgumentException(`IntegerLiteral constructor: value '${value}' is not an integer.`, 'value');
		}

		this.value = value as number;
	}

	public toString(): string {
		// Do not allow the output to be formatted as scientific notation.

		return `${this.value}`;
	}

	public equals(obj: unknown): boolean {
		const otherIntegerLiteral = obj as LCIntegerLiteral;

		return isLCIntegerLiteral(otherIntegerLiteral) && this.value === otherIntegerLiteral.value;
	}

	public applySubstitution(substitution: ISubstitution<ILCExpression>): ILCExpression {
		return this;
	}

	public unify(
		other: IUnifiable<ILCExpression>,
		variablesInOriginalExpr1Param?: IImmutableSet<string>,
		variablesInOriginalExpr2Param?: IImmutableSet<string>
	): ISubstitution<ILCExpression> | undefined {
		const variablesInOriginalExpr1 =
			typeof variablesInOriginalExpr1Param !== 'undefined'
				? variablesInOriginalExpr1Param
				: this.getSetOfAllVariableNames();
		const variablesInOriginalExpr2 =
			typeof variablesInOriginalExpr2Param !== 'undefined'
				? variablesInOriginalExpr2Param
				: (other as ILCExpression).getSetOfAllVariableNames();

		const otherExpr = other as ILCExpression;

		if (this.equals(otherExpr)) {
			return createSubstitution();
		} else if (isLCVariable(otherExpr)) {
			return otherExpr.unify(this, variablesInOriginalExpr2, variablesInOriginalExpr1);
		} else {
			return undefined;
		}
	}

	public isIsomorphicTo(other: IUnifiable<ILCExpression>): boolean {
		return areIsomorphic(this, other);
	}

	public containsVariableNamed(name: string): boolean {
		return false;
	}

	public containsBoundVariableNamed(name: string): boolean {
		return false;
	}

	public containsUnboundVariableNamed(
		name: string,
		boundVariableNames: IImmutableSet<string>
	): boolean {
		return false;
	}

	public substituteForUnboundVariable(name: string, value: ILCExpression): ILCExpression {
		return this;
	}

	public getSetOfAllVariableNames(): IImmutableSet<string> {
		return createSet<string>();
	}

	public renameBoundVariable(newName: string, oldName: string): ILCExpression {
		// Alpha-conversion

		return this;
	}

	public isBetaReducible(): boolean {
		return false;
	}

	public betaReduce(
		strategy: BetaReductionStrategy,
		generateNewVariableName: () => string,
		maxDepth: number
	): ILCExpression {
		return this;
	}

	// public betaReduceV2(
	// 	options: ILCBetaReductionOptions,
	// 	generateNewVariableName: () => string,
	// 	maxDepth: number
	// ): ILCExpression {
	// 	return this;
	// }

	public deltaReduce(): ILCExpression {
		return this;
	}

	public etaReduce(): ILCExpression {
		return this;
	}
}
