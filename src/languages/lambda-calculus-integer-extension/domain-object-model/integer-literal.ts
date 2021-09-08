// tom-weatherhead/thaw-grammar/src/languages/lambda-calculus-integer-extension/domain-object-model/integer-literal.ts

import { IImmutableSet } from 'thaw-common-utilities.ts';

import { ArgumentException } from '../../../common/exceptions/argument-exception';

import {
	ILCExpression,
	isLCVariable,
	ISubstitution,
	IUnifiable
} from '../../lambda-calculus/domain-object-model/interfaces/expression';

import { createSubstitution } from '../../lambda-calculus/domain-object-model/substitution';

import { LCValueBase } from '../../lambda-calculus/domain-object-model/value-base';

const typenameIntegerLiteral = 'LCIntegerLiteral';

export function isLCIntegerLiteral(obj: unknown): obj is LCIntegerLiteral {
	const otherIntegerLiteral = obj as LCIntegerLiteral;

	return (
		typeof otherIntegerLiteral !== 'undefined' &&
		otherIntegerLiteral.typename === typenameIntegerLiteral
	);
}

// All integer literals are irreducible.

export class LCIntegerLiteral extends LCValueBase {
	public readonly value: number;

	constructor(value: unknown) {
		super(typenameIntegerLiteral);

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
		} else if (Math.floor(value) !== value) {
			throw new ArgumentException(
				`IntegerLiteral constructor: value '${value}' is not an integer.`,
				'value'
			);
		}

		this.value = value as number;
	}

	public toString(): string {
		// TODO: Do not allow the output to be formatted as scientific notation.

		return `${this.value}`;
	}

	public override equals(obj: unknown): boolean {
		const otherIntegerLiteral = obj as LCIntegerLiteral;

		return isLCIntegerLiteral(otherIntegerLiteral) && this.value === otherIntegerLiteral.value;
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
}
