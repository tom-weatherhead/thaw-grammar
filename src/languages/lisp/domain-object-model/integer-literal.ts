// tom-weatherhead/thaw-grammar/src/languages/lisp/domain-object-model/integer-literal.ts

import { ArgumentException } from 'thaw-interpreter-core';

import { IExpression } from '../../../common/domain-object-model/iexpression';
import { INumber } from './inumber';
import { ISExpression } from './isexpression';
import { SExpressionBase } from './sexpression-base';

const typenameIntegerLiteral = 'IntegerLiteral';

export function isIntegerLiteral(obj: unknown): obj is IntegerLiteral {
	const otherIntegerLiteral = obj as IntegerLiteral;

	return (
		typeof otherIntegerLiteral !== 'undefined' &&
		otherIntegerLiteral.typename === typenameIntegerLiteral
	);
}

export class IntegerLiteral extends SExpressionBase implements INumber {
	// , IConvertibleToGraph
	public readonly typename: string = typenameIntegerLiteral;
	public readonly value: number;

	constructor(value: unknown) {
		super();

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

	public toInteger(): number {
		return this.value;
	}

	public toDouble(): number {
		return this.value;
	}

	public override isNumber(): boolean {
		return true;
	}

	public convertToGraph(): IExpression<ISExpression> {
		return this;
	}

	public override isEqualTo(other: unknown): boolean {
		return isIntegerLiteral(other) && other.value === this.value;
	}
}
