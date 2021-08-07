// tom-weatherhead/thaw-grammar/src/languages/lisp/domain-object-model/integer-literal.ts

import { IExpression } from '../../../common/domain-object-model/iexpression';
import { ArgumentException } from '../../../common/exceptions/argument-exception';
import { INumber } from './inumber';
import { ISExpression } from './isexpression';
import { SExpressionBase } from './sexpression-base';

export class IntegerLiteral extends SExpressionBase implements INumber {
	// , IConvertibleToGraph
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

	// public override bool Equals(object obj)
	// {
	// 	IntegerLiteral otherIntLit = obj as IntegerLiteral;

	// 	return otherIntLit != null && Value == otherIntLit.Value;
	// }

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
}
