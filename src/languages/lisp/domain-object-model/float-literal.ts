// tom-weatherhead/thaw-grammar/src/languages/lisp/domain-object-model/float-literal.ts

import { ArgumentException, EvaluationException } from 'thaw-interpreter-core';

import { IExpression } from '../../../common/domain-object-model/iexpression';
import { INumber } from './inumber';
import { ISExpression } from './isexpression';
import { SExpressionBase } from './sexpression-base';

export class FloatLiteral extends SExpressionBase implements INumber {
	// , IConvertibleToGraph
	public readonly value: number;

	constructor(value: unknown) {
		super();

		if (typeof value !== 'number') {
			throw new ArgumentException(
				`FloatLiteral constructor: typeof value is not 'number'; it is '${typeof value}'.`,
				'value'
			);
		} else if (Number.isNaN(value)) {
			throw new ArgumentException(
				'FloatLiteral constructor: value is not a number (NaN).',
				'value'
			);
		}

		this.value = value as number;
	}

	// public override string ToString()
	// {
	// 	// E.g. If d == 13.0, we want the string to be "13.0" rather than just "13", so that we can distinguish it from an integer.
	// 	// Note: d == (tan (/ pi 4)) is a good test case; it should yield "1.0", not "1".
	// 	var result = Value.ToString();

	// 	if (result.IndexOf('.') < 0 && result.IndexOf('E') < 0) // I.e. If result does not contain either '.' or 'E'.
	// 	{
	// 		result = result + ".0";
	// 	}

	// 	return result;
	// }

	public toString(): string {
		// TODO: Ensure that there is a decimal point in the resulting string.
		// Floating-point numbers must always contain a decimal point.
		// Integers must never contain a decimal point.
		// Do not allow the output to be formatted as scientific notation.
		let result = `${this.value}`;

		if (result.match(/[A-Za-z]/)) {
			throw new EvaluationException(
				`FloatLiteral.toString() : The result ${result} contains one or more alphabetic characters`
			);
		}

		if (result.indexOf('.') < 0) {
			result = result + '.0';
		}

		return result;
	}

	// public override bool Equals(object obj)
	// {
	// 	FloatLiteral otherFltLit = obj as FloatLiteral;

	// 	return otherFltLit != null && Value == otherFltLit.Value;
	// }

	public toInteger(): number {
		return Math.floor(this.value);
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
