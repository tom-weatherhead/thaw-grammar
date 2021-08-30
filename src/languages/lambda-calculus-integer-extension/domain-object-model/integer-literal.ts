// tom-weatherhead/thaw-grammar/src/languages/lambda-calculus-integer-extension/domain-object-model/integer-literal.ts

import {
	// createSet,
	// IEqualityComparable,
	// IImmutableSet,
	IStringifiable
} from 'thaw-common-utilities.ts';

import { ArgumentException } from '../../../common/exceptions/argument-exception';

import { LCLambdaExpression } from '../../lambda-calculus/domain-object-model/lambda-expression';

export interface ILCIntegerExpression extends IStringifiable {
	evaluate(arg1: number, arg2: number): number | LCLambdaExpression;
}

const typenameIntegerLiteral = 'IntegerLiteral';

export function isIntegerLiteral(obj: unknown): obj is IntegerLiteral {
	const otherIntegerLiteral = obj as IntegerLiteral;

	return (
		typeof otherIntegerLiteral !== 'undefined' &&
		otherIntegerLiteral.typename === typenameIntegerLiteral
	);
}

export class IntegerLiteral implements ILCIntegerExpression {
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

	// eslint-disable-next-line @typescript-eslint/no-unused-vars
	public evaluate(arg1: number, arg2: number): number | LCLambdaExpression {
		return this.value;
	}
}
