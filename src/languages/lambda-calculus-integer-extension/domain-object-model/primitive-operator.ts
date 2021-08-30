// tom-weatherhead/thaw-grammar/src/languages/lambda-calculus-integer-extension/domain-object-model/primitive-operator.ts

import { LCLambdaExpression } from '../../lambda-calculus/domain-object-model/lambda-expression';
import { LCVariable } from '../../lambda-calculus/domain-object-model/variable';

import { ILCIntegerExpression } from './integer-literal';

// **** BEGIN Construct trueValue and falseValue ****

const varX = new LCVariable('x');
const varY = new LCVariable('y');

const f1 = new LCLambdaExpression(varY, varX);
const f2 = new LCLambdaExpression(varY, varY);

const trueValue = new LCLambdaExpression(varX, f1); // x => y => x
const falseValue = new LCLambdaExpression(varX, f2); // x => y => y

// **** END ****

const typenamePrimitiveOperator = 'PrimitiveOperator';

export function isPrimitiveOperator(obj: unknown): obj is PrimitiveOperator {
	const otherPrimitiveOperator = obj as PrimitiveOperator;

	return (
		typeof otherPrimitiveOperator !== 'undefined' &&
		otherPrimitiveOperator.typename === typenamePrimitiveOperator
	);
}

export class PrimitiveOperator implements ILCIntegerExpression {
	public readonly typename = typenamePrimitiveOperator;
	// private readonly numArgs = 2;

	constructor(
		public readonly name: string,
		public readonly leftChild: ILCIntegerExpression,
		public readonly rightChild: ILCIntegerExpression
	) {
		// if (['+', '-', '*', '/', '%', '=', '<', '>'].indexOf(this.name) < 0) {
		if (['+', '-', '*', '/', '%', '='].indexOf(this.name) < 0) {
			throw new Error(`Unrecognized PrimitiveOperator '${this.name}'`);
		}
	}

	public toString(): string {
		return `[${this.name} ${this.leftChild} ${this.rightChild}] `;
	}

	// Delta-reduction? See Kamin p. 194.

	public evaluate(arg1: number, arg2: number): number | LCLambdaExpression {
		switch (this.name) {
			case '+':
				return arg1 + arg2;
			case '-':
				return arg1 - arg2;
			case '*':
				return arg1 * arg2;
			case '/':
				return arg1 / arg2;
			case '%':
				return arg1 % arg2;
			case '=':
				return arg1 === arg2 ? trueValue : falseValue;
			default:
				throw new Error(`Unrecognized PrimitiveOperator '${this.name}'`);
		}
	}
}
