// tom-weatherhead/thaw-grammar/src/languages/lambda-calculus-integer-extension/domain-object-model/primitive-operator.ts

import { createSet, IImmutableSet } from 'thaw-common-utilities.ts';

import {
	ILCExpression,
	ISubstitution,
	IUnifiable
} from '../../lambda-calculus/domain-object-model/interfaces/expression';

import { LCLambdaExpression } from '../../lambda-calculus/domain-object-model/lambda-expression';
import { LCVariable } from '../../lambda-calculus/domain-object-model/variable';

import { LCIntegerLiteral, isLCIntegerLiteral } from './integer-literal';

// **** BEGIN Construct trueValue and falseValue ****

const varX = new LCVariable('x');
const varY = new LCVariable('y');

const f1 = new LCLambdaExpression(varY, varX);
const f2 = new LCLambdaExpression(varY, varY);

const trueValue = new LCLambdaExpression(varX, f1); // x => y => x
const falseValue = new LCLambdaExpression(varX, f2); // x => y => y

// **** END ****

const typenamePrimitiveOperator = 'LCPrimitiveOperator';

export function isPrimitiveOperator(obj: unknown): obj is LCPrimitiveOperator {
	const otherPrimitiveOperator = obj as LCPrimitiveOperator;

	return (
		typeof otherPrimitiveOperator !== 'undefined' &&
		otherPrimitiveOperator.typename === typenamePrimitiveOperator
	);
}

export class LCPrimitiveOperator implements ILCExpression {
	public readonly typename = typenamePrimitiveOperator;
	// private readonly numArgs = 2;

	constructor(
		public readonly name: string,
		public readonly leftChild: ILCExpression,
		public readonly rightChild: ILCExpression
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

	public deltaReduce(): ILCExpression {
		const leftValue = this.leftChild.deltaReduce();
		const rightValue = this.rightChild.deltaReduce();

		if (!isLCIntegerLiteral(leftValue) || !isLCIntegerLiteral(rightValue)) {
			return new LCPrimitiveOperator(this.name, leftValue, rightValue);
		}

		const arg1 = leftValue.value;
		const arg2 = rightValue.value;

		switch (this.name) {
			case '+':
				return new LCIntegerLiteral(arg1 + arg2);
			case '-':
				return new LCIntegerLiteral(arg1 - arg2);
			case '*':
				return new LCIntegerLiteral(arg1 * arg2);
			case '/':
				return new LCIntegerLiteral(arg1 / arg2);
			case '%':
				return new LCIntegerLiteral(arg1 % arg2);
			case '=':
				return arg1 === arg2 ? trueValue : falseValue;
			default:
				throw new Error(`Unrecognized PrimitiveOperator '${this.name}'`);
		}
	}

	public applySubstitution(substitution: ISubstitution<ILCExpression>): ILCExpression {
		return this;
	}

	public unify(other: IUnifiable<ILCExpression>): ISubstitution<ILCExpression> | undefined {
		return undefined; // TODO FIXME
	}

	public isIsomorphicTo(other: IUnifiable<ILCExpression>): boolean {
		return false; // TODO FIXME
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

	public renameBoundVariable(newName: string, oldName: string): ILCExpression {
		// Alpha-conversion

		return this;
	}

	public substituteForUnboundVariable(name: string, value: ILCExpression): ILCExpression {
		return this;
	}

	public betaReduce(generateNewVariableName: () => string): ILCExpression {
		return this;
	}

	public etaReduce(): ILCExpression {
		return this;
	}

	public getSetOfAllVariableNames(): IImmutableSet<string> {
		return createSet<string>();
	}
}
