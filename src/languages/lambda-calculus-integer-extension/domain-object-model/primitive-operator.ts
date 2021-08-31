// tom-weatherhead/thaw-grammar/src/languages/lambda-calculus-integer-extension/domain-object-model/primitive-operator.ts

import { IImmutableSet } from 'thaw-common-utilities.ts';

import {
	areIsomorphic,
	BetaReductionStrategy,
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

	public applySubstitution(substitution: ISubstitution<ILCExpression>): ILCExpression {
		return new LCPrimitiveOperator(
			this.name,
			this.leftChild.applySubstitution(substitution),
			this.rightChild.applySubstitution(substitution)
		);
	}

	public unify(other: IUnifiable<ILCExpression>): ISubstitution<ILCExpression> | undefined {
		if (!isPrimitiveOperator(other) || other.name !== this.name) {
			return undefined;
		}

		const u1 = this.leftChild.unify(other.leftChild);

		if (typeof u1 === 'undefined') {
			return undefined;
		}

		// return undefined; // TODO FIXME

		return this.rightChild.applySubstitution(u1).unify(other.rightChild.applySubstitution(u1));
	}

	public isIsomorphicTo(other: IUnifiable<ILCExpression>): boolean {
		return areIsomorphic(this, other);
	}

	public containsVariableNamed(name: string): boolean {
		return (
			this.leftChild.containsVariableNamed(name) ||
			this.rightChild.containsVariableNamed(name)
		);
	}

	public containsBoundVariableNamed(name: string): boolean {
		return (
			this.leftChild.containsBoundVariableNamed(name) ||
			this.rightChild.containsBoundVariableNamed(name)
		);
	}

	public containsUnboundVariableNamed(
		name: string,
		boundVariableNames: IImmutableSet<string>
	): boolean {
		return (
			this.leftChild.containsUnboundVariableNamed(name, boundVariableNames) ||
			this.rightChild.containsUnboundVariableNamed(name, boundVariableNames)
		);
	}

	public substituteForUnboundVariable(name: string, value: ILCExpression): ILCExpression {
		return new LCPrimitiveOperator(
			this.name,
			this.leftChild.substituteForUnboundVariable(name, value),
			this.rightChild.substituteForUnboundVariable(name, value)
		);
	}

	public getSetOfAllVariableNames(): IImmutableSet<string> {
		// return createSet<string>();

		return this.leftChild
			.getSetOfAllVariableNames()
			.union(this.rightChild.getSetOfAllVariableNames());
	}

	public renameBoundVariable(newName: string, oldName: string): ILCExpression {
		// Alpha-conversion

		return this;
	}

	public betaReduce(
		strategy: BetaReductionStrategy,
		generateNewVariableName: () => string,
		maxDepth: number
	): ILCExpression {
		return this;
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

	public etaReduce(): ILCExpression {
		return this;
	}
}
