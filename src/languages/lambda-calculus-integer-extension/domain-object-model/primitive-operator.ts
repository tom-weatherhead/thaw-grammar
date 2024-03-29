// tom-weatherhead/thaw-grammar/src/languages/lambda-calculus-integer-extension/domain-object-model/primitive-operator.ts

import { ifDefinedThenElse, IImmutableSet } from 'thaw-common-utilities.ts';

import {
	BetaReductionStrategy,
	ILCExpression,
	ISubstitution,
	IUnifiable
} from '../../lambda-calculus/domain-object-model/interfaces/expression';

import { defaultMaxBetaReductionDepth } from '../../lambda-calculus/utilities';

import { LCLambdaExpression } from '../../lambda-calculus/domain-object-model/lambda-expression';

import { LCValueBase } from '../../lambda-calculus/domain-object-model/value-base';

import { LCVariable } from '../../lambda-calculus/domain-object-model/variable';

import { LCIntegerLiteral, isLCIntegerLiteral } from './integer-literal';

// **** BEGIN Construct trueValue and falseValue ****

const varX = new LCVariable('x');
const varY = new LCVariable('y');

const f1 = new LCLambdaExpression(varY, varX);
const f2 = new LCLambdaExpression(varY, varY);

const trueValue = new LCLambdaExpression(varX, f1); // x => y => x
const falseValue = new LCLambdaExpression(varX, f2); // x => y => y

// **** END Construct trueValue and falseValue ****

const typenamePrimitiveOperator = 'LCPrimitiveOperator';

export function isPrimitiveOperator(obj: unknown): obj is LCPrimitiveOperator {
	const otherPrimitiveOperator = obj as LCPrimitiveOperator;

	return (
		typeof otherPrimitiveOperator !== 'undefined' &&
		otherPrimitiveOperator.typename === typenamePrimitiveOperator
	);
}

export class LCPrimitiveOperator extends LCValueBase {
	constructor(
		public readonly name: string,
		public readonly leftChild: ILCExpression,
		public readonly rightChild: ILCExpression
	) {
		super(typenamePrimitiveOperator);

		if (['+', '-', '*', '/', '%', '='].indexOf(this.name) < 0) {
			throw new Error(`Unrecognized PrimitiveOperator '${this.name}'`);
		}
	}

	public toString(): string {
		return `(${this.name} ${this.leftChild} ${this.rightChild})`;
	}

	public override applySubstitution(substitution: ISubstitution<ILCExpression>): ILCExpression {
		return new LCPrimitiveOperator(
			this.name,
			this.leftChild.applySubstitution(substitution),
			this.rightChild.applySubstitution(substitution)
		);
	}

	public unify(
		other: IUnifiable<ILCExpression>,
		variablesInOriginalExpr1Param?: IImmutableSet<string>,
		variablesInOriginalExpr2Param?: IImmutableSet<string>
	): ISubstitution<ILCExpression> | undefined {
		if (!isPrimitiveOperator(other) || other.name !== this.name) {
			return undefined;
		}

		const variablesInOriginalExpr1 = ifDefinedThenElse(
			variablesInOriginalExpr1Param,
			this.getSetOfAllVariableNames()
		);
		const variablesInOriginalExpr2 = ifDefinedThenElse(
			variablesInOriginalExpr2Param,
			(other as ILCExpression).getSetOfAllVariableNames()
		);

		const u1 = this.leftChild.unify(
			other.leftChild,
			variablesInOriginalExpr1,
			variablesInOriginalExpr2
		);

		if (typeof u1 === 'undefined') {
			return undefined;
		}

		return this.rightChild
			.applySubstitution(u1)
			.unify(
				other.rightChild.applySubstitution(u1),
				variablesInOriginalExpr1,
				variablesInOriginalExpr2
			);
	}

	public override containsVariableNamed(name: string): boolean {
		return (
			this.leftChild.containsVariableNamed(name) ||
			this.rightChild.containsVariableNamed(name)
		);
	}

	public override containsBoundVariableNamed(name: string): boolean {
		return (
			this.leftChild.containsBoundVariableNamed(name) ||
			this.rightChild.containsBoundVariableNamed(name)
		);
	}

	public override containsUnboundVariableNamed(
		name: string,
		boundVariableNames: IImmutableSet<string>
	): boolean {
		return (
			this.leftChild.containsUnboundVariableNamed(name, boundVariableNames) ||
			this.rightChild.containsUnboundVariableNamed(name, boundVariableNames)
		);
	}

	public override substituteForUnboundVariable(
		name: string,
		value: ILCExpression
	): ILCExpression {
		return new LCPrimitiveOperator(
			this.name,
			this.leftChild.substituteForUnboundVariable(name, value),
			this.rightChild.substituteForUnboundVariable(name, value)
		);
	}

	public override getSetOfAllVariableNames(): IImmutableSet<string> {
		return this.leftChild
			.getSetOfAllVariableNames()
			.union(this.rightChild.getSetOfAllVariableNames());
	}

	public override renameBoundVariable(newName: string, oldName: string): ILCExpression {
		// α-conversion

		return new LCPrimitiveOperator(
			this.name,
			this.leftChild.renameBoundVariable(newName, oldName),
			this.rightChild.renameBoundVariable(newName, oldName)
		);
	}

	public override isBetaReducible(): boolean {
		return this.leftChild.isBetaReducible() || this.rightChild.isBetaReducible();
	}

	public override betaReduce(
		options: {
			strategy?: BetaReductionStrategy;
			generateNewVariableName?: () => string;
			maxDepth?: number;
		} = {}
	): ILCExpression {
		const maxDepth = ifDefinedThenElse(options.maxDepth, defaultMaxBetaReductionDepth);

		if (maxDepth <= 0) {
			return this;
		}

		const newOptions = {
			strategy: options.strategy,
			generateNewVariableName: options.generateNewVariableName,
			maxDepth: maxDepth - 1
		};

		const l = this.leftChild.betaReduce(newOptions);
		const r = this.rightChild.betaReduce(newOptions);

		return new LCPrimitiveOperator(this.name, l, r).deltaReduce();
	}

	// Delta-reduction? See Kamin p. 194.

	public override deltaReduce(): ILCExpression {
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
}
