// tom-weatherhead/thaw-grammar/src/languages/lambda-calculus/domain-object-model/lambda-expression.ts

import { createSet, ifDefinedThenElse, IImmutableSet } from 'thaw-common-utilities.ts';

import {
	BetaReductionStrategy,
	ILCExpression,
	ILCLambdaExpression,
	ILCSubstitution,
	ILCUnifiable
} from './interfaces/expression';

import {
	isLCFunctionCall,
	isLCLambdaExpression,
	isLCVariable,
	typenameLCLambdaExpression
} from '../type-guards';

import { LCValueBase } from './value-base';

import { LCVariable } from './variable';

// 'let x = e1 in e2;' can be expressed as '(λx.e2 e1)'

// TODO: Name it 'LCLambdaExpression' or 'LCFunction' ?

export class LCLambdaExpression extends LCValueBase implements ILCLambdaExpression {
	constructor(public readonly arg: LCVariable, public readonly body: ILCExpression) {
		super(typenameLCLambdaExpression);
	}
	// constructor(public readonly arg: LCVariable, public readonly body: LCExpressionMapKey) {}

	public toString(): string {
		return `λ${this.arg}.${this.body}`;
	}

	public override containsVariableNamed(name: string): boolean {
		return this.arg.containsVariableNamed(name) || this.body.containsVariableNamed(name);
	}

	public override containsBoundVariableNamed(name: string): boolean {
		return this.arg.name === name || this.body.containsBoundVariableNamed(name);
	}

	public override containsUnboundVariableNamed(
		name: string,
		boundVariableNames: IImmutableSet<string>
	): boolean {
		return this.body.containsUnboundVariableNamed(
			name,
			boundVariableNames.union(createSet([...this.arg.name]))
		);
	}

	public override renameBoundVariable(newName: string, oldName: string): ILCExpression {
		if (this.arg.name === oldName) {
			const newVariable = new LCVariable(newName);

			return new LCLambdaExpression(
				newVariable,
				this.body.substituteForUnboundVariable(oldName, newVariable)
			);
		} else {
			return new LCLambdaExpression(
				this.arg,
				this.body.renameBoundVariable(newName, oldName)
			);
		}
	}

	public override substituteForUnboundVariable(
		name: string,
		value: ILCExpression
	): ILCExpression {
		return name === this.arg.name
			? this
			: new LCLambdaExpression(this.arg, this.body.substituteForUnboundVariable(name, value));
	}

	public override isBetaReducible(): boolean {
		return this.body.isBetaReducible();
	}

	public override betaReduce(
		strategy: BetaReductionStrategy,
		generateNewVariableName: () => string,
		maxDepth: number
	): ILCExpression {
		if (maxDepth <= 0) {
			return this;
		}

		// 'redex' means 'reducible expression'.
		const redex = this.etaReduce();

		if (!isLCLambdaExpression(redex)) {
			return redex.betaReduce(strategy, generateNewVariableName, maxDepth);
		}

		switch (strategy) {
			case BetaReductionStrategy.CallByName:
				return redex;

			case BetaReductionStrategy.NormalOrder:
				return new LCLambdaExpression(
					redex.arg,
					redex.body.betaReduce(strategy, generateNewVariableName, maxDepth - 1)
				);

			case BetaReductionStrategy.CallByValue:
				return redex;

			case BetaReductionStrategy.ApplicativeOrder:
				return new LCLambdaExpression(
					redex.arg,
					redex.body.betaReduce(strategy, generateNewVariableName, maxDepth - 1)
				);

			case BetaReductionStrategy.ThAWHackForYCombinator:
				// Note: Simply returning 'redex' here fails for the Y comb.
				// This is the second of two differences between this strategy and CallByName.
				// return redex; // Use this for real CallByName semantics.
				// ThAW hack 2021-09-07 :
				return new LCLambdaExpression(
					redex.arg,
					redex.body.betaReduce(strategy, generateNewVariableName, maxDepth - 1)
				);

			default:
				throw new Error(
					`LCLambdaExpression.betaReduce() : Unsupported BetaReductionStrategy ${BetaReductionStrategy[strategy]}`
				);
		}
	}

	public override deltaReduce(): ILCExpression {
		return new LCLambdaExpression(this.arg, this.body.deltaReduce());
	}

	// public isEtaReducible(): boolean {
	// 	return !this.body.containsUnboundVariableNamed(this.arg.name, createSet<string>());
	// }

	public override etaReduce(): ILCExpression {
		// λx.(f x) eta-reduces to f iff x does not occur unbound in f.

		if (
			isLCFunctionCall(this.body) &&
			isLCVariable(this.body.arg) &&
			this.body.arg.name === this.arg.name &&
			!this.body.callee.containsUnboundVariableNamed(this.arg.name, createSet<string>())
		) {
			return this.body.callee.etaReduce();
		}

		return this;
	}

	public override getSetOfAllVariableNames(): IImmutableSet<string> {
		return createSet([this.arg.name]).union(this.body.getSetOfAllVariableNames());
	}

	public override applySubstitution(substitution: ILCSubstitution): ILCExpression {
		const newArg = this.arg.applySubstitution(substitution);

		if (!isLCVariable(newArg)) {
			throw new Error(
				`LCLambdaExpression.applySubstitution() : '${newArg}' is not a variable`
			);
		}

		return new LCLambdaExpression(
			newArg as LCVariable,
			this.body.applySubstitution(substitution)
		);
	}

	public unify(
		other: ILCUnifiable,
		variablesInOriginalExpr1Param?: IImmutableSet<string>,
		variablesInOriginalExpr2Param?: IImmutableSet<string>
	): ILCSubstitution | undefined {
		const variablesInOriginalExpr1 = ifDefinedThenElse(
			variablesInOriginalExpr1Param,
			this.getSetOfAllVariableNames()
		);
		const variablesInOriginalExpr2 = ifDefinedThenElse(
			variablesInOriginalExpr2Param,
			(other as ILCExpression).getSetOfAllVariableNames()
		);

		if (isLCVariable(other)) {
			return other.unify(this, variablesInOriginalExpr2, variablesInOriginalExpr1);
		} else if (!isLCLambdaExpression(other)) {
			return undefined;
		}

		const otherLCLambdaExpression = other as LCLambdaExpression;

		const unifier1 = this.arg.unify(
			otherLCLambdaExpression.arg,
			variablesInOriginalExpr1,
			variablesInOriginalExpr2
		);

		if (typeof unifier1 === 'undefined') {
			return undefined;
		}

		const bodyA = this.body.applySubstitution(unifier1);
		const bodyB = otherLCLambdaExpression.body.applySubstitution(unifier1);

		const unifier2 = bodyA.unify(bodyB, variablesInOriginalExpr1, variablesInOriginalExpr2);

		if (typeof unifier2 === 'undefined') {
			return undefined;
		}

		return unifier1.compose(unifier2);
	}
}
