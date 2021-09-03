// tom-weatherhead/thaw-grammar/src/languages/lambda-calculus/domain-object-model/lambda-expression.ts

import { createSet, IImmutableSet } from 'thaw-common-utilities.ts';

import {
	areIsomorphic,
	BetaReductionStrategy,
	ILCExpression,
	ILCSubstitution,
	ILCUnifiable,
	ILCValue,
	isLCVariable
} from './interfaces/expression';

import { isLCFunctionCall } from './call';

import { LCVariable } from './variable';

// 'let x = e1 in e2;' can be expressed as '(λx.e2 e1)'

const typenameLCLambdaExpression = 'LCLambdaExpression';

export function isLCLambdaExpression(obj: unknown): obj is LCLambdaExpression {
	const otherLCLambdaExpression = obj as LCLambdaExpression;

	return (
		typeof otherLCLambdaExpression !== 'undefined' &&
		otherLCLambdaExpression.typename === typenameLCLambdaExpression
	);
}

// TODO: Name it 'LCLambdaExpression' or 'LCFunction' ?

export class LCLambdaExpression implements ILCValue {
	public readonly typename: string = typenameLCLambdaExpression;

	constructor(public readonly arg: LCVariable, public readonly body: ILCExpression) {}

	public toString(): string {
		return `λ${this.arg}.${this.body}`;
	}

	public containsVariableNamed(name: string): boolean {
		return this.arg.containsVariableNamed(name) || this.body.containsVariableNamed(name);
	}

	public containsBoundVariableNamed(name: string): boolean {
		return this.arg.name === name || this.body.containsBoundVariableNamed(name);
	}

	public containsUnboundVariableNamed(
		name: string,
		boundVariableNames: IImmutableSet<string>
	): boolean {
		return this.body.containsUnboundVariableNamed(
			name,
			boundVariableNames.union(createSet([...this.arg.name]))
		);
	}

	public renameBoundVariable(newName: string, oldName: string): ILCExpression {
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

	public substituteForUnboundVariable(name: string, value: ILCExpression): ILCExpression {
		return name === this.arg.name
			? this
			: new LCLambdaExpression(this.arg, this.body.substituteForUnboundVariable(name, value));
	}

	public betaReduce(
		strategy: BetaReductionStrategy,
		generateNewVariableName: () => string,
		maxDepth: number
	): ILCExpression {
		return new LCLambdaExpression(
			this.arg,
			this.body.betaReduce(strategy, generateNewVariableName, maxDepth)
		);
	}

	public deltaReduce(): ILCExpression {
		return new LCLambdaExpression(this.arg, this.body.deltaReduce());
	}

	public etaReduce(): ILCExpression {
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

	public getSetOfAllVariableNames(): IImmutableSet<string> {
		return createSet([this.arg.name]).union(this.body.getSetOfAllVariableNames());
	}

	public applySubstitution(substitution: ILCSubstitution): ILCExpression {
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
		const variablesInOriginalExpr1 =
			typeof variablesInOriginalExpr1Param !== 'undefined'
				? variablesInOriginalExpr1Param
				: this.getSetOfAllVariableNames();
		const variablesInOriginalExpr2 =
			typeof variablesInOriginalExpr2Param !== 'undefined'
				? variablesInOriginalExpr2Param
				: (other as ILCExpression).getSetOfAllVariableNames();

		if (isLCVariable(other)) {
			return other.unify(this, variablesInOriginalExpr2, variablesInOriginalExpr1);
		} else if (!isLCLambdaExpression(other)) {
			// console.log(
			// 	`${other} is neither a LambdaExpression nor a Variable; it fails to unify with ${this}`
			// );

			return undefined;
		}

		// console.log(`LCLambdaExpression.unify() : Trying to unify ${this} with ${other} ...`);

		const otherLCLambdaExpression = other as LCLambdaExpression;

		// console.log(
		// 	`LCLambdaExpression.unify() : Trying to unify ${this.arg} with ${otherLCLambdaExpression.arg} ...`
		// );

		const unifier1 = this.arg.unify(
			otherLCLambdaExpression.arg,
			variablesInOriginalExpr1,
			variablesInOriginalExpr2
		);

		if (typeof unifier1 === 'undefined') {
			// console.log(
			// 	`LCLambdaExpression.unify() : Failed to unify ${this.arg} with ${otherLCLambdaExpression.arg}`
			// );

			return undefined;
		}

		// console.log(
		// 	`LCLambdaExpression.unify() : The unifier of ${this.arg} and ${otherLCLambdaExpression.arg} is: ${unifier1}`
		// );

		const bodyA = this.body.applySubstitution(unifier1);
		const bodyB = otherLCLambdaExpression.body.applySubstitution(unifier1);

		// console.log(`LCLambdaExpression.unify() : bodyA is: ${bodyA}`);
		// console.log(`LCLambdaExpression.unify() : bodyB is: ${bodyB}`);

		const unifier2 = bodyA.unify(bodyB, variablesInOriginalExpr1, variablesInOriginalExpr2);

		if (typeof unifier2 === 'undefined') {
			// console.log(`LCLambdaExpression.unify() : Failed to unify ${bodyA} with ${bodyB}`);

			return undefined;
		}

		// console.log(
		// 	`LCLambdaExpression.unify() : The unifier of ${bodyA} and ${bodyB} is: ${unifier2}`
		// );

		const compositeSubstitution = unifier1.compose(unifier2);

		// console.log(
		// 	`LCLambdaExpression.unify() : The composition of ${unifier1} and ${unifier2} is: ${compositeSubstitution}`
		// );

		return compositeSubstitution;
	}

	public isIsomorphicTo(other: ILCExpression): boolean {
		return areIsomorphic(this, other);
	}
}
