// tom-weatherhead/thaw-grammar/src/languages/lambda-calculus/domain-object-model/call.ts

// Call === Function Call === Application (Invocation) of Function

import { IImmutableSet } from 'thaw-common-utilities.ts';

import {
	areIsomorphic,
	ILCExpression,
	ILCSubstitution,
	ILCUnifiable,
	isLCVariable
} from './interfaces/expression';

import { isLCLambdaExpression, LCLambdaExpression } from './lambda-expression';

const typenameLCFunctionCall = 'LCFunctionCall';

export function isLCFunctionCall(obj: unknown): obj is LCFunctionCall {
	const otherLCFunctionCall = obj as LCFunctionCall;

	return (
		typeof otherLCFunctionCall !== 'undefined' &&
		otherLCFunctionCall.typename === typenameLCFunctionCall
	);
}

export class LCFunctionCall implements ILCExpression {
	public readonly typename: string = typenameLCFunctionCall;

	constructor(public readonly callee: ILCExpression, public readonly arg: ILCExpression) {}

	public toString(): string {
		return `(${this.callee} ${this.arg})`;
	}

	public containsVariableNamed(name: string): boolean {
		return this.callee.containsVariableNamed(name) || this.arg.containsVariableNamed(name);
	}

	public containsBoundVariableNamed(name: string): boolean {
		return (
			this.callee.containsBoundVariableNamed(name) ||
			this.arg.containsBoundVariableNamed(name)
		);
	}

	public containsUnboundVariableNamed(
		name: string,
		boundVariableNames: IImmutableSet<string>
	): boolean {
		return (
			this.callee.containsUnboundVariableNamed(name, boundVariableNames) ||
			this.arg.containsUnboundVariableNamed(name, boundVariableNames)
		);
	}

	// Alpha-conversion:

	public renameBoundVariable(newName: string, oldName: string): ILCExpression {
		return new LCFunctionCall(
			this.callee.renameBoundVariable(newName, oldName),
			this.arg.renameBoundVariable(newName, oldName)
		);
	}

	public substituteForUnboundVariable(name: string, value: ILCExpression): ILCExpression {
		return new LCFunctionCall(
			this.callee.substituteForUnboundVariable(name, value),
			this.arg.substituteForUnboundVariable(name, value)
		);
	}

	// Alpha-conversion is the renaming of bound variables
	// (λx.M[x]) → (λy.M[y]) 	α-conversion 	Renaming the bound variables in the expression. Used to avoid name collisions.

	// Beta-reduction is substitution
	// ((λx.M) E) → (M[x := E]) 	β-reduction 	Replacing the bound variables with the argument expression in the body of the abstraction.

	// Eta-reduction is ???
	//
	// η-reduction expresses the idea of extensionality, which in this context is that two functions are the same if and only if they give the same result for all arguments. η-reduction converts between λx.f x and f whenever x does not appear free in f.
	//
	// E.g. In Javascript, x => abs(x) eta-reduces to abs. See also https://wiki.haskell.org/Eta_conversion
	//
	// η-reduction can be seen to be the same as the concept of local completeness in natural deduction, via the Curry–Howard isomorphism.

	public betaReduce(generateNewVariableName: () => string): ILCExpression {
		// console.log(`LCFunctionCall.betaReduce() : Unevaluated callee is ${this.callee}`);
		// console.log(`LCFunctionCall.betaReduce() : Unevaluated actual parameter is ${this.arg}`);

		// First, evaluate this.callee; if it does not evaluate to a LCLambdaExpression,
		// then return.
		// const evaluatedCallee = this.callee; // Temp hack: No evaluation
		const evaluatedCallee = this.callee.betaReduce(generateNewVariableName);

		// console.log(`LCFunctionCall.betaReduce() : evaluatedCallee is ${evaluatedCallee}`);

		if (!isLCLambdaExpression(evaluatedCallee)) {
			const result = new LCFunctionCall(
				evaluatedCallee,
				this.arg.betaReduce(generateNewVariableName)
			);

			// console.log(
			// 	`LCFunctionCall.betaReduce() : evaluatedCallee is not an LCLambdaExpression; returning ${result}`
			// );

			return result;
		}

		// Next, substitute this.arg in for the arg in the evaluated callee.
		let lambdaExpression = evaluatedCallee as LCLambdaExpression;

		// console.log(`The call's callee is: ${lambdaExpression}`);
		// console.log(`The call's actual parameter is: ${this.arg}`);

		// return lambdaExpression.body
		// 	.substituteForUnboundVariable(lambdaExpression.arg.name, this.arg)
		// 	.betaReduce();

		// TODO: Rename variables as necessary (alpha reduction)
		// My idea for an algorithm:
		// 1) Build a set of all (unbound?) variables in the body;
		const argVarNames = this.arg.getSetOfAllVariableNames().toArray();
		// 2) for each var v in the set:
		//   - If v occurs as a bound variable in the callee, then:
		//     - Generate a new variable name w that does not occur in the callee;
		//     - In the callee, replace all bound occurrences of v with w.
		// console.log("Names of variables in the call's actual parameter:", argVarNames);

		for (const name of argVarNames) {
			if (lambdaExpression.containsBoundVariableNamed(name)) {
				let generatedVarName: string;

				do {
					generatedVarName = generateNewVariableName();
				} while (lambdaExpression.containsVariableNamed(generatedVarName));

				// console.log(`1) Old lambdaExpression: ${lambdaExpression}`);
				// console.log(`2) Replacing ${name} with ${generatedVarName}...`);
				lambdaExpression = lambdaExpression.renameBoundVariable(
					generatedVarName,
					name
				) as LCLambdaExpression;
				// console.log(`3) New lambdaExpression: ${lambdaExpression}`);
			}
		}

		const lambdaExpressionBody = lambdaExpression.body;

		// console.log('LCFunctionCall.betaReduce() : lambdaExpressionBody is', lambdaExpressionBody);

		const bodyAfterSubst = lambdaExpressionBody.substituteForUnboundVariable(
			lambdaExpression.arg.name,
			this.arg
		);

		// console.log(
		// 	`LCFunctionCall.betaReduce() : Replaced ${lambdaExpression.arg.name} with ${this.arg}; bodyAfterSubst is ${bodyAfterSubst}`
		// );

		const afterBeta = bodyAfterSubst.betaReduce(generateNewVariableName);

		// console.log(`LCFunctionCall.betaReduce() : After beta: ${afterBeta}`);

		return afterBeta;
	}

	public deltaReduce(): ILCExpression {
		return this;
	}

	public etaReduce(): ILCExpression {
		return this;
	}

	public getSetOfAllVariableNames(): IImmutableSet<string> {
		return this.callee.getSetOfAllVariableNames().union(this.arg.getSetOfAllVariableNames());
	}

	public applySubstitution(substitution: ILCSubstitution): ILCExpression {
		return new LCFunctionCall(
			this.callee.applySubstitution(substitution),
			this.arg.applySubstitution(substitution)
		);
	}

	public unify(other: ILCUnifiable): ILCSubstitution | undefined {
		if (isLCVariable(other)) {
			return other.unify(this);
		} else if (!isLCFunctionCall(other)) {
			return undefined;
		}

		const otherLCFunctionCall = other as LCFunctionCall;
		const unifier1 = this.callee.unify(otherLCFunctionCall.callee);

		if (typeof unifier1 === 'undefined') {
			return undefined;
		}

		const argA = this.arg.applySubstitution(unifier1);
		const argB = otherLCFunctionCall.arg.applySubstitution(unifier1);

		return argA.unify(argB);
	}

	public isIsomorphicTo(other: ILCExpression): boolean {
		return areIsomorphic(this, other);
	}
}
