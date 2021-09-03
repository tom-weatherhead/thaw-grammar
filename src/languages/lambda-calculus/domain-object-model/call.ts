// tom-weatherhead/thaw-grammar/src/languages/lambda-calculus/domain-object-model/call.ts

// Call === Function Call === Application (Invocation) of Function

import { IImmutableSet } from 'thaw-common-utilities.ts';

import {
	areIsomorphic,
	BetaReductionStrategy,
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

	// private
	public betaReduceCore(
		lambdaExpression: LCLambdaExpression,
		arg: ILCExpression,
		generateNewVariableName: () => string
	): ILCExpression {
		// Rename variables as necessary (alpha reduction)
		// My idea for an algorithm:
		// 1) Build a set of all (unbound?) variables in the body;
		const argVarNames = arg.getSetOfAllVariableNames().toArray();
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

		// console.log(
		// 	`LCFunctionCall.betaReduceCore() : lambdaExpressionBody is ${lambdaExpressionBody};`,
		// 	lambdaExpressionBody
		// );
		// console.log('LCFunctionCall.betaReduceCore() : arg is', arg);

		const bodyAfterSubst = lambdaExpressionBody.substituteForUnboundVariable(
			lambdaExpression.arg.name,
			arg
		);

		// console.log(
		// 	`LCFunctionCall.betaReduceCore() : Replaced ${lambdaExpression.arg.name} with ${this.arg}; bodyAfterSubst is ${bodyAfterSubst}`
		// );

		return bodyAfterSubst;
	}

	private betaReduceCallByName(
		generateNewVariableName: () => string,
		maxDepth: number
	): ILCExpression {
		// - Demonstrating Lambda Calculus Reduction : https://www.cs.cornell.edu/courses/cs6110/2014sp/Handouts/Sestoft.pdf
		//
		// 7 Reduction Strategies and Reduction Functions
		// 7.1 Call-by-Name Reduction to Weak Head Normal Form
		//
		// In Standard ML:
		//
		// fun cbn (Var x) = Var x
		// 	| cbn (Lam(x, e)) = Lam(x, e)
		// 	| cbn (App(e1, e2)) =
		// 		case cbn e1 of
		// 			Lam (x, e) => cbn (subst e2 (Lam(x, e)))
		// 			| e1’ => App(e1’, e2)
		//
		// In Rust:
		//
		// fn beta_cbn(&mut self, limit: usize, count: &mut usize) {
		//     if limit != 0 && *count == limit {
		//         return;
		//     }
		//
		//     if let App(_) = *self {
		//         self.lhs_mut().unwrap().beta_cbn(limit, count);
		//
		//         if self.is_reducible(limit, *count) {
		//             self.eval(count);
		//             self.beta_cbn(limit, count);
		//         }
		//     }
		// }

		// ****

		// console.log(
		// 	`LCFunctionCall.betaReduce(maxDepth = ${maxDepth}) : Unevaluated callee is ${this.callee}`
		// );
		// console.log(
		// 	`LCFunctionCall.betaReduce(maxDepth = ${maxDepth}) : Unevaluated actual parameter is ${this.arg}`
		// );

		if (maxDepth <= 0) {
			return this;
		}

		// First, evaluate this.callee; if it does not evaluate to a LCLambdaExpression,
		// then return.
		// const evaluatedCallee = this.callee; // Temp hack: No evaluation
		const evaluatedCallee = this.callee
			.deltaReduce()
			.betaReduce(BetaReductionStrategy.CallByName, generateNewVariableName, maxDepth - 1);

		// console.log(
		// 	`LCFunctionCall.betaReduce(maxDepth = ${maxDepth}) : evaluatedCallee is ${evaluatedCallee}`
		// );

		if (!isLCLambdaExpression(evaluatedCallee)) {
			const result = new LCFunctionCall(
				evaluatedCallee,
				this.arg
					.deltaReduce()
					.betaReduce(
						BetaReductionStrategy.CallByName,
						generateNewVariableName,
						maxDepth - 1
					)
				// this.arg
			);

			// console.log(
			// 	`LCFunctionCall.betaReduce() : evaluatedCallee is not an LCLambdaExpression; returning ${result}`
			// );

			return result;
		}

		// case cbn e1 of
		// Lam (x, e) => cbn (subst e2 (Lam(x, e)))
		// x := evaluatedCallee.arg
		// e := evaluatedCallee.body

		// Next, substitute this.arg in for the arg in the evaluated callee.
		// let lambdaExpression = evaluatedCallee as LCLambdaExpression;

		// console.log(`The call's callee is: ${lambdaExpression}`);
		// console.log(`The call's actual parameter is: ${this.arg}`);

		// return lambdaExpression.body
		// 	.substituteForUnboundVariable(lambdaExpression.arg.name, this.arg)
		// 	.betaReduce();

		const bodyAfterSubst = this.betaReduceCore(
			evaluatedCallee,
			this.arg,
			generateNewVariableName
		);

		const afterBeta = bodyAfterSubst
			.deltaReduce()
			.betaReduce(BetaReductionStrategy.CallByName, generateNewVariableName, maxDepth - 1);

		// console.log(`LCFunctionCall.betaReduce() : After beta: ${afterBeta}`);

		return afterBeta;
	}

	public betaReduce(
		strategy: BetaReductionStrategy,
		generateNewVariableName: () => string,
		maxDepth: number
	): ILCExpression {
		switch (strategy) {
			case BetaReductionStrategy.CallByName:
				return this.betaReduceCallByName(generateNewVariableName, maxDepth);

			default:
				throw new Error(
					`LCFunctionCall.betaReduce() : Unsupported BetaReductionStrategy ${BetaReductionStrategy[strategy]}`
				);
		}
	}

	public deltaReduce(): ILCExpression {
		return new LCFunctionCall(this.callee.deltaReduce(), this.arg.deltaReduce());
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
		} else if (!isLCFunctionCall(other)) {
			// console.log(
			// 	`${other} is neither a FunctionCall nor a Variable; it fails to unify with ${this}`
			// );

			return undefined;
		}

		const otherLCFunctionCall = other as LCFunctionCall;
		const unifier1 = this.callee.unify(
			otherLCFunctionCall.callee,
			variablesInOriginalExpr1,
			variablesInOriginalExpr2
		);

		if (typeof unifier1 === 'undefined') {
			return undefined;
		}

		const argA = this.arg.applySubstitution(unifier1);
		const argB = otherLCFunctionCall.arg.applySubstitution(unifier1);

		// return argA.unify(argB);

		const unifier2 = argA.unify(argB, variablesInOriginalExpr1, variablesInOriginalExpr2);

		if (typeof unifier2 === 'undefined') {
			return undefined;
		}

		return unifier1.compose(unifier2);
	}

	public isIsomorphicTo(other: ILCExpression): boolean {
		return areIsomorphic(this, other);
	}
}
