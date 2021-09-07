// tom-weatherhead/thaw-grammar/src/languages/lambda-calculus/domain-object-model/call.ts

// Call === Function Call === Application (Invocation) of Function

import { createSet, IImmutableSet } from 'thaw-common-utilities.ts';

import {
	areIsomorphic,
	BetaReductionStrategy,
	ILCBetaReductionOptions,
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
	// constructor(public readonly callee: LCExpressionMapKey, public readonly arg: LCExpressionMapKey) {}

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

	public isBetaReducible(): boolean {
		return (
			isLCLambdaExpression(this.callee) ||
			this.callee.isBetaReducible() ||
			this.arg.isBetaReducible()
		);
		// return this.callee.isBetaReducible() || this.arg.isBetaReducible();
	}

	// private
	public betaReduceCore(
		lambdaExpression: LCLambdaExpression,
		arg: ILCExpression,
		generateNewVariableName: () => string
	): ILCExpression {
		// Rename variables as necessary (alpha reduction)
		// My idea for an algorithm:
		// 1) Build a set of all (unbound?) variables in the body;

		// const argVarNames = arg.getSetOfAllVariableNames().toArray();

		// I.e. Create an array of the names of all unbound variables in arg:
		const argVarNames = arg
			.getSetOfAllVariableNames()
			.toArray()
			.filter((name: string) => arg.containsUnboundVariableNamed(name, createSet<string>()));

		// If we set argVarNames = [] so that we don't rename any variables,
		// the unit testing appears to never terminate.
		// const argVarNames: string[] = [];

		// 2) for each var v in the set:
		//   - If v occurs as a bound variable in the callee, then:
		//     - Generate a new variable name w that does not occur in the callee;
		//     - In the callee, replace all bound occurrences of v with w.
		// console.log("Names of variables in the call's actual parameter:", argVarNames);

		// The variable renaming here prevents unbound variables in arg from becoming
		// unintentionally bound when the substitution (into the Lambda expression's body)
		// is performed.

		for (const name of argVarNames) {
			if (lambdaExpression.containsBoundVariableNamed(name)) {
				let generatedVarName: string;

				do {
					generatedVarName = generateNewVariableName();
				} while (lambdaExpression.containsVariableNamed(generatedVarName));

				// α-conversion :
				lambdaExpression = lambdaExpression.renameBoundVariable(
					generatedVarName,
					name
				) as LCLambdaExpression;
			}
		}

		// const lambdaExpressionBody = lambdaExpression.body;

		// Substitution:
		// Replace all unbound occurrences of Lambda expression's formal parameter
		// (lambdaExpression.arg) in the Lambda expression's body (lambdaExpression.body)
		// with an actual parameter (arg) :

		return lambdaExpression.body.substituteForUnboundVariable(lambdaExpression.arg.name, arg);
	}

	/// call-by-name - leftmost outermost, no reductions inside abstractions

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

		if (maxDepth <= 0) {
			return this;
		}

		// First, evaluate this.callee; if it does not evaluate to a LCLambdaExpression,
		// then return.
		const evaluatedCallee = this.callee
			.deltaReduce()
			.betaReduce(BetaReductionStrategy.CallByName, generateNewVariableName, maxDepth - 1);

		if (!isLCLambdaExpression(evaluatedCallee)) {
			const result = new LCFunctionCall(
				evaluatedCallee,
				// Note: Simply using 'this.arg' as the second argument fails.
				this.arg
				// .deltaReduce()
				// .betaReduce(
				// 	BetaReductionStrategy.CallByName,
				// 	generateNewVariableName,
				// 	maxDepth - 1
				// )
			);

			return result;
		}

		// case cbn e1 of
		// Lam (x, e) => cbn (subst e2 (Lam(x, e)))
		// x := evaluatedCallee.arg
		// e := evaluatedCallee.body

		// Next, substitute this.arg in for the arg in the evaluated callee.

		return this.betaReduceCore(evaluatedCallee, this.arg, generateNewVariableName)
			.deltaReduce()
			.betaReduce(BetaReductionStrategy.CallByName, generateNewVariableName, maxDepth - 1);
	}

	/// normal - leftmost outermost; the most popular reduction strategy

	private betaReduceNormalOrder(
		generateNewVariableName: () => string,
		maxDepth: number
	): ILCExpression {
		// - Demonstrating Lambda Calculus Reduction : https://www.cs.cornell.edu/courses/cs6110/2014sp/Handouts/Sestoft.pdf
		//
		// 7 Reduction Strategies and Reduction Functions
		// 7.2 Normal Order Reduction to Normal Form
		//
		// In Standard ML:
		//
		// fun nor (Var x) = Var x
		//   | nor (Lam (x, e)) = Lam(x, nor e) // See our lambda-expression.ts
		//   | nor (App(e1, e2)) =
		//     case nor e1 of // ThAW: Was case cbn e1 of
		//       Lam(x, e) => nor (subst e2 (Lam(x, e)))
		//       | e1’ => let val e1’’ = nor e1’ in App(e1’’, nor e2) end
		//
		// In Rust:
		//
		// fn beta_nor(&mut self, limit: usize, count: &mut usize) {
		//     if limit != 0 && *count == limit {
		//         return;
		//     }
		//
		//     match *self {
		//         Abs(ref mut abstracted) => abstracted.beta_nor(limit, count),
		//         App(_) => {
		//             self.lhs_mut().unwrap().beta_cbn(limit, count);
		//
		//             if self.is_reducible(limit, *count) {
		//                 self.eval(count);
		//                 self.beta_nor(limit, count);
		//             } else {
		//                 self.lhs_mut().unwrap().beta_nor(limit, count);
		//                 self.rhs_mut().unwrap().beta_nor(limit, count);
		//             }
		//         }
		//         _ => (),
		//     }
		// }

		// ****

		if (maxDepth <= 0) {
			return this;
		}

		// First, evaluate this.callee; if it does not evaluate to a LCLambdaExpression,
		// then return.
		const evaluatedCallee = this.callee
			.deltaReduce()
			.betaReduce(BetaReductionStrategy.NormalOrder, generateNewVariableName, maxDepth - 1);

		if (!isLCLambdaExpression(evaluatedCallee)) {
			// The result is App(e1’’, nor e2),
			// where e1’’ = nor e1’ = ...
			// and e1’ = nor e1 = evaluatedCallee
			// and e1 = this.callee
			const result = new LCFunctionCall(
				evaluatedCallee
					.deltaReduce()
					.betaReduce(
						BetaReductionStrategy.NormalOrder,
						generateNewVariableName,
						maxDepth - 1
					),
				// Note: Simply using 'this.arg' as the second argument fails.
				this.arg
					.deltaReduce()
					.betaReduce(
						BetaReductionStrategy.NormalOrder,
						generateNewVariableName,
						maxDepth - 1
					)
			);

			return result;
		}

		// Next, substitute this.arg in for the arg in the evaluated callee.

		return this.betaReduceCore(evaluatedCallee, this.arg, generateNewVariableName)
			.deltaReduce()
			.betaReduce(BetaReductionStrategy.NormalOrder, generateNewVariableName, maxDepth - 1);
	}

	/// call-by-value - leftmost innermost, no reductions inside abstractions

	private betaReduceCallByValue(
		generateNewVariableName: () => string,
		maxDepth: number
	): ILCExpression {
		// - Demonstrating Lambda Calculus Reduction : https://www.cs.cornell.edu/courses/cs6110/2014sp/Handouts/Sestoft.pdf
		//
		// 7 Reduction Strategies and Reduction Functions
		// 7.x Call-by-Value Reduction...
		//
		// In Standard ML: (?)
		//
		// fun cbv (Var x) = Var x
		// 	| cbv (Lam(x, e)) = Lam(x, e)
		// 	| cbv (App(e1, e2)) =
		// 		case cbv e1 of
		// 			Lam (x, e) => cbv (subst (cbv e2) (Lam(x, e)))
		// 			| e1’ => App(e1’, (cbv e2))
		//
		// In Rust:
		//
		// fn beta_cbv(&mut self, limit: usize, count: &mut usize) {
		//     if limit != 0 && *count == limit {
		//         return;
		//     }
		//
		//     if let App(_) = *self {
		//         self.lhs_mut().unwrap().beta_cbv(limit, count);
		//         self.rhs_mut().unwrap().beta_cbv(limit, count);
		//
		//         if self.is_reducible(limit, *count) {
		//             self.eval(count);
		//             self.beta_cbv(limit, count);
		//         }
		//     }
		// }

		// ****

		if (maxDepth <= 0) {
			return this;
		}

		// First, evaluate this.callee; if it does not evaluate to a LCLambdaExpression,
		// then return.
		const evaluatedCallee = this.callee
			.deltaReduce()
			.betaReduce(BetaReductionStrategy.CallByValue, generateNewVariableName, maxDepth - 1);
		const evaluatedArg = this.arg
			.deltaReduce()
			.betaReduce(BetaReductionStrategy.CallByValue, generateNewVariableName, maxDepth - 1);

		if (!isLCLambdaExpression(evaluatedCallee)) {
			return new LCFunctionCall(evaluatedCallee, evaluatedArg);
		}

		// case cbv e1 of
		// Lam (x, e) => cbv (subst (cbv e2) (Lam(x, e)))
		// x := evaluatedCallee.arg
		// e := evaluatedCallee.body

		// Next, substitute evaluatedArg in for the arg in the evaluated callee.

		// return this.betaReduceCore(evaluatedCallee, evaluatedArg, generateNewVariableName)
		return this.betaReduceCore(evaluatedCallee, this.arg, generateNewVariableName)
			.deltaReduce()
			.betaReduce(BetaReductionStrategy.CallByValue, generateNewVariableName, maxDepth - 1);
	}

	// private betaReduceCallByValue(
	// 	generateNewVariableName: () => string,
	// 	maxDepth: number
	// ): ILCExpression {
	// 	if (maxDepth <= 0 || !isLCLambdaExpression(this.callee)) {
	// 		return this;
	// 	}
	//
	// 	const reducedArg = this.arg.betaReduce(
	// 		BetaReductionStrategy.CallByValue,
	// 		generateNewVariableName,
	// 		maxDepth - 1
	// 	);
	//
	// 	// Note here that the callee is not reduced before betaReduceCore is called.
	// 	const result = this.betaReduceCore(this.callee, reducedArg, generateNewVariableName);
	//
	// 	return result.betaReduce(
	// 		BetaReductionStrategy.CallByValue,
	// 		generateNewVariableName,
	// 		maxDepth - 1
	// 	);
	// }

	public betaReduce(
		strategy: BetaReductionStrategy,
		generateNewVariableName: () => string,
		maxDepth: number
	): ILCExpression {
		switch (strategy) {
			case BetaReductionStrategy.CallByName:
				return this.betaReduceCallByName(generateNewVariableName, maxDepth);

			case BetaReductionStrategy.NormalOrder:
				return this.betaReduceNormalOrder(generateNewVariableName, maxDepth);

			case BetaReductionStrategy.CallByValue:
				return this.betaReduceCallByValue(generateNewVariableName, maxDepth);

			default:
				throw new Error(
					`LCFunctionCall.betaReduce() : Unsupported BetaReductionStrategy ${BetaReductionStrategy[strategy]}`
				);
		}
	}

	public betaReduceV2(
		options: ILCBetaReductionOptions,
		generateNewVariableName: () => string,
		maxDepth: number
	): ILCExpression {
		if (maxDepth <= 0) {
			return this;
		}

		// const callee: ILCExpression = this.callee;

		if (!options.reduceChildrenBeforeParents && isLCLambdaExpression(this.callee)) {
			// Reduce parent before child(ren). Callee is a Lambda expression.
			return this.betaReduceCore(this.callee, this.arg, generateNewVariableName);
		}

		// Reduce child(ren)
		let reducedLeftChild = this.callee;
		let reducedRightChild = this.arg;

		if (options.reduceLeftmostChildFirst || options.reduceRecessiveChild) {
			reducedLeftChild = reducedLeftChild.betaReduceV2(
				options,
				generateNewVariableName,
				maxDepth - 1
			);
		}

		if (!options.reduceLeftmostChildFirst || options.reduceRecessiveChild) {
			reducedRightChild = reducedLeftChild.betaReduceV2(
				options,
				generateNewVariableName,
				maxDepth - 1
			);
		}

		const reducedCall = new LCFunctionCall(reducedLeftChild, reducedRightChild);

		if (options.reduceChildrenBeforeParents && options.reduceRecessiveParentOrChild) {
			// Reduce parent after child(ren).
			// TODO: Should we call betaReduceCore (as above) instead of betaReduceV2 (as below)?
			return reducedCall.betaReduceV2(options, generateNewVariableName, maxDepth - 1);
		}

		return this;
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
