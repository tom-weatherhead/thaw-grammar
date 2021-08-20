// tom-weatherhead/thaw-grammar/src/languages/lambda-calculus/domain-object-model/call.ts

import { IImmutableSet } from 'thaw-common-utilities.ts';

import { isLCLambdaExpression, LCLambdaExpression } from './lambda-expression';

import { ILCExpression } from './variable';

export class LCFunctionCall implements ILCExpression {
	constructor(public readonly callee: ILCExpression, public readonly arg: ILCExpression) {}

	public containsVariableNamed(name: string): boolean {
		return this.callee.containsVariableNamed(name) || this.arg.containsVariableNamed(name);
	}

	public containsBoundVariableNamed(name: string): boolean {
		return (
			this.callee.containsBoundVariableNamed(name) ||
			this.arg.containsBoundVariableNamed(name)
		);
	}

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

	public betaReduce(generateNewVariableName: () => string): ILCExpression {
		console.log(`LCFunctionCall.betaReduce() : Unevaluated callee is ${this.callee}`);
		console.log(`LCFunctionCall.betaReduce() : Unevaluated actual parameter is ${this.arg}`);

		// First, evaluate this.callee; if it does not evaluate to a LCLambdaExpression,
		// then return.
		// const evaluatedCallee = this.callee; // Temp hack: No evaluation
		const evaluatedCallee = this.callee.betaReduce(generateNewVariableName);

		console.log(`LCFunctionCall.betaReduce() : evaluatedCallee is ${evaluatedCallee}`);

		if (!isLCLambdaExpression(evaluatedCallee)) {
			const result = new LCFunctionCall(
				evaluatedCallee,
				this.arg.betaReduce(generateNewVariableName)
			);

			console.log(
				`LCFunctionCall.betaReduce() : evaluatedCallee is not an LCLambdaExpression; returning ${result}`
			);

			return result;
		}

		// Next, substitute this.arg in for the arg in the evaluated callee.
		let lambdaExpression = evaluatedCallee as LCLambdaExpression;

		console.log(`The call's callee is: ${lambdaExpression}`);
		console.log(`The call's actual parameter is: ${this.arg}`);

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
		console.log("Names of variables in the call's actual parameter:", argVarNames);

		for (const name of argVarNames) {
			if (lambdaExpression.containsBoundVariableNamed(name)) {
				let generatedVarName: string;

				do {
					generatedVarName = generateNewVariableName();
				} while (lambdaExpression.containsVariableNamed(generatedVarName));

				console.log(`1) Old lambdaExpression: ${lambdaExpression}`);
				console.log(`2) Replacing ${name} with ${generatedVarName}...`);
				lambdaExpression = lambdaExpression.renameBoundVariable(
					generatedVarName,
					name
				) as LCLambdaExpression;
				console.log(`3) New lambdaExpression: ${lambdaExpression}`);
			}
		}

		const lambdaExpressionBody = lambdaExpression.body;

		// console.log('LCFunctionCall.betaReduce() : lambdaExpressionBody is', lambdaExpressionBody);

		const bodyAfterSubst = lambdaExpressionBody.substituteForUnboundVariable(
			lambdaExpression.arg.name,
			this.arg
		);

		console.log(
			`LCFunctionCall.betaReduce() : Replaced ${lambdaExpression.arg.name} with ${this.arg}; bodyAfterSubst is ${bodyAfterSubst}`
		);

		const afterBeta = bodyAfterSubst.betaReduce(generateNewVariableName);

		console.log(`LCFunctionCall.betaReduce() : After beta: ${afterBeta}`);

		return afterBeta;
	}

	public toString(): string {
		return `(${this.callee} ${this.arg})`;
	}

	public getSetOfAllVariableNames(): IImmutableSet<string> {
		return this.callee.getSetOfAllVariableNames().union(this.arg.getSetOfAllVariableNames());
	}
}
