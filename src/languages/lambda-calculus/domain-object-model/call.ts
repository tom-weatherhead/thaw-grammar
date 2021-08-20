// call.ts

import { isLCLambdaExpression, LCLambdaExpression } from './lambda-expression';

import { ILCExpression } from './variable';

export class LCFunctionCall implements ILCExpression {
	constructor(public readonly callee: ILCExpression, public readonly arg: ILCExpression) {}

	public substituteForUnboundVariable(name: string, value: ILCExpression): ILCExpression {
		return new LCFunctionCall(
			this.callee.substituteForUnboundVariable(name, value),
			this.arg.substituteForUnboundVariable(name, value)
		);
	}

	public betaReduce(): ILCExpression {
		// First, evaluate this.callee; if it does not evaluate to a LCLambdaExpression,
		// then return.
		const evaluatedCallee = this.callee; // Temp hack: No evaluation

		console.log('LCFunctionCall.betaReduce() : evaluatedCallee is', evaluatedCallee);

		if (!isLCLambdaExpression(evaluatedCallee)) {
			console.log(
				'LCFunctionCall.betaReduce() : evaluatedCallee is not an LCLambdaExpression; returning.'
			);

			return this;
		}

		// Next, substitute this.arg in for the arg in the evaluated callee.
		const lambdaExpression = evaluatedCallee as LCLambdaExpression;

		// return lambdaExpression.body
		// 	.substituteForUnboundVariable(lambdaExpression.arg.name, this.arg)
		// 	.betaReduce();

		const lambdaExpressionBody = lambdaExpression.body;

		console.log('LCFunctionCall.betaReduce() : lambdaExpressionBody is', lambdaExpressionBody);

		const bodyAfterSubst = lambdaExpressionBody.substituteForUnboundVariable(
			lambdaExpression.arg.name,
			this.arg
		);

		console.log(
			`LCFunctionCall.betaReduce() : Replaced ${lambdaExpression.arg.name} with ${this.arg}; bodyAfterSubst is`,
			bodyAfterSubst
		);

		const afterBeta = bodyAfterSubst.betaReduce();

		console.log('LCFunctionCall.betaReduce() : After beta:', afterBeta);

		return afterBeta;
	}
}
