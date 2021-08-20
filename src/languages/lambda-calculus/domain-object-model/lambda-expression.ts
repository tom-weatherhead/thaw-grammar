// lambda-expression.ts

import { ILCExpression, ILCValue, LCVariable } from './variable';

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
	public readonly typename = typenameLCLambdaExpression;

	constructor(public readonly arg: LCVariable, public readonly body: ILCExpression) {}

	public substituteForUnboundVariable(name: string, value: ILCExpression): ILCExpression {
		return name === this.arg.name
			? this
			: new LCLambdaExpression(this.arg, this.body.substituteForUnboundVariable(name, value));
	}

	public betaReduce(): ILCExpression {
		return new LCLambdaExpression(this.arg, this.body.betaReduce());
	}
}
