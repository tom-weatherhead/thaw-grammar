// lambda-expression.ts

import { ILCExpression, ILCValue, LCVariable } from './variable';

// 'let x = e1 in e2;' can be expressed as '(λx.e2 e1)'

// TODO: Name it 'LCLambdaExpression' or 'LCFunction' ?

export class LCLambdaExpression implements ILCValue {
	constructor(public readonly arg: LCVariable, public readonly body: ILCExpression) {}
}
