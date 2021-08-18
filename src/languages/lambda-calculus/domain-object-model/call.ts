// call.ts

import { ILCExpression } from './variable';

export class LCFunctionCall implements ILCExpression {
	constructor(public readonly callee: ILCExpression, public readonly arg: ILCExpression) {}
}
