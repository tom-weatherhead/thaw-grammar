// variable.ts

/* eslint-disable @typescript-eslint/no-empty-interface */

export interface ILCExpression {}
export interface ILCValue extends ILCExpression {}
export class LCVariable implements ILCExpression {
	constructor(public readonly name: string) {}
}
