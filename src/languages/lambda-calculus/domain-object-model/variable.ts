// variable.ts

/* eslint-disable @typescript-eslint/no-empty-interface */

export interface ILCExpression {
	substituteForUnboundVariable(name: string, value: ILCExpression): ILCExpression;
	betaReduce(): ILCExpression;
}

export interface ILCValue extends ILCExpression {}

export class LCVariable implements ILCExpression {
	constructor(public readonly name: string) {
		if (this.name.length !== 1) {
			throw new Error(`LCVariable: Name '${this.name}' is not 1 character long.`);
		}
	}

	public substituteForUnboundVariable(name: string, value: ILCExpression): ILCExpression {
		return name === this.name ? value : this;
	}

	public betaReduce(): ILCExpression {
		return this;
	}
}
