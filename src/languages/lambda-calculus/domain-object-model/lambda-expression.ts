// tom-weatherhead/thaw-grammar/src/languages/lambda-calculus/domain-object-model/lambda-expression.ts

import { createSet, IImmutableSet } from 'thaw-common-utilities.ts';

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

	public containsVariableNamed(name: string): boolean {
		return this.arg.containsVariableNamed(name) || this.body.containsVariableNamed(name);
	}

	public containsBoundVariableNamed(name: string): boolean {
		return this.arg.name === name || this.body.containsBoundVariableNamed(name);
	}

	public renameBoundVariable(newName: string, oldName: string): ILCExpression {
		if (this.arg.name === oldName) {
			const newVariable = new LCVariable(newName);

			return new LCLambdaExpression(
				newVariable,
				this.body.substituteForUnboundVariable(oldName, newVariable)
			);
		} else {
			return new LCLambdaExpression(
				this.arg,
				this.body.renameBoundVariable(newName, oldName)
			);
		}
	}

	public substituteForUnboundVariable(name: string, value: ILCExpression): ILCExpression {
		return name === this.arg.name
			? this
			: new LCLambdaExpression(this.arg, this.body.substituteForUnboundVariable(name, value));
	}

	public betaReduce(generateNewVariableName: () => string): ILCExpression {
		return new LCLambdaExpression(this.arg, this.body.betaReduce(generateNewVariableName));
	}

	public toString(): string {
		return `λ${this.arg}.${this.body}`;
	}

	public getSetOfAllVariableNames(): IImmutableSet<string> {
		return createSet([this.arg.name]).union(this.body.getSetOfAllVariableNames());
	}
}
