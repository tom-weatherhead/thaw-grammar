// tom-weatherhead/thaw-grammar/src/languages/lambda-calculus/domain-object-model/variable.ts

// ISet
import { createSet, IImmutableSet, IStringifiable } from 'thaw-common-utilities.ts';

export interface ILCExpression extends IStringifiable {
	containsVariableNamed(name: string): boolean;
	containsBoundVariableNamed(name: string): boolean;
	// containsUnboundVariableNamed(name: string): boolean;
	renameBoundVariable(newName: string, oldName: string): ILCExpression;
	substituteForUnboundVariable(name: string, value: ILCExpression): ILCExpression;
	betaReduce(generateNewVariableName: () => string): ILCExpression;
	getSetOfAllVariableNames(): IImmutableSet<string>;
}

/* eslint-disable @typescript-eslint/no-empty-interface */
export interface ILCValue extends ILCExpression {}

const typenameLCVariable = 'LCVariable';

export function isLCVariable(obj: unknown): obj is LCVariable {
	const otherLCVariable = obj as LCVariable;

	return (
		typeof otherLCVariable !== 'undefined' && otherLCVariable.typename === typenameLCVariable
	);
}

export class LCVariable implements ILCExpression {
	public readonly typename = typenameLCVariable;

	constructor(public readonly name: string) {
		// if (this.name.length !== 1) {
		// 	throw new Error(`LCVariable: Name '${this.name}' is not 1 character long.`);
		// }
	}

	public containsVariableNamed(name: string): boolean {
		return name === this.name;
	}

	public containsBoundVariableNamed(name: string): boolean {
		return false;
	}

	public renameBoundVariable(newName: string, oldName: string): ILCExpression {
		return this;
	}

	public substituteForUnboundVariable(name: string, value: ILCExpression): ILCExpression {
		return name === this.name ? value : this;
	}

	public betaReduce(generateNewVariableName: () => string): ILCExpression {
		return this;
	}

	public toString(): string {
		return this.name;
	}

	public getSetOfAllVariableNames(): IImmutableSet<string> {
		return createSet([this.name]);
	}
}
