// tom-weatherhead/thaw-grammar/src/languages/lambda-calculus/domain-object-model/value-base.ts

import { createSet, IImmutableSet } from 'thaw-common-utilities.ts';

import { ArgumentException } from '../../../common/exceptions/argument-exception';

import { ILCExpression, ISubstitution, IUnifiable } from './interfaces/expression';

// const typenameIntegerLiteral = 'IntegerLiteral';
//
// export function isIntegerLiteral(obj: unknown): obj is IntegerLiteral {
// 	const otherIntegerLiteral = obj as IntegerLiteral;
//
// 	return (
// 		typeof otherIntegerLiteral !== 'undefined' &&
// 		otherIntegerLiteral.typename === typenameIntegerLiteral
// 	);
// }

export abstract class LCValueBase implements ILCExpression {
	// public readonly typename = typenameIntegerLiteral;
	// public readonly value: number;

	constructor(public readonly typename: string) {}

	public abstract toString(): string;

	public applySubstitution(substitution: ISubstitution<ILCExpression>): ILCExpression {
		return this;
	}

	public unify(other: IUnifiable<ILCExpression>): ISubstitution<ILCExpression> | undefined {
		return undefined; // TODO FIXME
	}

	public isIsomorphicTo(other: IUnifiable<ILCExpression>): boolean {
		return false; // TODO FIXME
	}

	public containsVariableNamed(name: string): boolean {
		return false;
	}

	public containsBoundVariableNamed(name: string): boolean {
		return false;
	}

	public containsUnboundVariableNamed(
		name: string,
		boundVariableNames: IImmutableSet<string>
	): boolean {
		return false;
	}

	public renameBoundVariable(newName: string, oldName: string): ILCExpression {
		// Alpha-conversion

		return this;
	}

	public substituteForUnboundVariable(name: string, value: ILCExpression): ILCExpression {
		return this;
	}

	public getSetOfAllVariableNames(): IImmutableSet<string> {
		return createSet<string>();
	}

	public betaReduce(generateNewVariableName: () => string): ILCExpression {
		return this;
	}

	public deltaReduce(): ILCExpression {
		return this;
	}

	public etaReduce(): ILCExpression {
		return this;
	}
}
