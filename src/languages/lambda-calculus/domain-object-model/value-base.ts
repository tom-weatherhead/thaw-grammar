// tom-weatherhead/thaw-grammar/src/languages/lambda-calculus/domain-object-model/value-base.ts

// Rename this file to expression-base.ts ?

import { createSet, IImmutableSet } from 'thaw-common-utilities.ts';

import { areIsomorphic } from '../utilities';

import {
	// BetaReductionStrategy,
	ILCBetaReductionOptions,
	ILCExpression,
	ISubstitution,
	IUnifiable
} from './interfaces/expression';

export abstract class LCValueBase implements ILCExpression {
	constructor(public readonly typename: string) {}

	public abstract toString(): string;

	/* eslint-disable @typescript-eslint/no-unused-vars */

	public equals(obj: unknown): boolean {
		return false;
	}

	public applySubstitution(substitution: ISubstitution<ILCExpression>): ILCExpression {
		return this;
	}

	public abstract unify(
		other: IUnifiable<ILCExpression>,
		variablesInOriginalExpr1Param?: IImmutableSet<string>,
		variablesInOriginalExpr2Param?: IImmutableSet<string>
	): ISubstitution<ILCExpression> | undefined;

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

	public substituteForUnboundVariable(name: string, value: ILCExpression): ILCExpression {
		return this;
	}

	public getSetOfAllVariableNames(): IImmutableSet<string> {
		return createSet<string>();
	}

	public renameBoundVariable(newName: string, oldName: string): ILCExpression {
		// α-conversion

		return this;
	}

	public isBetaReducible(): boolean {
		return false;
	}

	/* eslint-disable @typescript-eslint/no-unused-vars */

	public betaReduce(options: ILCBetaReductionOptions = {}): ILCExpression {
		return this;
	}

	public deltaReduce(): ILCExpression {
		return this;
	}

	public etaReduce(): ILCExpression {
		return this;
	}

	public isIsomorphicTo(other: IUnifiable<ILCExpression>): boolean {
		return areIsomorphic(this, other);
	}
}
