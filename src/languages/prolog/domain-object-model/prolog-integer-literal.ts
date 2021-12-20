// prolog-integer-literal.ts

import { createSet, IImmutableSet } from 'thaw-common-utilities.ts';

import { IPrologExpression } from './interfaces/iprolog-expression';
import { IPrologNumber } from './interfaces/iprolog-number';
import { createSubstitution } from './prolog-substitution';

import { ISubstitution } from './interfaces/isubstitution';
import { IPrologVariable, isIPrologVariable } from './interfaces/ivariable';

const typenamePrologIntegerLiteral = 'PrologIntegerLiteral';

export function isPrologIntegerLiteral(obj: unknown): obj is PrologIntegerLiteral {
	const intlit = obj as PrologIntegerLiteral;

	return typeof intlit !== 'undefined' && intlit.typename === typenamePrologIntegerLiteral;
}

export class PrologIntegerLiteral implements IPrologNumber {
	public readonly typename: string = typenamePrologIntegerLiteral;
	public readonly Value: number;

	constructor(value: number) {
		this.Value = value;
	}

	public toString(): string {
		return `${this.Value}`;
	}

	public equals(other: unknown): boolean {
		const otherIntLit = other as PrologIntegerLiteral;

		return (
			typeof otherIntLit !== 'undefined' &&
			other instanceof PrologIntegerLiteral &&
			this.Value === otherIntLit.Value
		);
	}

	public FindBindingVariables(): IImmutableSet<IPrologVariable> {
		return createSet<IPrologVariable>();
	}

	public GetListOfBindingVariables(): IPrologVariable[] {
		return [];
	}

	// eslint-disable-next-line @typescript-eslint/no-unused-vars
	public ContainsVariable(v: IPrologVariable): boolean {
		return false;
	}

	public ApplySubstitution(
		// eslint-disable-next-line @typescript-eslint/no-unused-vars
		substitution: ISubstitution
	): IPrologExpression {
		return this;
	}

	public Unify(otherExpr: IPrologExpression): ISubstitution | undefined {
		if (this.equals(otherExpr)) {
			// Do not use "if (this == otherExpr)", which just compares references.
			return createSubstitution();
		} else if (isIPrologVariable(otherExpr)) {
			return otherExpr.Unify(this);
		}

		return undefined; // The PrologIntegerLiteral and the IPrologExpression are not unifiable.
	}

	public get IsGround(): boolean {
		return true;
	}

	public EvaluateToNumber(): IPrologNumber | undefined {
		return this;
	}

	public ToInteger(): number {
		return this.Value;
	}

	public ToDouble(): number {
		// return Convert.ToDouble(Value);
		return this.Value;
	}
}
