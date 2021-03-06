// prolog-integer-literal.ts

import { IEqualityComparable, Set } from 'thaw-common-utilities.ts';

import { IPrologExpression } from './iprolog-expression';
import { IPrologNumber } from './iprolog-number';
import { PrologSubstitution } from './prolog-substitution';
import { PrologVariable } from './prolog-variable';

export class PrologIntegerLiteral implements IEqualityComparable, IPrologNumber {
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

	public FindBindingVariables(): Set<PrologVariable> {
		return new Set<PrologVariable>();
	}

	public GetListOfBindingVariables(): PrologVariable[] {
		return [];
	}

	// eslint-disable-next-line @typescript-eslint/no-unused-vars
	public ContainsVariable(v: PrologVariable): boolean {
		return false;
	}

	public ApplySubstitution(
		// eslint-disable-next-line @typescript-eslint/no-unused-vars
		substitution: PrologSubstitution
	): IPrologExpression {
		return this;
	}

	public Unify(otherExpr: IPrologExpression): PrologSubstitution | undefined {
		if (this.equals(otherExpr)) {
			// Do not use "if (this == otherExpr)", which just compares references.
			return new PrologSubstitution();
			// } else if (otherExpr.constructor.name === PrologVariable.name) {
		} else if (otherExpr instanceof PrologVariable) {
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
