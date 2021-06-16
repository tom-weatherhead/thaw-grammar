// prolog-float-literal.ts

import { Set } from 'thaw-common-utilities.ts';

import { IPrologExpression } from './iprolog-expression';
import { IPrologNumber } from './iprolog-number';
import { PrologSubstitution } from './prolog-substitution';
import { PrologVariable } from './prolog-variable';

export class PrologFloatLiteral implements IPrologNumber {
	public readonly Value: number;

	constructor(value: number) {
		this.Value = value;
	}

	public toString(): string {
		//     // E.g. If d == 13.0, we want the string to be "13.0" rather than just "13", so that we can distinguish it from an integer.
		//     // Note from Lisp: d == (tan (/ pi 4)) is a good test case; it should yield "1.0", not "1".
		//     var result = Value.ToString();

		//     if (result.IndexOf('.') < 0 && result.IndexOf('E') < 0) // I.e. If result does not contain either '.' or 'E'.
		//     {
		//         result = result + ".0";
		//     }

		//     return result;

		return `${this.Value}`;
	}

	public Equals(obj: unknown): boolean {
		// if (object.ReferenceEquals(this, obj))
		// {
		//     return true;
		// }

		const otherFloatLit = obj as PrologFloatLiteral;

		return (
			typeof otherFloatLit !== 'undefined' &&
			this.Value === otherFloatLit.Value
		);
	}

	// public override int GetHashCode()
	// {
	//     return Value.GetHashCode();
	// }

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
		if (this.Equals(otherExpr)) {
			// Do not use "if (this == otherExpr)", which just compares references.
			return new PrologSubstitution();
		} else if (otherExpr.constructor.name === PrologVariable.name) {
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
		// return Convert.ToInt32(Math.Floor(Value));
		return Math.floor(this.Value);
	}

	public ToDouble(): number {
		return this.Value;
	}
}
