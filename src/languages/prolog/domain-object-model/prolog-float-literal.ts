// prolog-float-literal.ts

import { createSet, IImmutableSet } from 'thaw-common-utilities.ts';

import { IPrologExpression } from './interfaces/iprolog-expression';
import { IPrologNumber } from './interfaces/iprolog-number';
import { createSubstitution } from './prolog-substitution';

import { ISubstitution } from './interfaces/isubstitution';
import { IPrologVariable, isIPrologVariable } from './interfaces/ivariable';

const typenamePrologFloatLiteral = 'PrologFloatLiteral';

export class PrologFloatLiteral implements IPrologNumber {
	public readonly typename: string = typenamePrologFloatLiteral;
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

	public equals(other: unknown): boolean {
		const otherFloatLit = other as PrologFloatLiteral;

		return (
			typeof otherFloatLit !== 'undefined' &&
			otherFloatLit instanceof PrologFloatLiteral &&
			this.Value === otherFloatLit.Value
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

		return undefined; // The PrologFloatLiteral and the IPrologExpression are not unifiable.
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
