// prolog-integer-literal.ts

import { IPrologNumber } from './iprolog-number';

export class PrologIntegerLiteral implements IPrologNumber {
	public readonly Value: number;

	constructor(value: number) {
		this.Value = value;
	}

	public toString(): string {
		return `${this.Value}`;
	}

	public Equals(obj: unknown): boolean {
		// if (object.ReferenceEquals(this, obj))
		// {
		//     return true;
		// }

		const otherIntLit = obj as PrologIntegerLiteral;

		return (
			typeof otherIntLit !== 'undefined' &&
			this.Value === otherIntLit.Value
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

		// return null;    // The PrologIntegerLiteral and the IPrologExpression are not unifiable.
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