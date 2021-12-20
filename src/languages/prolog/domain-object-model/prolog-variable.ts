// tom-weatherhead/thaw-grammar/src/languages/prolog/domain-object-model/prolog-variable.ts

import { createSet, IImmutableSet } from 'thaw-common-utilities.ts';

import { IPrologExpression } from './interfaces/iprolog-expression';
import { IPrologNumber } from './interfaces/iprolog-number';
import { createSubstitution } from './prolog-substitution';

import { ISubstitution } from './interfaces/isubstitution';
import { isIPrologVariable, IPrologVariable, typenamePrologVariable } from './interfaces/ivariable';

class PrologVariable implements IPrologVariable {
	public readonly typename: string = typenamePrologVariable;
	public readonly Name: string;

	constructor(name: string) {
		if (!name) {
			throw new Error('A PrologVariable cannot have a null or empty name');
		}

		this.Name = name;
	}

	public toString(): string {
		return this.Name;
	}

	public equals(other: unknown): boolean {
		// const otherVar = other as IPrologVariable;

		// We can compare the Name members with == because Name is a string.
		return (
			// typeof otherVar !== 'undefined' &&
			// otherVar instanceof PrologVariable &&
			isIPrologVariable(other) && this.Name === other.Name
		);
	}

	public get IsNonBinding(): boolean {
		// The following supports non-binding variables such as _ and _Foo .
		// See http://www.csupomona.edu/~jrfisher/www/prolog_tutorial/2_3.html

		// This may contradict http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlse2 ,
		// which implies that only _ is non-binding, and that any other variable that begins with _ is a normal, binding variable.
		return this.Name.startsWith('_');
	}

	// public FindBindingVariables(): Set<PrologVariable> {
	public FindBindingVariables(): IImmutableSet<IPrologVariable> {
		// if (this.IsNonBinding) // This allows the test Prolog2Parser_Fixture.DoNotRenameNonBindingVariablesTest() to pass.
		// {
		//     return [];   // Ignore non-binding variables; we don't want to rename them to binding variables.
		// }

		// return [this];

		// return Set.createFromArray(this.IsNonBinding ? [] : [this]);

		const result = createSet<IPrologVariable>();

		if (!this.IsNonBinding) {
			result.add(this);
		}

		return result;
	}

	public GetListOfBindingVariables(): IPrologVariable[] {
		return this.FindBindingVariables().toArray();
	}

	public ContainsVariable(v: IPrologVariable): boolean {
		return this.equals(v);
	}

	public ApplySubstitution(sub: ISubstitution): IPrologExpression {
		const value = sub.SubstitutionList.get(this.Name);

		if (typeof value !== 'undefined') {
			return value;
		}

		return this;
	}

	public Unify(otherExpr: IPrologExpression): ISubstitution | undefined {
		// console.log('PrologVariable.Unify():');
		// console.log('  this is', typeof this, this);
		// console.log('  otherExpr is', typeof otherExpr, otherExpr);

		const otherVariable = otherExpr as PrologVariable;

		if (
			this.equals(otherExpr) ||
			this.IsNonBinding ||
			// 2014/03/13 : Don't add the binding { X = _ } to any substitution.
			// But what about a binding such as { X = foo(_) } ?
			(typeof otherVariable !== 'undefined' && otherVariable.IsNonBinding)
		) {
			return createSubstitution();
		} else if (otherExpr.ContainsVariable(this)) {
			// console.log('PrologVariable.Unify(): Returning undefined');

			// This is the "occurs" check.
			return undefined; // This PrologVariable and the IPrologExpression are not unifiable.
		} else {
			return createSubstitution(this.Name, otherExpr);
		}
	}

	public get IsGround(): boolean {
		return false;
	}

	public EvaluateToNumber(): IPrologNumber | undefined {
		return undefined;
	}
}

export function createPrologVariable(name: string): IPrologVariable {
	return new PrologVariable(name);
}
