// tom-weatherhead/thaw-grammar/src/languages/prolog/domain-object-model/prolog-variable.ts

import { createSet, IImmutableSet } from 'thaw-common-utilities.ts';

import { IPrologExpression } from './interfaces/iprolog-expression';
import { IPrologNumber } from './interfaces/iprolog-number';
import { createSubstitution } from './prolog-substitution';

import { ISubstitution } from './interfaces/isubstitution';
import { isIVariable, IVariable, typenamePrologVariable } from './interfaces/ivariable';

// const typenamePrologVariable = 'PrologVariable';

// export function isPrologVariable(obj: unknown): obj is PrologVariable {
// 	const otherPrologVariable = obj as PrologVariable;
//
// 	return (
// 		typeof otherPrologVariable !== 'undefined' &&
// 		otherPrologVariable.typename === typenamePrologVariable
// 	);
// }

class PrologVariable implements /* IEqualityComparable, IPrologExpression, */ IVariable {
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

	public equals(obj: unknown): boolean {
		const otherVar = obj as IVariable;

		// We can compare the Name members with == because Name is a string.
		return (
			// typeof otherVar !== 'undefined' &&
			// otherVar instanceof PrologVariable &&
			isIVariable(otherVar) && this.Name === otherVar.Name
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
	public FindBindingVariables(): IImmutableSet<IVariable> {
		// if (this.IsNonBinding) // This allows the test Prolog2Parser_Fixture.DoNotRenameNonBindingVariablesTest() to pass.
		// {
		//     return [];   // Ignore non-binding variables; we don't want to rename them to binding variables.
		// }

		// return [this];

		// return Set.createFromArray(this.IsNonBinding ? [] : [this]);

		const result = createSet<IVariable>();

		if (!this.IsNonBinding) {
			result.add(this);
		}

		return result;
	}

	public GetListOfBindingVariables(): IVariable[] {
		return this.FindBindingVariables().toArray();
	}

	public ContainsVariable(v: IVariable): boolean {
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
		} else if (
			// [PrologClause.name, PrologGoal.name].indexOf(otherExpr.constructor.name) >= 0 ||
			// [PrologGoal.name].indexOf(otherExpr.constructor.name) >= 0 ||
			otherExpr.ContainsVariable(this)
		) {
			// console.log('PrologVariable.Unify(): Returning undefined');
			// console.log(otherExpr as PrologGoal);
			// console.log(otherExpr as PrologClause);
			// console.log(typeof otherExpr);
			// console.log(getTypeString(otherExpr));
			// console.log(otherExpr.constructor.name);
			// console.log(otherExpr.ContainsVariable(this));

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

export function createVariable(name: string): IVariable {
	return new PrologVariable(name);
}
