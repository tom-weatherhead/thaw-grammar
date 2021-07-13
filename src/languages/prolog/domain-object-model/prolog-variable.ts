// tom-weatherhead/thaw-grammar/src/languages/prolog/domain-object-model/prolog-variable.ts

import { IEqualityComparable, Set } from 'thaw-common-utilities.ts';

// import { setToArray } from './prolog-global-info';

import { IPrologExpression } from './iprolog-expression';
import { IPrologNumber } from './iprolog-number';
// import { PrologClause } from './prolog-clause';
// import { PrologGoal } from './prolog-goal';
import { PrologSubstitution } from './prolog-substitution';

export class PrologVariable implements IEqualityComparable, IPrologExpression {
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
		const otherVar = obj as PrologVariable;

		// We can compare the Name members with == because Name is a string.
		return (
			typeof otherVar !== 'undefined' &&
			otherVar instanceof PrologVariable &&
			this.Name === otherVar.Name
		);
	}

	public get IsNonBinding(): boolean {
		// The following supports non-binding variables such as _ and _Foo .
		// See http://www.csupomona.edu/~jrfisher/www/prolog_tutorial/2_3.html

		// This may contradict http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlse2 ,
		// which implies that only _ is non-binding, and that any other variable that begins with _ is a normal, binding variable.
		return this.Name.startsWith('_');
	}

	public FindBindingVariables(): Set<PrologVariable> {
		// if (this.IsNonBinding) // This allows the test Prolog2Parser_Fixture.DoNotRenameNonBindingVariablesTest() to pass.
		// {
		//     return [];   // Ignore non-binding variables; we don't want to rename them to binding variables.
		// }

		// return [this];

		// return Set.createFromArray(this.IsNonBinding ? [] : [this]);

		const result = new Set<PrologVariable>();

		if (!this.IsNonBinding) {
			result.add(this);
		}

		return result;
	}

	public GetListOfBindingVariables(): PrologVariable[] {
		return this.FindBindingVariables().toArray();
	}

	public ContainsVariable(v: PrologVariable): boolean {
		return this.equals(v);
	}

	public ApplySubstitution(sub: PrologSubstitution): IPrologExpression {
		const value = sub.SubstitutionList.get(this.Name);

		if (typeof value !== 'undefined') {
			return value;
		}

		return this;
	}

	public Unify(otherExpr: IPrologExpression): PrologSubstitution | undefined {
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
			return new PrologSubstitution();
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
			return new PrologSubstitution(this, otherExpr);
		}
	}

	public get IsGround(): boolean {
		return false;
	}

	public EvaluateToNumber(): IPrologNumber | undefined {
		return undefined;
	}
}
