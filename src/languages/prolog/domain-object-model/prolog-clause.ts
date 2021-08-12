// prolog-clause.ts

import { IImmutableSet } from 'thaw-common-utilities.ts';

import { PrologGlobalInfo } from './prolog-global-info';
import { PrologGoal } from './prolog-goal';
import { PrologSubstitution } from './prolog-substitution';
// import { PrologVariable } from './prolog-variable';

// import { IPrologExpression } from './interfaces/iprolog-expression';
import { IPrologNumber } from './interfaces/iprolog-number';
import { IVariable } from './interfaces/ivariable';

export class PrologClause /* implements IPrologExpression */ {
	public readonly Lhs: PrologGoal;
	public readonly Rhs: PrologGoal[];

	constructor(lhs: PrologGoal, rhs: PrologGoal[]) {
		this.Lhs = lhs;
		this.Rhs = rhs;
	}

	public toString(): string {
		let tail = '';

		if (this.Rhs.length > 0) {
			tail = ' :- ' + this.Rhs.map((goal: PrologGoal) => goal.toString()).join(', ');
		}

		return this.Lhs.toString() + tail;
	}

	// public override bool Equals(object obj)
	// {

	//     var otherClause = obj as PrologClause;

	//     if (otherClause == null || !Lhs.Equals(otherClause.Lhs) || Rhs.Count != otherClause.Rhs.Count)
	//     {
	//         return false;
	//     }

	//     for (var i = 0; i < Rhs.Count; ++i)
	//     {

	//         if (!Rhs[i].Equals(otherClause.Rhs[i]))
	//         {
	//             return false;
	//         }
	//     }

	//     return true;
	// }

	public FindBindingVariables(): IImmutableSet<IVariable> {
		let result = this.Lhs.FindBindingVariables();

		for (const subgoal of this.Rhs) {
			result = result.union(subgoal.FindBindingVariables());
		}

		return result;
	}

	public GetListOfBindingVariables(): IVariable[] {
		return this.FindBindingVariables().toArray();
	}

	public ContainsVariable(v: IVariable): boolean {
		return (
			this.ContainsVariable(v) ||
			this.Rhs.some((goal: PrologGoal) => goal.ContainsVariable(v))
		);
	}

	public ApplySubstitution(substitution: PrologSubstitution): PrologClause {
		return new PrologClause(
			this.Lhs.ApplySubstitution(substitution) as PrologGoal,
			this.Rhs.map(
				(subgoal: PrologGoal) => subgoal.ApplySubstitution(substitution) as PrologGoal
			)
		);
	}

	private isVariableInArrayOfVariables(v: IVariable, a: IVariable[]): boolean {
		return typeof a.find((vv: IVariable) => vv.Name === v.Name) !== 'undefined';
	}

	public RenameVariables(
		variablesToAvoid: IImmutableSet<IVariable>,
		globalInfo: PrologGlobalInfo
	): PrologClause {
		const oldVariables = this.FindBindingVariables();
		const substitution = new PrologSubstitution();
		const arrayOfOldVariables = oldVariables.toArray();
		const arrayOfVariablesToAvoid = variablesToAvoid.toArray();

		// for (const oldVariable of oldVariables.getIterator()) {
		for (const oldVariable of oldVariables) {
			// if (!variablesToAvoid.contains(oldVariable)) {
			if (!this.isVariableInArrayOfVariables(oldVariable, arrayOfVariablesToAvoid)) {
				continue;
			}

			let newVariable: IVariable;

			do {
				newVariable = globalInfo.GetNextUniqueVariable();
				// console.log(
				// 	`Clause.RenameVariables() : Name of new uniqueVariable: '${newVariable.Name}'`
				// );
			} while (
				// oldVariables.contains(newVariable) ||
				// variablesToAvoid.contains(newVariable)
				this.isVariableInArrayOfVariables(newVariable, arrayOfOldVariables) ||
				this.isVariableInArrayOfVariables(newVariable, arrayOfVariablesToAvoid)
			);

			substitution.SubstitutionList.set(oldVariable.Name, newVariable); // This is safe because all of the oldVariables and newVariables are unique.
			//substitution.SubstitutionList[oldVariable] = globalInfo.GetNextUniqueVariable();    // This would probably work too.
		}

		return this.ApplySubstitution(substitution) as PrologClause;
	}

	public Unify(otherExpr: PrologClause): PrologSubstitution | undefined {
		const otherClause = otherExpr as PrologClause;

		if (typeof otherClause === 'undefined' || this.Rhs.length !== otherClause.Rhs.length) {
			return undefined;
		}

		let substitution = this.Lhs.Unify(otherClause.Lhs);

		if (typeof substitution === 'undefined') {
			return undefined;
		}

		for (let i = 0; i < this.Rhs.length; ++i) {
			const newGoal1 = this.Rhs[i].ApplySubstitution(substitution);
			const newGoal2 = otherClause.Rhs[i].ApplySubstitution(substitution);
			const substitution2 = newGoal1.Unify(newGoal2);

			if (typeof substitution2 === 'undefined') {
				return undefined;
			}

			substitution = substitution.compose(substitution2);
		}

		return substitution;
	}

	// public IsIsomorphicTo(
	// 	otherClause: PrologClause,
	// 	variablesToAvoid: Set<PrologVariable> | undefined,
	// 	globalInfo: PrologGlobalInfo
	// ): boolean {
	// 	if (typeof variablesToAvoid === 'undefined') {
	// 		variablesToAvoid = this.FindBindingVariables();
	// 	}

	// 	otherClause = otherClause.RenameVariables(variablesToAvoid, globalInfo);

	// 	const unifier = this.Unify(otherClause);

	// 	return typeof unifier !== 'undefined' && unifier.IsOneToOne;
	// }

	public get IsGround(): boolean {
		return this.Lhs.IsGround && this.Rhs.every((goal: PrologGoal) => goal.IsGround);
	}

	public EvaluateToNumber(): IPrologNumber | undefined {
		return undefined;
	}
}
