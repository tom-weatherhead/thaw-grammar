// prolog-clause.ts

import { IPrologExpression } from './iprolog-expression';
import { PrologGoal } from './prolog-goal';
import { PrologSubstitution } from './prolog-substitution';
import { PrologVariable } from './prolog-variable';

export class PrologClause implements IPrologExpression {
	public readonly Lhs: PrologGoal;
	public readonly Rhs: PrologGoal[];

	constructor(lhs: PrologGoal, rhs: PrologGoal[]) {
		this.Lhs = lhs;
		this.Rhs = rhs;
	}

	public toString(): string {
		return 'PrologClause.toString()';
	}

	// public override string ToString()
	// {
	//     //return string.Format("{0} <= {1}", Lhs, string.Join(" ", Rhs));

	//     // Prolog2 clause format:

	//     if (Rhs.Count == 0)
	//     {
	//         return string.Format("{0}.", Lhs);
	//     }
	//     else
	//     {
	//         return string.Format("{0} :- {1}.", Lhs, string.Join(", ", Rhs));
	//     }
	// }

	// public override bool Equals(object obj)
	// {

	//     if (object.ReferenceEquals(this, obj))
	//     {
	//         return true;
	//     }

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

	// public override int GetHashCode()
	// {
	//     return Rhs
	//         .Select(subgoal => subgoal.GetHashCode())
	//         .Aggregate(Lhs.GetHashCode(), (accumulator, hashCode) => accumulator * 101 + hashCode);
	// }

	public FindBindingVariables(): Set<PrologVariable> {
		const result = this.Lhs.FindBindingVariables();

		for (const subgoal of this.Rhs) {
			result.unionInPlace(subgoal.FindBindingVariables());
		}

		return result;
	}

	public GetListOfBindingVariables(): PrologVariable[] {
		// var result = Lhs.GetListOfBindingVariables();

		// foreach (var subgoal in Rhs)
		// {
		//     result.AddRangeUnique(subgoal.GetListOfBindingVariables());
		// }

		// return result;

		// return this.FindBindingVariables();
		return setToArray(this.FindBindingVariables());
	}

	public ContainsVariable(v: PrologVariable): boolean {
		return (
			this.ContainsVariable(v) ||
			this.Rhs.some((goal: PrologGoal) => goal.ContainsVariable(v))
		);
	}

	public ApplySubstitution(
		substitution: PrologSubstitution
	): IPrologExpression {
		return new PrologClause(
			this.Lhs.ApplySubstitution(substitution) as PrologGoal,
			this.Rhs.map(
				(subgoal: PrologGoal) =>
					subgoal.ApplySubstitution(substitution) as PrologGoal
			)
		);
	}

	public RenameVariables(
		variablesToAvoid: Set<PrologVariable>,
		globalInfo: PrologGlobalInfo
	): PrologClause {
		const oldVariables = this.FindBindingVariables();
		const substitution = new PrologSubstitution();

		// for (const oldVariable of oldVariables.getIterator()) {
		for (const oldVariable of setToArray(oldVariables)) {
			if (!variablesToAvoid.contains(oldVariable)) {
				continue;
			}

			let newVariable: PrologVariable;

			do {
				newVariable = globalInfo.GetNextUniqueVariable();
			} while (
				oldVariables.contains(newVariable) ||
				variablesToAvoid.contains(newVariable)
			);

			substitution.SubstitutionList.set(oldVariable.Name, newVariable); // This is safe because all of the oldVariables and newVariables are unique.
			//substitution.SubstitutionList[oldVariable] = globalInfo.GetNextUniqueVariable();    // This would probably work too.
		}

		return this.ApplySubstitution(substitution) as PrologClause;
	}

	public Unify(otherExpr: IPrologExpression): PrologSubstitution | undefined {
		const otherClause = otherExpr as PrologClause;

		if (
			typeof otherClause === 'undefined' ||
			this.Rhs.length !== otherClause.Rhs.length
		) {
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

			substitution = substitution.Compose(substitution2);
		}

		return substitution;
	}

	public IsIsomorphicTo(
		otherClause: PrologClause,
		variablesToAvoid: Set<PrologVariable> | undefined,
		globalInfo: PrologGlobalInfo
	): boolean {
		if (typeof variablesToAvoid === 'undefined') {
			variablesToAvoid = this.FindBindingVariables();
		}

		otherClause = otherClause.RenameVariables(variablesToAvoid, globalInfo);

		const unifier = this.Unify(otherClause);

		return typeof unifier !== 'undefined' && unifier.IsOneToOne;
	}

	public get IsGround(): boolean {
		return (
			this.Lhs.IsGround &&
			this.Rhs.every((goal: PrologGoal) => goal.IsGround)
		);
	}

	public EvaluateToNumber(): IPrologNumber | undefined {
		return undefined;
	}
}
