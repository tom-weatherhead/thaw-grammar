// prolog-name-expression.ts

import { LanguageSelector } from 'thaw-lexical-analyzer';

import { IPrologExpression } from './iprolog-expression';
import { IPrologNumber } from './iprolog-number';
import { PrologFloatLiteral } from './prolog-float-literal';
import { PrologFunctor } from './prolog-functor';
import { PrologGoal } from './prolog-goal';
import { PrologIntegerLiteral } from './prolog-integer-literal';
import { PrologNameBase } from './prolog-name-base';
import { PrologPredicate } from './prolog-predicate';
import { PrologSubstitution } from './prolog-substitution';
import { PrologVariable } from './prolog-variable';

export class PrologNameExpression<T extends PrologNameBase>
	implements IPrologExpression
{
	public readonly gs: LanguageSelector;
	public readonly Name: T;
	public readonly ExpressionList: IPrologExpression[] = [];
	public DCGDoNotAddExtraArguments = false; // Part of Definite Clause Grammar support.

	constructor(
		gs: LanguageSelector,
		name: T,
		expressionList: IPrologExpression[] | undefined = undefined
	) {
		this.gs = gs;
		this.Name = name;

		if (typeof expressionList !== 'undefined') {
			this.ExpressionList = expressionList;
		}
	}

	private ListToString(expr: IPrologExpression): string {
		const functorExpression = expr as PrologNameExpression<PrologFunctor>;

		if (
			typeof functorExpression !== 'undefined' &&
			functorExpression.Name.Name === '[]' &&
			functorExpression.ExpressionList.length === 0
		) {
			return '';
		} else if (
			typeof functorExpression !== 'undefined' &&
			functorExpression.Name.Name === '.' &&
			functorExpression.ExpressionList.length === 2
		) {
			return `, ${functorExpression.ExpressionList[0]}${this.ListToString(
				functorExpression.ExpressionList[1]
			)}`;
		} else {
			return ` | ${expr}`;
		}
	}

	private SequenceToString(expr: IPrologExpression): string {
		const functorExpression = expr as PrologNameExpression<PrologFunctor>;

		if (
			typeof functorExpression !== 'undefined' &&
			functorExpression.Name.Name === 'consSeq' &&
			functorExpression.ExpressionList.length === 2
		) {
			return `${
				functorExpression.ExpressionList[0]
			}, ${this.SequenceToString(functorExpression.ExpressionList[1])}`;
		} else {
			return `${expr}`;
		}
	}

	public toString(): string {
		// const nameAsString = this.Name.toString();
		// const isProlog2FunctorExpression = this.gs === LanguageSelector.Prolog2 && this.Name.constructor.name === PrologFunctor.name;

		// if (ExpressionList.Count == 0)
		// {
		// // #if DEAD_CODE
		// // if (isProlog2FunctorExpression && nameAsString == "nil")
		// // {
		// // 	return "[]";
		// // }
		// // #endif
		// return nameAsString;
		// }
		// else if (gs == LanguageSelector.Prolog)
		// {
		// return string.Format("({0} {1})", Name, string.Join(" ", ExpressionList.Select(expr => expr.ToString())));
		// }
		// else
		// {

		// if (isProlog2FunctorExpression && ExpressionList.Count == 2)
		// {

		// if (nameAsString == ".")
		// {
		// return string.Format("[{0}{1}]", ExpressionList[0], ListToString(ExpressionList[1]));
		// }
		// else if (nameAsString == "consSeq")
		// {
		// // #if DEAD_CODE
		// // return string.Format("{0}, {1}", ExpressionList[0], SequenceToString(ExpressionList[1]));
		// // #else
		// // ThAW 2014/03/28 : I added the brackets here because without them, ?- X = [(1, 2), (3, 4)], print(X). yielded [1, 2, 3, 4],
		// // which was misleading.
		// return string.Format("({0}, {1})", ExpressionList[0], SequenceToString(ExpressionList[1]));
		// // #endif
		// }
		// }

		// return string.Format("{0}({1})", Name, string.Join(", ", ExpressionList.Select(expr => expr.ToString())));
		// }

		return `${this.Name}(${this.ExpressionList.map(
			(expr: IPrologExpression) => expr.toString()
		).join(', ')})`;
	}

	//     public override bool Equals(object obj)
	//     {

	//         if (object.ReferenceEquals(this, obj))
	//         {
	//             return true;
	//         }

	// #if NAME_EXPRESSION_EQUALITY
	//         var otherName = obj as T;

	//         if (otherName != null)
	//         {
	//             return ExpressionList.Count == 0 && Name.Equals(otherName);
	//         }
	// #endif

	//         var otherNameExpr = obj as PrologNameExpression<T>;

	//         if (otherNameExpr == null || !Name.Equals(otherNameExpr.Name) || ExpressionList.Count != otherNameExpr.ExpressionList.Count)
	//         {
	//             return false;
	//         }

	//         for (var i = 0; i < ExpressionList.Count; ++i)
	//         {

	//             if (!ExpressionList[i].Equals(otherNameExpr.ExpressionList[i]))
	//             {
	//                 return false;
	//             }
	//         }

	//         return true;
	//     }

	//     public override int GetHashCode()
	//     {
	//         return ExpressionList
	//             .Select(expr => expr.GetHashCode())
	//             .Aggregate(Name.GetHashCode(), (accumulator, hashCode) => accumulator * 101 + hashCode);
	//     }

	public FindBindingVariables(): Set<PrologVariable> {
		const result = new Set<PrologVariable>();

		// foreach (var expr in ExpressionList)
		for (const expr of this.ExpressionList) {
			// for (const v of expr.FindBindingVariables()) {

			// 	if (!result.find(v2 => v2.Name === v.Name)) {
			// 		result.add(v);
			// 	}
			// }

			result.unionInPlace(expr.FindBindingVariables());
		}

		return result;
	}

	public GetListOfBindingVariables(): PrologVariable[] {
		// var result = new List<PrologVariable>();

		// foreach (var expr in ExpressionList)
		// {
		//     result.AddRangeUnique(expr.GetListOfBindingVariables());
		// }

		// return result;

		return setToArray(this.FindBindingVariables());
	}

	public ContainsVariable(v: PrologVariable): boolean {
		return this.ExpressionList.some((expr: IPrologExpression) =>
			expr.ContainsVariable(v)
		);
	}

	public ApplySubstitution(
		substitution: PrologSubstitution
	): IPrologExpression {
		return new PrologNameExpression<T>(
			this.gs,
			this.Name,
			this.ExpressionList.map((expr: IPrologExpression) =>
				expr.ApplySubstitution(substitution)
			)
		);
	}

	public Unify(otherExpr: IPrologExpression): PrologSubstitution | undefined {
		if (otherExpr.constructor.name === PrologVariable.name) {
			return otherExpr.Unify(this);
		}

		// if (!GetType().Equals(otherExpr.GetType()))
		// {
		//     // A PrologFunctorExpression can unify with a PrologFunctorExpression;
		//     // a PrologGoal can unify with a PrologGoal,
		//     // but a PrologFunctorExpression cannot unify with a PrologGoal.
		//     return null;
		// }

		const otherNameExpression = otherExpr as PrologNameExpression<T>;

		if (
			!this.Name.Equals(otherNameExpression.Name) ||
			this.ExpressionList.length !=
				otherNameExpression.ExpressionList.length
		) {
			return undefined;
		}

		let substitution = new PrologSubstitution();

		for (let i = 0; i < this.ExpressionList.length; ++i) {
			const newExpr1 =
				this.ExpressionList[i].ApplySubstitution(substitution);
			const newExpr2 =
				otherNameExpression.ExpressionList[i].ApplySubstitution(
					substitution
				);
			const substitution2 = newExpr1.Unify(newExpr2);

			if (typeof substitution2 === 'undefined') {
				return undefined;
			}

			substitution = substitution.Compose(substitution2);
		}

		return substitution;
	}

	public get IsGround(): boolean {
		return this.ExpressionList.every(
			(expr: IPrologExpression) => expr.IsGround
		);
	}

	private static EvaluateUnaryOperatorToNumber(
		thisFunctorExpression: PrologNameExpression<PrologFunctor>
	): IPrologNumber | undefined {
		const arg1Evaluated =
			thisFunctorExpression.ExpressionList[0].EvaluateToNumber();

		if (typeof arg1Evaluated === 'undefined') {
			return undefined;
		}

		if (arg1Evaluated.constructor.name === PrologIntegerLiteral.name) {
			const arg1Value = arg1Evaluated.ToInteger();
			let result: number;

			switch (thisFunctorExpression.Name.Name) {
				case '+':
					result = arg1Value;
					break;

				case '-':
					result = -arg1Value;
					break;

				default:
					return undefined;
			}

			return new PrologIntegerLiteral(result);
		} else {
			const arg1Value = arg1Evaluated.ToDouble();
			let result: number;

			switch (thisFunctorExpression.Name.Name) {
				case '+':
					result = arg1Value;
					break;

				case '-':
					result = -arg1Value;
					break;

				default:
					return undefined;
			}

			return new PrologFloatLiteral(result);
		}
	}

	public EvaluateToNumber(): IPrologNumber | undefined {
		const thisFunctorExpression =
			this as PrologNameExpression<PrologFunctor>;

		if (typeof thisFunctorExpression === 'undefined') {
			return undefined;
		} else if (this.ExpressionList.length === 1) {
			return PrologNameExpression.EvaluateUnaryOperatorToNumber(
				thisFunctorExpression
			);
		} else if (this.ExpressionList.length !== 2) {
			return undefined;
		}

		const arg1Evaluated = this.ExpressionList[0].EvaluateToNumber();
		const arg2Evaluated = this.ExpressionList[1].EvaluateToNumber();

		if (
			typeof arg1Evaluated === 'undefined' ||
			typeof arg2Evaluated === 'undefined'
		) {
			return undefined;
		}

		if (
			arg1Evaluated.constructor.name === PrologIntegerLiteral.name &&
			arg2Evaluated.constructor.name === PrologIntegerLiteral.name
		) {
			const arg1Value = arg1Evaluated.ToInteger();
			const arg2Value = arg2Evaluated.ToInteger();
			let result: number;

			switch (this.Name.Name) {
				case '+':
					result = arg1Value + arg2Value;
					break;

				case '-':
					result = arg1Value - arg2Value;
					break;

				case '*':
					result = arg1Value * arg2Value;
					break;

				case '/':
					result = arg1Value / arg2Value;
					break;

				case 'mod':
					result = arg1Value % arg2Value;
					break;

				default:
					return undefined;
			}

			return new PrologIntegerLiteral(result);
		} else {
			const arg1Value = arg1Evaluated.ToDouble();
			const arg2Value = arg2Evaluated.ToDouble();
			let result: number;

			switch (this.Name.Name) {
				case '+':
					result = arg1Value + arg2Value;
					break;

				case '-':
					result = arg1Value - arg2Value;
					break;

				case '*':
					result = arg1Value * arg2Value;
					break;

				case '/':
					result = arg1Value / arg2Value;
					break;

				case 'mod':
					result = arg1Value % arg2Value;
					break;

				default:
					return undefined;
			}

			return new PrologFloatLiteral(result);
		}
	}

	public ToGoal(): PrologGoal {
		return new PrologGoal(
			this.gs,
			new PrologPredicate(this.Name.Name),
			this.ExpressionList
		);
	}
}
