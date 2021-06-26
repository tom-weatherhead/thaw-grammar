// prolog-predicate.ts

// Note: The C# class PrologGrammar2_LL1 does not create or use predicates.

import { PrologNameBase } from './prolog-name-base';

export class PrologPredicate extends PrologNameBase {
	constructor(name: string) {
		super(name);
	}

	public equals(obj: unknown): boolean {
		return obj instanceof PrologPredicate && super.equals(obj);
	}

	// public override bool Equals(object obj)
	// {

	//     if (object.ReferenceEquals(this, obj))
	//     {
	//         return true;
	//     }

	// 	#if NAME_EXPRESSION_EQUALITY
	//     var otherNameExpr = obj as PrologNameExpression<PrologPredicate>;

	//     if (otherNameExpr != null)
	//     {
	//         return otherNameExpr.Equals(this);
	//     }
	// 	#endif

	//     // var otherPredicate = obj as PrologPredicate;

	//     // // == is used to test Name equality because Name is a string.
	//     // return otherPredicate != null && Name == otherPredicate.Name;
	//     return base.Equals(obj);
	// }

	// public override int GetHashCode()   // We override this function in order to satisfy Visual Studio, since we overrode this class's Equals().
	// {
	//     return base.GetHashCode() + 2;
	// }
}
