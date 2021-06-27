// prolog-functor.ts

// import { PrologNameBase } from './prolog-name-base';

// export class PrologFunctor extends PrologNameBase {
// 	constructor(name: string) {
// 		// The name must not begin with a capital letter.
// 		super(name);
// 	}

// 	public equals(obj: unknown): boolean {
// 		return obj instanceof PrologFunctor && super.equals(obj);
// 	}

// 	// public override bool Equals(object obj)
// 	// {

// 	//     if (object.ReferenceEquals(this, obj))
// 	//     {
// 	//         return true;
// 	//     }

// 	// 	#if NAME_EXPRESSION_EQUALITY
// 	//     var otherNameExpr = obj as PrologNameExpression<PrologFunctor>;

// 	//     if (otherNameExpr != null)
// 	//     {
// 	//         return otherNameExpr.Equals(this);
// 	//     }
// 	// 	#endif

// 	//     // var otherFunctor = obj as PrologFunctor;

// 	//     // // == is used to test Name equality because Name is a string.
// 	//     // return otherFunctor != null && Name == otherFunctor.Name;
// 	//     return base.Equals(obj);
// 	// }

// 	// public override int GetHashCode()   // We override this function in order to satisfy Visual Studio, since we overrode this class's Equals().
// 	// {
// 	//     return base.GetHashCode() + 1;
// 	// }
// }
