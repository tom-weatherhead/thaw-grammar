// tom-weatherhead/thaw-grammar/src/languages/lisp/domain-object-model/null-sexpression.ts

'use strict';

import { SExpressionBase } from './sexpression-base';

export class NullSExpression extends SExpressionBase {
	public toString(): string {
		return '()';
	}

	// public override bool Equals(object obj)
	// {

	// 	if (object.ReferenceEquals(this, obj))
	// 	{
	// 		return true;
	// 	}

	// 	return obj is NullSExpression;
	// }

	// public override int GetHashCode()
	// {
	// 	return 0;
	// }

	public isNull(): boolean {
		return true;
	}
}
