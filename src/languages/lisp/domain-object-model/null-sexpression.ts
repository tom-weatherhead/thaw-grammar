// tom-weatherhead/thaw-grammar/src/languages/lisp/domain-object-model/null-sexpression.ts

import { SExpressionBase } from './sexpression-base';

export class NullSExpression extends SExpressionBase {
	public toString(): string {
		return '()';
	}

	// public override bool Equals(object obj)
	// {

	// 	return obj is NullSExpression;
	// }

	public override isNull(): boolean {
		return true;
	}
}
