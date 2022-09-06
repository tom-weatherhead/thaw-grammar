// tom-weatherhead/thaw-grammar/src/languages/lisp/domain-object-model/null-sexpression.ts

import { SExpressionBase } from './sexpression-base';

const typenameLISPNull = 'NullSExpression';

export function isNullSExpression(obj: unknown): obj is NullSExpression {
	const value = obj as NullSExpression;

	return typeof value !== 'undefined' && value.typename === typenameLISPNull;
}

export class NullSExpression extends SExpressionBase {
	public readonly typename: string = typenameLISPNull;

	public toString(): string {
		return '()';
	}

	public override isNull(): boolean {
		return true;
	}

	public override isEqualTo(other: unknown): boolean {
		return isNullSExpression(other);
	}
}
