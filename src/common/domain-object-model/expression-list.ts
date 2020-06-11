// tom-weatherhead/thaw-grammar/src/common/domain-object-model/expression-list.ts

'use strict';

import { IExpression } from './iexpression';

export class ExpressionList<T> {
	// Implements INonExpression<T> ? Then semanticStack: Stack<IExpression<T> | INonExpression<T>>
	public readonly value: Array<IExpression<T>> = [];

	public toString(): string {
		return `(${this.value
			.map((expr: IExpression<T>) => expr.toString())
			.join(' ')})`;
	}
}
