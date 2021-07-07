// tom-weatherhead/thaw-grammar/src/common/domain-object-model/variable-list.ts

'use strict';

import { Variable } from './variable';

export class VariableList<T> {
	public readonly value: Variable<T>[] = [];

	public toString(): string {
		return `(${this.value.map((v: Variable<T>) => v.toString()).join(' ')})`;
	}
}
