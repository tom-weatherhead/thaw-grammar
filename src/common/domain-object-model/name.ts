// tom-weatherhead/thaw-grammar/src/common/domain-object-model/name.ts

'use strict';

import { ArgumentNullException } from '../exceptions/argument-null-exception';

export class Name {
	public readonly value: string;
	public readonly line: number;
	public readonly column: number;

	constructor(value: string, line = 0, column = 0) {
		if (!value) {
			throw new ArgumentNullException(
				'A Name cannot have a null or empty value',
				'value'
			);
		}

		this.value = value;
		this.line = line;
		this.column = column;
	}

	public toString(): string {
		return this.value;
	}
}
