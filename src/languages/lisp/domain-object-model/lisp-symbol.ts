// tom-weatherhead/thaw-grammar/src/languages/lisp/domain-object-model/lisp-symbol.ts

'use strict';

import { ArgumentException } from '../../../common/exceptions/argument-exception';
import { SExpressionBase } from './sexpression-base';

export class LISPSymbol extends SExpressionBase {
	public readonly value: string;

	constructor(value: string) {
		super();

		if (!value) {
			throw new ArgumentException(
				'LISPSymbol constructor: value is null or empty.',
				'value'
			);
		}

		this.value = value;
	}

	// public override string ToString()
	// {
	// 	return Value;
	// }

	public toString(): string {
		return this.value;
	}

	// public override bool Equals(object obj)
	// {

	// 	if (object.ReferenceEquals(this, obj))
	// 	{
	// 		return true;
	// 	}

	// 	LISPSymbol otherSymbol = obj as LISPSymbol;

	// 	return otherSymbol != null && Value == otherSymbol.Value;
	// }

	// public override int GetHashCode()
	// {
	// 	return Value.GetHashCode();
	// }

	public isSymbol(): boolean {
		return true;
	}
}
