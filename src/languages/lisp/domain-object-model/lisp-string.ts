// tom-weatherhead/thaw-grammar/src/languages/lisp/domain-object-model/lisp-string.ts

import { ArgumentException } from '../../../common/exceptions/argument-exception';
import { SExpressionBase } from './sexpression-base';

export class LISPString extends SExpressionBase {
	public readonly value: string;

	constructor(value: string) {
		super();

		// if (value == null) // 2013/12/05 : We will allow an empty string, but not a null reference.
		// {
		// 	throw new ArgumentException("LISPString constructor: value is null.", "value");
		// }

		if (!value) {
			throw new ArgumentException(
				'LISPString constructor: value is null or empty.',
				'value'
			);
		}

		this.value = value;
	}

	public toString(): string {
		return '"' + this.value + '"';
	}

	// public override bool Equals(object obj)
	// {

	// 	LISPString otherString = obj as LISPString;

	// 	return otherString != null && Value == otherString.Value;
	// }

	public isString(): boolean {
		return true;
	}
}
