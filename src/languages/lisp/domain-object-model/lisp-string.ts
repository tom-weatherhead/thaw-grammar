// tom-weatherhead/thaw-grammar/src/languages/lisp/domain-object-model/lisp-string.ts

import { ArgumentException } from 'thaw-interpreter-core';

import { SExpressionBase } from './sexpression-base';

const typenameLISPString = 'LISPString';

export function isLISPString(obj: unknown): obj is LISPString {
	const str = obj as LISPString;

	return typeof str !== 'undefined' && str.typename === typenameLISPString;
}

export class LISPString extends SExpressionBase {
	public readonly typename: string = typenameLISPString;
	public readonly value: string;

	constructor(value: string) {
		super();

		// if (value == null) // 2013/12/05 : We will allow an empty string, but not a null reference.
		// {
		// 	throw new ArgumentException("LISPString constructor: value is null.", "value");
		// }

		if (!value) {
			throw new ArgumentException('LISPString constructor: value is null or empty.', 'value');
		}

		this.value = value;
	}

	public toString(): string {
		return `"${this.value}"`;
		// return this.value;
	}

	// public override bool Equals(object obj)
	// {

	// 	LISPString otherString = obj as LISPString;

	// 	return otherString != null && Value == otherString.Value;
	// }

	public override isString(): boolean {
		return true;
	}
}
