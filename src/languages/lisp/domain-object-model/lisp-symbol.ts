// tom-weatherhead/thaw-grammar/src/languages/lisp/domain-object-model/lisp-symbol.ts

import { ArgumentException } from 'thaw-interpreter-core';

import { SExpressionBase } from './sexpression-base';

const typenameLISPSymbol = 'LISPSymbol';

export function isLISPSymbol(obj: unknown): obj is LISPSymbol {
	const sym = obj as LISPSymbol;

	return typeof sym !== 'undefined' && sym.typename === typenameLISPSymbol;
}

export class LISPSymbol extends SExpressionBase {
	public readonly typename: string = typenameLISPSymbol;
	public readonly value: string;

	constructor(value: string) {
		super();

		if (!value) {
			throw new ArgumentException('LISPSymbol constructor: value is null or empty.', 'value');
		}

		this.value = value;
	}

	public toString(): string {
		return this.value;
	}

	public override isSymbol(): boolean {
		return true;
	}

	public override isEqualTo(other: unknown): boolean {
		return isLISPSymbol(other) && other.value === this.value;
	}
}
