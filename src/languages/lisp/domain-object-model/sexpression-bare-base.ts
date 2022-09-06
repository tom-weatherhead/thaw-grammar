// tom-weatherhead/thaw-grammar/src/languages/lisp/domain-object-model/sexpression-bare-base.ts

import { ISExpression } from './isexpression';

export abstract class SExpressionBareBase implements ISExpression {
	// TODO: Add 'get' to all of the type predicates to turn them into accessors; e.g.:
	// public get isNumber(): boolean {
	// 	return false;
	// }

	public isNumber(): boolean {
		return false;
	}

	public isSymbol(): boolean {
		return false;
	}

	public isList(): boolean {
		return false;
	}

	public isNull(): boolean {
		return false;
	}

	public isPrimOp(): boolean {
		return false;
	}

	public isClosure(): boolean {
		return false;
	}

	public isString(): boolean {
		return false;
	}

	public abstract toString(): string;

	public isEqualTo(other: unknown): boolean {
		other;
		throw new Error('Call to unimplemented isEqualTo()');
		// return false;
	}
}
