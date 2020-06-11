// tom-weatherhead/thaw-grammar/src/languages/lisp/domain-object-model/isexpression.ts

'use strict';

export interface ISExpression {
	// TODO: Turn these predicates into 'get' accessors; e.g.:
	// isNumber: boolean; // This is a 'get' accessor.
	isNumber(): boolean;
	isSymbol(): boolean;
	isList(): boolean;
	isNull(): boolean;
	isPrimOp(): boolean;
	isClosure(): boolean;
	isString(): boolean;

	toString(): string;
}
