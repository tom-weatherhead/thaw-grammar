// tom-weatherhead/thaw-grammar/src/languages/scheme/exceptions/continuation-exception.ts

import { ExceptionBase } from 'thaw-interpreter-core';

import { ISExpression } from '../../lisp/domain-object-model/isexpression';

export class ContinuationException extends ExceptionBase {
	public readonly ccGuid: number;
	public readonly returnValue: ISExpression;

	constructor(ccGuid: number, returnValue: ISExpression, line = 0, column = 0) {
		super('ContinuationException', 'ContinuationException', line, column);

		this.ccGuid = ccGuid;
		this.returnValue = returnValue;
	}
}
