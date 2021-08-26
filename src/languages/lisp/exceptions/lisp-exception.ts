// tom-weatherhead/thaw-grammar/src/languages/lisp/exceptions/lisp-exception.ts

import { ExceptionBase } from 'thaw-interpreter-core';

export class LISPException extends ExceptionBase {
	constructor(message: string, line: number, column: number) {
		super('LISPException', message, line, column);
	}
}
