// tom-weatherhead/thaw-grammar/src/languages/lisp/exceptions/lisp-exception.ts

'use strict';

import { ExceptionBase } from '../../../common/exceptions/exception-base';

export class LISPException extends ExceptionBase {
	constructor(message: string, line: number, column: number) {
		super('LISPException', message, line, column);
	}
}
