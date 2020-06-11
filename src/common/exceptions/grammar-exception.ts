// tom-weatherhead/thaw-grammar/src/common/exceptions/grammar-exception.ts

'use strict';

import { ExceptionBase } from './exception-base';

export class GrammarException extends ExceptionBase {
	constructor(message: string, line = 0, column = 0) {
		super('GrammarException', message, line, column);
	}
}
