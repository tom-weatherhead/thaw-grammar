// tom-weatherhead/thaw-grammar/src/common/exceptions/not-implemented-exception.ts

'use strict';

import { ExceptionBase } from './exception-base';

export class NotImplementedException extends ExceptionBase {
	constructor(message: string, line = 0, column = 0) {
		super('NotImplementedException', message, line, column);
	}
}
