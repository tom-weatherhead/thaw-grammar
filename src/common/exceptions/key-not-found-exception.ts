// tom-weatherhead/thaw-grammar/src/common/exceptions/key-not-found-exception.ts

import { ExceptionBase } from 'thaw-interpreter-core';

export class KeyNotFoundException extends ExceptionBase {
	constructor(message: string, line = 0, column = 0) {
		super('KeyNotFoundException', message, line, column);
	}
}
