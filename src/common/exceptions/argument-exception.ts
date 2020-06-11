// tom-weatherhead/thaw-grammar/src/common/exceptions/argument-exception.ts

'use strict';

import { ExceptionBase } from './exception-base';

export class ArgumentException extends ExceptionBase {
	public readonly argumentName: string;

	constructor(message: string, argumentName: string, line = 0, column = 0) {
		super('ArgumentException', message, line, column);

		this.argumentName = argumentName;
	}
}
