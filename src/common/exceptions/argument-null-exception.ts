// tom-weatherhead/thaw-grammar/src/common/exceptions/argument-null-exception.ts

import { ExceptionBase } from './exception-base';

export class ArgumentNullException extends ExceptionBase {
	public readonly argumentName: string;

	constructor(message: string, argumentName: string, line = 0, column = 0) {
		super('ArgumentNullException', message, line, column);

		this.argumentName = argumentName;
	}
}
