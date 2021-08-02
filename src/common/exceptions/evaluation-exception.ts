// tom-weatherhead/thaw-grammar/src/common/exceptions/evaluation-exception.ts

import { ExceptionBase } from './exception-base';

export class EvaluationException extends ExceptionBase {
	constructor(message: string, line = 0, column = 0) {
		super('EvaluationException', message, line, column);
	}
}
