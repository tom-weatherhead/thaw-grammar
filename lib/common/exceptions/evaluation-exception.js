// tom-weatherhead/thaw-grammar/src/common/exceptions/evaluation-exception.ts
'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
exports.EvaluationException = void 0;
const exception_base_1 = require("./exception-base");
class EvaluationException extends exception_base_1.ExceptionBase {
    constructor(message, line = 0, column = 0) {
        super('EvaluationException', message, line, column);
    }
}
exports.EvaluationException = EvaluationException;
