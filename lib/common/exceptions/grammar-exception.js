// tom-weatherhead/thaw-grammar/src/common/exceptions/grammar-exception.ts
'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
exports.GrammarException = void 0;
const exception_base_1 = require("./exception-base");
class GrammarException extends exception_base_1.ExceptionBase {
    constructor(message, line = 0, column = 0) {
        super('GrammarException', message, line, column);
    }
}
exports.GrammarException = GrammarException;
