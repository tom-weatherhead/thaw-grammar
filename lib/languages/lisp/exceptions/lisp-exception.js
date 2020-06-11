// tom-weatherhead/thaw-grammar/src/languages/lisp/exceptions/lisp-exception.ts
'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
exports.LISPException = void 0;
const exception_base_1 = require("../../../common/exceptions/exception-base");
class LISPException extends exception_base_1.ExceptionBase {
    constructor(message, line, column) {
        super('LISPException', message, line, column);
    }
}
exports.LISPException = LISPException;
