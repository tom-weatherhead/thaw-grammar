// tom-weatherhead/thaw-grammar/src/languages/scheme/exceptions/continuation-exception.ts
'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
exports.ContinuationException = void 0;
const exception_base_1 = require("../../../common/exceptions/exception-base");
class ContinuationException extends exception_base_1.ExceptionBase {
    constructor(ccGuid, returnValue, line = 0, column = 0) {
        super('ContinuationException', 'ContinuationException', line, column);
        this.ccGuid = ccGuid;
        this.returnValue = returnValue;
    }
}
exports.ContinuationException = ContinuationException;
