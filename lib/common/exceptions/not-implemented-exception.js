// tom-weatherhead/thaw-grammar/src/common/exceptions/not-implemented-exception.ts
'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
exports.NotImplementedException = void 0;
const exception_base_1 = require("./exception-base");
class NotImplementedException extends exception_base_1.ExceptionBase {
    constructor(message, line = 0, column = 0) {
        super('NotImplementedException', message, line, column);
    }
}
exports.NotImplementedException = NotImplementedException;
