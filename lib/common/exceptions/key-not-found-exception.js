// tom-weatherhead/thaw-grammar/src/common/exceptions/key-not-found-exception.ts
'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
exports.KeyNotFoundException = void 0;
const exception_base_1 = require("./exception-base");
class KeyNotFoundException extends exception_base_1.ExceptionBase {
    constructor(message, line = 0, column = 0) {
        super('KeyNotFoundException', message, line, column);
    }
}
exports.KeyNotFoundException = KeyNotFoundException;
