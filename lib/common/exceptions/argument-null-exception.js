// tom-weatherhead/thaw-grammar/src/common/exceptions/argument-null-exception.ts
'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
exports.ArgumentNullException = void 0;
const exception_base_1 = require("./exception-base");
class ArgumentNullException extends exception_base_1.ExceptionBase {
    constructor(message, argumentName, line = 0, column = 0) {
        super('ArgumentNullException', message, line, column);
        this.argumentName = argumentName;
    }
}
exports.ArgumentNullException = ArgumentNullException;
