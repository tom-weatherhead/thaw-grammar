// tom-weatherhead/thaw-grammar/src/common/exceptions/argument-exception.ts
'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
exports.ArgumentException = void 0;
const exception_base_1 = require("./exception-base");
class ArgumentException extends exception_base_1.ExceptionBase {
    constructor(message, argumentName, line = 0, column = 0) {
        super('ArgumentException', message, line, column);
        this.argumentName = argumentName;
    }
}
exports.ArgumentException = ArgumentException;
