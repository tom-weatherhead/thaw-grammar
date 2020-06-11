// tom-weatherhead/thaw-grammar/src/common/exceptions/exception-base.ts
'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
exports.ExceptionBase = void 0;
class ExceptionBase {
    constructor(typeName, message, line = 0, column = 0) {
        const lineAndColumnText = line > 0 && column > 0
            ? ` at line ${line}, column ${column}`
            : '';
        this.message = `${typeName}${lineAndColumnText}: ${message}`;
        this.line = line;
        this.column = column;
    }
    toString() {
        return this.message;
    }
}
exports.ExceptionBase = ExceptionBase;
