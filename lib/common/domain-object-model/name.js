// tom-weatherhead/thaw-grammar/src/common/domain-object-model/name.ts
'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
exports.Name = void 0;
const argument_null_exception_1 = require("../exceptions/argument-null-exception");
class Name {
    constructor(value, line = 0, column = 0) {
        if (!value) {
            throw new argument_null_exception_1.ArgumentNullException('A Name cannot have a null or empty value', 'value');
        }
        this.value = value;
        this.line = line;
        this.column = column;
    }
    toString() {
        return this.value;
    }
}
exports.Name = Name;
