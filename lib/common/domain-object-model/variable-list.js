// tom-weatherhead/thaw-grammar/src/common/domain-object-model/variable-list.ts
'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
exports.VariableList = void 0;
class VariableList {
    constructor() {
        this.value = [];
    }
    toString() {
        return `(${this.value
            .map((v) => v.toString())
            .join(' ')})`;
    }
}
exports.VariableList = VariableList;
