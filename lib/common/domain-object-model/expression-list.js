// tom-weatherhead/thaw-grammar/src/common/domain-object-model/expression-list.ts
'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
exports.ExpressionList = void 0;
class ExpressionList {
    constructor() {
        // Implements INonExpression<T> ? Then semanticStack: Stack<IExpression<T> | INonExpression<T>>
        this.value = [];
    }
    toString() {
        return `(${this.value
            .map((expr) => expr.toString())
            .join(' ')})`;
    }
}
exports.ExpressionList = ExpressionList;
