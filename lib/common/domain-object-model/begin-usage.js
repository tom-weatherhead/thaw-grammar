// tom-weatherhead/thaw-grammar/src/common/domain-object-model/begin-usage.ts
'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
exports.BeginUsage = void 0;
class BeginUsage {
    constructor(firstExpression, expressionList) {
        this.firstExpression = firstExpression;
        this.expressionList = expressionList;
    }
    toString() {
        return `(begin ${this.firstExpression} ${this.expressionList})`;
    }
    evaluate(localEnvironment, globalInfo) {
        return this.expressionList.value.reduce((previousResult, expression) => expression.evaluate(localEnvironment, globalInfo), // Lint: Yes, previousResult is unused.
        this.firstExpression.evaluate(localEnvironment, globalInfo));
    }
}
exports.BeginUsage = BeginUsage;
