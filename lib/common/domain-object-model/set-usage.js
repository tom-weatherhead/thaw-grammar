// tom-weatherhead/thaw-grammar/src/common/domain-object-model/set-usage.ts
'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
exports.SetUsage = void 0;
class SetUsage {
    constructor(variableName, expression) {
        this.variableName = variableName;
        this.expression = expression;
    }
    toString() {
        return `(set ${this.variableName} ${this.expression})`;
    }
    evaluate(localEnvironment, globalInfo) {
        const expressionValue = this.expression.evaluate(localEnvironment, globalInfo);
        // If the variable is not already defined in a local env, we may have to assign it to the global env.
        // console.log(`SetUsage<T>.Evaluate() : var is ${this.variableName.Name}; value is ${expressionValue}`);
        localEnvironment.addBubbleDown(this.variableName, expressionValue);
        return expressionValue;
    }
}
exports.SetUsage = SetUsage;
