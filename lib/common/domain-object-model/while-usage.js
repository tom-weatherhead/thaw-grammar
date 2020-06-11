// tom-weatherhead/thaw-grammar/src/common/domain-object-model/while-usage.ts
'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
exports.WhileUsage = void 0;
class WhileUsage {
    constructor(condition, body) {
        this.condition = condition;
        this.body = body;
    }
    toString() {
        return `(if ${this.condition} ${this.body})`;
    }
    evaluate(localEnvironment, globalInfo) {
        while (!globalInfo.valueIsFalse(this.condition.evaluate(localEnvironment, globalInfo))) {
            this.body.evaluate(localEnvironment, globalInfo);
        }
        return globalInfo.falseValue;
    }
}
exports.WhileUsage = WhileUsage;
