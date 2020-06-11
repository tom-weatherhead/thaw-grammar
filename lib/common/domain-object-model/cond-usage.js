// tom-weatherhead/thaw-grammar/src/common/domain-object-model/cond-usage.ts
'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
exports.CondUsage = void 0;
class CondUsage {
    constructor(exprPairList) {
        this.exprPairList = exprPairList;
    }
    toString() {
        const fnExprPairAsString = ([expr1, expr2]) => `(${expr1} ${expr2})`;
        return `(cond ${this.exprPairList
            .map(([expr1, expr2]) => '(' + expr1.toString() + ' ' + expr2.toString() + ')')
            .join(' ')})`;
    }
    evaluate(localEnvironment, globalInfo) {
        for (const [expr1, expr2] of this.exprPairList) {
            if (!globalInfo.valueIsFalse(expr1.evaluate(localEnvironment, globalInfo))) {
                return expr2.evaluate(localEnvironment, globalInfo);
            }
        }
        return globalInfo.falseValue;
    }
}
exports.CondUsage = CondUsage;
