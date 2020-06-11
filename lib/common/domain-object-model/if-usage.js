// tom-weatherhead/thaw-grammar/src/common/domain-object-model/if-usage.ts
'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
exports.IfUsage = void 0;
class IfUsage {
    constructor(condition, ifBody, elseBody) {
        // console.log(`IfUsage 1: condition is ${condition}`);
        // console.log(`IfUsage 2: ifBody is ${ifBody}`);
        // console.log(`IfUsage 3: elseBody is ${elseBody}`);
        this.condition = condition;
        this.ifBody = ifBody;
        this.elseBody = elseBody;
    }
    toString() {
        return `(if ${this.condition} ${this.ifBody} ${this.elseBody})`;
    }
    evaluate(localEnvironment, globalInfo) {
        const conditionValue = this.condition.evaluate(localEnvironment, globalInfo);
        // console.log(`IfUsage.evaluate() 1: conditionValue is ${typeof conditionValue} ${conditionValue}`);
        // console.log(`IfUsage.evaluate() 2: globalInfo.falseValue is ${typeof globalInfo.falseValue} ${globalInfo.falseValue}`);
        // console.log(`IfUsage.evaluate() 3: conditionValue is false? ${globalInfo.valueIsFalse(conditionValue)}`);
        if (!globalInfo.valueIsFalse(conditionValue)) {
            return this.ifBody.evaluate(localEnvironment, globalInfo);
        }
        else {
            return this.elseBody.evaluate(localEnvironment, globalInfo);
        }
    }
}
exports.IfUsage = IfUsage;
