// tom-weatherhead/thaw-grammar/src/common/domain-object-model/function-definition.ts
'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
exports.FunctionDefinition = void 0;
class FunctionDefinition {
    constructor(functionName, argList, body) {
        this.functionName = functionName;
        this.argList = argList;
        this.body = body;
    }
    toString() {
        return `(define ${this.functionName} ${this.argList} ${this.body})`;
    }
    evaluate(localEnvironment, globalInfo) {
        globalInfo.functionDefinitions.set(this.functionName.value, this);
        return globalInfo.trueValue;
    }
}
exports.FunctionDefinition = FunctionDefinition;
