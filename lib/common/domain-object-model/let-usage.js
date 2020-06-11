// tom-weatherhead/thaw-grammar/src/common/domain-object-model/let-usage.ts
'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
exports.LetUsage = void 0;
const environment_frame_1 = require("./environment-frame");
class LetUsage {
    constructor(bindings, expression) {
        this.bindings = bindings;
        this.expression = expression;
    }
    toString() {
        const fnBindingAsString = ([v, expr]) => `(${v} ${expr})`;
        const bindingsAsString = this.bindings
            .map(fnBindingAsString)
            .join(' ');
        return `(let (${bindingsAsString}) ${this.expression})`;
    }
    evaluate(localEnvironment, globalInfo) {
        const newEnvFrame = new environment_frame_1.EnvironmentFrame(localEnvironment);
        this.bindings.forEach(([v, expr]) => {
            newEnvFrame.add(v, expr.evaluate(localEnvironment, globalInfo));
        });
        return this.expression.evaluate(newEnvFrame, globalInfo);
    }
}
exports.LetUsage = LetUsage;
