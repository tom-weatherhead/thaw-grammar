// tom-weatherhead/thaw-grammar/src/languages/scheme/domain-object-model/let-rec-usage.ts
'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
exports.LetRecUsage = void 0;
const environment_frame_1 = require("../../../common/domain-object-model/environment-frame");
class LetRecUsage {
    constructor(bindings, expression) {
        this.bindings = bindings;
        this.expression = expression;
    }
    toString() {
        const fnBindingAsString = ([v, expr]) => `(${v} ${expr})`;
        const bindingsAsString = this.bindings
            .map(fnBindingAsString)
            .join(' ');
        return `(letrec (${bindingsAsString}) ${this.expression})`;
    }
    // public ISExpression Evaluate(EnvironmentFrame<ISExpression> localEnvironment, IGlobalInfo<ISExpression> globalInfo)
    // {
    //     var falseValue = globalInfo.FalseValue;
    //     var newEnvFrame = new EnvironmentFrame<ISExpression>(localEnvironment);
    //     foreach (var binding in Bindings)   // Add all variables that are bound in Bindings to newEnvFrame before any closures are created in the next loop.
    //     {
    //         newEnvFrame.Add(binding.Key, falseValue);
    //     }
    //     foreach (var binding in Bindings)
    //     {
    //         newEnvFrame.Add(binding.Key, binding.Value.Evaluate(newEnvFrame, globalInfo));
    //     }
    //     return Expression.Evaluate(newEnvFrame, globalInfo);
    // }
    evaluate(localEnvironment, globalInfo) {
        const newEnvFrame = new environment_frame_1.EnvironmentFrame(localEnvironment);
        this.bindings.forEach(([v, expr]) => {
            // Add all variables that are bound in this.bindings to newEnvFrame before any closures are created in the next loop.
            newEnvFrame.add(v, globalInfo.falseValue);
        });
        this.bindings.forEach(([v, expr]) => {
            newEnvFrame.add(v, expr.evaluate(newEnvFrame, globalInfo));
        });
        return this.expression.evaluate(newEnvFrame, globalInfo);
    }
}
exports.LetRecUsage = LetRecUsage;
