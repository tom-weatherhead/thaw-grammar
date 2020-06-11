// tom-weatherhead/thaw-grammar/src/common/domain-object-model/let-star-usage.ts
'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
exports.LetStarUsage = void 0;
const environment_frame_1 = require("./environment-frame");
class LetStarUsage {
    constructor(bindings, expression) {
        this.bindings = bindings;
        this.expression = expression;
    }
    toString() {
        const fnBindingAsString = ([v, expr]) => `(${v} ${expr})`;
        const bindingsAsString = this.bindings
            .map(fnBindingAsString)
            .join(' ');
        return `(let* (${bindingsAsString}) ${this.expression})`;
    }
    evaluate(localEnvironment, globalInfo) {
        // 1) No:
        // const newEnvFrame = new EnvironmentFrame<T>(localEnvironment);
        // this.bindings.forEach(([v, expr]: [Variable<T>, IExpression<T>]) => {
        // 	// For this line, LetUsage.evaluate() does this instead:
        // 	// newEnvFrame.add(v, expr.evaluate(localEnvironment, globalInfo));
        // 	newEnvFrame.add(v, expr.evaluate(newEnvFrame, globalInfo));
        // });
        // return this.expression.evaluate(newEnvFrame, globalInfo);
        // 2) Correct C#:
        // var lastEnv = localEnvironment;
        // foreach (var binding in Bindings)
        // {
        //     var newEnvFrame = new EnvironmentFrame<T>(lastEnv);
        //     newEnvFrame.Add(binding.Key, binding.Value.Evaluate(lastEnv, globalInfo));
        //     lastEnv = newEnvFrame;
        // }
        // return Expression.Evaluate(lastEnv, globalInfo);
        // 3)
        const lastNewEnvFrame = this.bindings.reduce((previousEnvFrame, [v, expr]) => {
            const newEnvFrame = new environment_frame_1.EnvironmentFrame(previousEnvFrame);
            newEnvFrame.add(v, expr.evaluate(previousEnvFrame, globalInfo));
            return newEnvFrame;
        }, localEnvironment);
        return this.expression.evaluate(lastNewEnvFrame, globalInfo);
    }
}
exports.LetStarUsage = LetStarUsage;
