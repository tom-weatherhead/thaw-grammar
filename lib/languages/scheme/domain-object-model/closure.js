// tom-weatherhead/thaw-grammar/src/languages/scheme/domain-object-model/closure.ts
'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
exports.Closure = void 0;
const environment_frame_1 = require("../../../common/domain-object-model/environment-frame");
// import { ArgumentException } from '../../../common/exceptions/argument-exception';
const evaluation_exception_1 = require("../../../common/exceptions/evaluation-exception");
const sexpression_base_1 = require("../../lisp/domain-object-model/sexpression-base");
class Closure extends sexpression_base_1.SExpressionBase {
    constructor(argList, body, closureEnvironment, line = 0, column = 0) {
        super();
        // console.log('Creating an instance of Closure...');
        this.argList = argList;
        this.body = body;
        this.closureEnvironment = closureEnvironment;
        this.line = line;
        this.column = column;
    }
    call(expressionList, localEnvironment, globalInfo) {
        if (expressionList.value.length !== this.argList.value.length) {
            throw new evaluation_exception_1.EvaluationException(`Closure.call() : Expected ${this.argList.value.length} argument(s), instead of the actual ${expressionList.value.length} argument(s); body = ${this.body}`);
        }
        const evaluatedArguments = expressionList.value.map((expr) => expr.evaluate(localEnvironment, globalInfo));
        const newEnvironment = new environment_frame_1.EnvironmentFrame(this.closureEnvironment);
        // if (globalInfo.Debug) {
        // 	LISPGlobalInfo.CreateStackTraceInNewEnvironmentFrame(localEnvironment, newEnvironment, Line, Column);
        // }
        newEnvironment.compose(this.argList.value, evaluatedArguments);
        return this.body.evaluate(newEnvironment, globalInfo);
    }
    isClosure() {
        return true;
    }
    toString() {
        return '<closure>';
    }
    evaluate(localEnvironment, globalInfo) {
        // console.log('Evaluating an instance of Closure...');
        return this;
    }
}
exports.Closure = Closure;
