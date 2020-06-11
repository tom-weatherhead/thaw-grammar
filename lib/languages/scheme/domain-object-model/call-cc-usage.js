// tom-weatherhead/thaw-grammar/src/languages/scheme/domain-object-model/call-cc-usage.ts
'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
exports.CallCCUsage = void 0;
const expression_list_1 = require("../../../common/domain-object-model/expression-list");
const evaluation_exception_1 = require("../../../common/exceptions/evaluation-exception");
const continuation_exception_1 = require("../exceptions/continuation-exception");
const closure_1 = require("./closure");
const continuation_1 = require("./continuation");
class CallCCUsage {
    constructor(body) {
        this.body = body;
    }
    evaluate(localEnvironment, globalInfo) {
        const evaluatedBody = this.body.evaluate(localEnvironment, globalInfo);
        if (!(evaluatedBody instanceof closure_1.Closure)) {
            throw new evaluation_exception_1.EvaluationException('CallCCUsage.evaluate() : Body does not evaluate to a Closure.');
        }
        const closure = evaluatedBody;
        if (closure.argList.value.length !== 1) {
            throw new evaluation_exception_1.EvaluationException(`CallCCUsage.evaluate() : The Closure takes ${closure.argList.value.length} arguments instead of the expected 1`, closure.line, closure.column);
        }
        // TODO: Use a singleton class named SequenceNumberGenerator to create the 'guids'.
        const ccGuid = Math.random(); // Guid.NewGuid();
        const exprList = new expression_list_1.ExpressionList();
        exprList.value.push(new continuation_1.Continuation(ccGuid, closure.line, closure.column));
        try {
            return closure.call(exprList, localEnvironment, globalInfo);
        }
        catch (ex) {
            if (!(ex instanceof continuation_exception_1.ContinuationException)) {
                throw ex;
            }
            const ce = ex;
            if (ce.ccGuid !== ccGuid) {
                throw ex;
            }
            return ce.returnValue;
        }
    }
}
exports.CallCCUsage = CallCCUsage;
