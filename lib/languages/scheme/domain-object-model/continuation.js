// tom-weatherhead/thaw-grammar/src/languages/scheme/domain-object-model/continuation.ts
'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
exports.Continuation = void 0;
const evaluation_exception_1 = require("../../../common/exceptions/evaluation-exception");
const sexpression_base_1 = require("../../lisp/domain-object-model/sexpression-base");
const continuation_exception_1 = require("../exceptions/continuation-exception");
class Continuation extends sexpression_base_1.SExpressionBase {
    constructor(ccGuid, line = 0, column = 0) {
        super();
        this.ccGuid = ccGuid;
        this.line = line;
        this.column = column;
    }
    toString() {
        return '<continuation>';
    }
    isClosure() {
        return true;
    }
    call(args, localEnvironment, globalInfo) {
        const actualNumArgs = args.value.length;
        if (actualNumArgs !== 1) {
            throw new evaluation_exception_1.EvaluationException(`Continuation.call() : Expected 1 argument, instead of the actual ${actualNumArgs} arguments`, this.line, this.column);
        }
        throw new continuation_exception_1.ContinuationException(this.ccGuid, args.value[0].evaluate(localEnvironment, globalInfo));
    }
}
exports.Continuation = Continuation;
