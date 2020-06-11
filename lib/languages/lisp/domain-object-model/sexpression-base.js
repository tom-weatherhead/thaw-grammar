// tom-weatherhead/thaw-grammar/src/languages/lisp/domain-object-model/sexpression-base.ts
'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
exports.SExpressionBase = void 0;
const sexpression_bare_base_1 = require("./sexpression-bare-base");
class SExpressionBase extends sexpression_bare_base_1.SExpressionBareBase {
    evaluate(localEnvironment, globalInfo) {
        return this;
    }
}
exports.SExpressionBase = SExpressionBase;
