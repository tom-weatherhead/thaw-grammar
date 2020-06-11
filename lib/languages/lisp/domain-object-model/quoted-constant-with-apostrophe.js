// tom-weatherhead/thaw-grammar/src/languages/lisp/domain-object-model/quoted-constant-with-apostrophe.ts
'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
exports.QuotedConstantWithApostrophe = void 0;
class QuotedConstantWithApostrophe {
    constructor(sexpression) {
        this.sexpression = sexpression;
    }
    toString() {
        // return "'" + this.sexpression.toString();
        return `'${this.sexpression}`;
    }
    evaluate(localEnvironment, globalInfo) {
        return this.sexpression;
    }
}
exports.QuotedConstantWithApostrophe = QuotedConstantWithApostrophe;
