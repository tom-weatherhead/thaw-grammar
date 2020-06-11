// tom-weatherhead/thaw-grammar/src/languages/lisp/domain-object-model/quoted-constant-with-quote-keyword.ts
'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
exports.QuotedConstantWithQuoteKeyword = void 0;
class QuotedConstantWithQuoteKeyword {
    constructor(sexpression) {
        this.sexpression = sexpression;
    }
    toString() {
        // return "'" + this.sexpression.toString();
        return `(quote ${this.sexpression})`;
    }
    evaluate(localEnvironment, globalInfo) {
        return this.sexpression;
    }
}
exports.QuotedConstantWithQuoteKeyword = QuotedConstantWithQuoteKeyword;
