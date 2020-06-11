// tom-weatherhead/thaw-grammar/src/languages/lisp/domain-object-model/sexpression-bare-base.ts
'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
exports.SExpressionBareBase = void 0;
class SExpressionBareBase {
    // TODO: Add 'get' to all of the type predicates to turn them into accessors; e.g.:
    // public get isNumber(): boolean {
    // 	return false;
    // }
    isNumber() {
        return false;
    }
    isSymbol() {
        return false;
    }
    isList() {
        return false;
    }
    isNull() {
        return false;
    }
    isPrimOp() {
        return false;
    }
    isClosure() {
        return false;
    }
    isString() {
        return false;
    }
}
exports.SExpressionBareBase = SExpressionBareBase;
