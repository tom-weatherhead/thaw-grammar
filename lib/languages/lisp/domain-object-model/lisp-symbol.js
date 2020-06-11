// tom-weatherhead/thaw-grammar/src/languages/lisp/domain-object-model/lisp-symbol.ts
'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
exports.LISPSymbol = void 0;
const argument_exception_1 = require("../../../common/exceptions/argument-exception");
const sexpression_base_1 = require("./sexpression-base");
class LISPSymbol extends sexpression_base_1.SExpressionBase {
    constructor(value) {
        super();
        if (!value) {
            throw new argument_exception_1.ArgumentException('LISPSymbol constructor: value is null or empty.', 'value');
        }
        this.value = value;
    }
    // public override string ToString()
    // {
    // 	return Value;
    // }
    toString() {
        return this.value;
    }
    // public override bool Equals(object obj)
    // {
    // 	if (object.ReferenceEquals(this, obj))
    // 	{
    // 		return true;
    // 	}
    // 	LISPSymbol otherSymbol = obj as LISPSymbol;
    // 	return otherSymbol != null && Value == otherSymbol.Value;
    // }
    // public override int GetHashCode()
    // {
    // 	return Value.GetHashCode();
    // }
    isSymbol() {
        return true;
    }
}
exports.LISPSymbol = LISPSymbol;
