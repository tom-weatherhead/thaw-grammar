// tom-weatherhead/thaw-grammar/src/languages/lisp/domain-object-model/lisp-string.ts
'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
exports.LISPString = void 0;
const argument_exception_1 = require("../../../common/exceptions/argument-exception");
const sexpression_base_1 = require("./sexpression-base");
class LISPString extends sexpression_base_1.SExpressionBase {
    constructor(value) {
        super();
        // if (value == null) // 2013/12/05 : We will allow an empty string, but not a null reference.
        // {
        // 	throw new ArgumentException("LISPString constructor: value is null.", "value");
        // }
        if (!value) {
            throw new argument_exception_1.ArgumentException('LISPString constructor: value is null or empty.', 'value');
        }
        this.value = value;
    }
    // public override string ToString()
    // {
    // 	return Value;
    // }
    toString() {
        return '"' + this.value + '"';
    }
    // public override bool Equals(object obj)
    // {
    // 	if (object.ReferenceEquals(this, obj))
    // 	{
    // 		return true;
    // 	}
    // 	LISPString otherString = obj as LISPString;
    // 	return otherString != null && Value == otherString.Value;
    // }
    // public override int GetHashCode()
    // {
    // 	return Value.GetHashCode();
    // }
    isString() {
        return true;
    }
}
exports.LISPString = LISPString;
