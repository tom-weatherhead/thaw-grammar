// tom-weatherhead/thaw-grammar/src/languages/lisp/domain-object-model/integer-literal.ts
'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
exports.IntegerLiteral = void 0;
const argument_exception_1 = require("../../../common/exceptions/argument-exception");
const sexpression_base_1 = require("./sexpression-base");
class IntegerLiteral extends sexpression_base_1.SExpressionBase {
    constructor(value) {
        super();
        // HACK ThAW 2020-01-06 : The value in an IntegerLiteral can be int or float.
        // In the future, we will need to properly support FloatLiterals
        // and distinguish them from IntegerLiterals.
        if (typeof value !== 'number') {
            throw new argument_exception_1.ArgumentException(`IntegerLiteral constructor: typeof value '${value}' is not 'number'; it is '${typeof value}'.`, 'value');
        }
        else if (Number.isNaN(value)) {
            throw new argument_exception_1.ArgumentException('IntegerLiteral constructor: value is not a number (NaN).', 'value');
            // } else if (Math.floor(value) !== value) {
            // throw new ArgumentException(`IntegerLiteral constructor: value '${value}' is not an integer.`, 'value');
        }
        this.value = value;
    }
    toString() {
        // Do not allow the output to be formatted as scientific notation.
        // return this.Value.toString();
        return `${this.value}`;
    }
    // public override bool Equals(object obj)
    // {
    // 	if (object.ReferenceEquals(this, obj))
    // 	{
    // 		return true;
    // 	}
    // 	IntegerLiteral otherIntLit = obj as IntegerLiteral;
    // 	return otherIntLit != null && Value == otherIntLit.Value;
    // }
    // public override int GetHashCode()
    // {
    // 	return Value.GetHashCode();
    // }
    toInteger() {
        return this.value;
    }
    toDouble() {
        return this.value;
    }
    isNumber() {
        return true;
    }
    convertToGraph() {
        return this;
    }
}
exports.IntegerLiteral = IntegerLiteral;
