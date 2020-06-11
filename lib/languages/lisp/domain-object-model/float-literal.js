// tom-weatherhead/thaw-grammar/src/languages/lisp/domain-object-model/float-literal.ts
'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
exports.FloatLiteral = void 0;
const argument_exception_1 = require("../../../common/exceptions/argument-exception");
const evaluation_exception_1 = require("../../../common/exceptions/evaluation-exception");
const sexpression_base_1 = require("./sexpression-base");
class FloatLiteral extends sexpression_base_1.SExpressionBase {
    constructor(value) {
        super();
        if (typeof value !== 'number') {
            throw new argument_exception_1.ArgumentException(`FloatLiteral constructor: typeof value is not 'number'; it is '${typeof value}'.`, 'value');
        }
        else if (Number.isNaN(value)) {
            throw new argument_exception_1.ArgumentException('FloatLiteral constructor: value is not a number (NaN).', 'value');
        }
        this.value = value;
    }
    // public override string ToString()
    // {
    // 	// E.g. If d == 13.0, we want the string to be "13.0" rather than just "13", so that we can distinguish it from an integer.
    // 	// Note: d == (tan (/ pi 4)) is a good test case; it should yield "1.0", not "1".
    // 	var result = Value.ToString();
    // 	if (result.IndexOf('.') < 0 && result.IndexOf('E') < 0) // I.e. If result does not contain either '.' or 'E'.
    // 	{
    // 		result = result + ".0";
    // 	}
    // 	return result;
    // }
    toString() {
        // TODO: Ensure that there is a decimal point in the resulting string.
        // Floating-point numbers must always contain a decimal point.
        // Integers must never contain a decimal point.
        // Do not allow the output to be formatted as scientific notation.
        let result = `${this.value}`;
        if (result.match(/[A-Za-z]/)) {
            throw new evaluation_exception_1.EvaluationException(`FloatLiteral.toString() : The result ${result} contains one or more alphabetic characters`);
        }
        if (result.indexOf('.') < 0) {
            result = result + '.0';
        }
        return result;
    }
    // public override bool Equals(object obj)
    // {
    // 	if (object.ReferenceEquals(this, obj))
    // 	{
    // 		return true;
    // 	}
    // 	FloatLiteral otherFltLit = obj as FloatLiteral;
    // 	return otherFltLit != null && Value == otherFltLit.Value;
    // }
    // public override int GetHashCode()
    // {
    // 	return Value.GetHashCode();
    // }
    toInteger() {
        return Math.floor(this.value);
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
exports.FloatLiteral = FloatLiteral;
