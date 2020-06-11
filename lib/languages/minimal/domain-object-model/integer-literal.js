"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.IntegerLiteral = void 0;
const argument_exception_1 = require("../../../common/exceptions/argument-exception");
class IntegerLiteral {
    constructor(value) {
        if (typeof value !== 'number') {
            throw new argument_exception_1.ArgumentException('IntegerLiteral constructor: value is not a number.', 'value');
        }
        else if (Math.floor(value) !== value) {
            throw new argument_exception_1.ArgumentException('IntegerLiteral constructor: value is not an integer.', 'value');
        }
        this.value = value;
    }
    toString() {
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
    // public ToInteger(): number {
    // 	return this.Value;
    // }
    evaluate(localEnvironment, globalInfo) {
        return this.value;
    }
}
exports.IntegerLiteral = IntegerLiteral;
