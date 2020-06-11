"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.IntegerLiteral = void 0;
const argument_exception_1 = require("../../../common/exceptions/argument-exception");
class IntegerLiteral {
    constructor(value, line = 0, column = 0) {
        if (typeof value !== 'number') {
            throw new argument_exception_1.ArgumentException(`IntegerLiteral constructor: typeof value is not 'number'; it is '${typeof value}'.`, 'value');
        }
        else if (Number.isNaN(value)) {
            throw new argument_exception_1.ArgumentException('IntegerLiteral constructor: value is not a number (NaN).', 'value');
        }
        else if (Math.floor(value) !== value) {
            throw new argument_exception_1.ArgumentException('IntegerLiteral constructor: value is not an integer.', 'value');
        }
        this.value = value;
        this.line = line;
        this.column = column;
    }
    toString() {
        // Do not allow the output to be formatted as scientific notation.
        // return this.Value.toString();
        return `${this.value}`;
    }
    evaluate(localEnvironment, globalInfo) {
        return this.value;
    }
}
exports.IntegerLiteral = IntegerLiteral;
