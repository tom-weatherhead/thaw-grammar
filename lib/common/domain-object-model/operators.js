// tom-weatherhead/thaw-grammar/src/common/domain-object-model/operators.ts
'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
exports.Operators = exports.OperatorType = void 0;
var OperatorType;
(function (OperatorType) {
    // Unknown,
    OperatorType[OperatorType["BinaryNumericOperator"] = 0] = "BinaryNumericOperator";
    OperatorType[OperatorType["BinaryNumericPredicate"] = 1] = "BinaryNumericPredicate";
    OperatorType[OperatorType["TypePredicate"] = 2] = "TypePredicate";
    OperatorType[OperatorType["UnlimitedNumericOperator"] = 3] = "UnlimitedNumericOperator";
})(OperatorType = exports.OperatorType || (exports.OperatorType = {}));
class Operators {
    constructor() {
        this.mapOperatorNameToDetails = new Map();
        this.mapOperatorNameToDetails.set('+', [
            OperatorType.UnlimitedNumericOperator,
            -1,
            (a, b) => a + b
        ]);
        this.mapOperatorNameToDetails.set('-', [
            OperatorType.BinaryNumericOperator,
            2,
            (a, b) => a - b
        ]);
        this.mapOperatorNameToDetails.set('*', [
            OperatorType.UnlimitedNumericOperator,
            -1,
            (a, b) => a * b
        ]);
        this.mapOperatorNameToDetails.set('/', [
            OperatorType.BinaryNumericOperator,
            2,
            (a, b) => Math.floor(a / b)
        ]);
        this.mapOperatorNameToDetails.set('=', [
            OperatorType.BinaryNumericPredicate,
            2,
            (a, b) => (a === b ? 1 : 0)
        ]);
        this.mapOperatorNameToDetails.set('>', [
            OperatorType.BinaryNumericPredicate,
            2,
            (a, b) => (a > b ? 1 : 0)
        ]);
        this.mapOperatorNameToDetails.set('<', [
            OperatorType.BinaryNumericPredicate,
            2,
            (a, b) => (a < b ? 1 : 0)
        ]);
        // this.mapOperatorNameToDetails.set('isNumber?', [OperatorType.TypePredicate, 1, () => ]);
        // this.mapOperatorNameToDetails.set('', [OperatorType., 0, () => ]);
    }
    // This class is a Singleton.
    static getInstance() {
        if (!Operators.instance) {
            Operators.instance = new Operators();
        }
        return Operators.instance;
    }
    getOperator(name) {
        return this.mapOperatorNameToDetails.get(name);
    }
}
exports.Operators = Operators;
