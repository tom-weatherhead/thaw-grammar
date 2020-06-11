// tom-weatherhead/thaw-grammar/src/languages/scheme/domain-object-model/primitive-operator.ts
'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
exports.PrimOp = void 0;
const evaluation_exception_1 = require("../../../common/exceptions/evaluation-exception");
const lisp_operator_usage_1 = require("../../lisp/domain-object-model/lisp-operator-usage");
const sexpression_base_1 = require("../../lisp/domain-object-model/sexpression-base");
// TODO? Create static methods in common/domain-object-model/operator-usage
// that export the following functionality:
// - getNumExpectedArgs(operator: string): number | undefined
//   - return -1 for 'any number of arguments is acceptable'
//   - return undefined for 'unknown operator'
// - getBinaryNumericalOperator(operator: string) : (a: number, b: number) => number
// - getBinaryNumericalPredicate(operator: string) : (a: number, b: number) => boolean
// - enum OperatorType { BinaryNumericalOperator, BinaryNumericalPredicate, ..., Unknown }
// - getOperatorType(operator: string): OperatorType | undefined
// - Then PrimOp.evaluate() looks like this:
// switch (operatorType) {
// 	case OperatorType.BinaryNumericalOperator:
// 		const fn: (a: number, b: number) => number = getBinaryNumericalOperator(operatorName.value);
// 		// return new NumberLiteral(fn((evaluatedArguments[0] as IntegerLiteral).value, (evaluatedArguments[1] as IntegerLiteral).value));
// 		// or:
// 		const evaluatedArgumentsAsNumbers: number[] = evaluatedArguments.map((arg: ISExpression) => (arg as IntegerLiteral).value);
// 		return new NumberLiteral(fn(...evaluatedArgumentsAsNumbers));
// 	case ...
// }
class PrimOp extends sexpression_base_1.SExpressionBase {
    constructor(name) {
        super();
        // super(name, new ExpressionList<ISExpression>());
        // if (['+', '-', '*', '/', '=', '<', '>'].indexOf(name.value) < 0) {
        // 	throw new ArgumentException(`Primitive operator '${name.value}' not yet supported`, 'name', name.line, name.column);
        // }
        this.name = name;
        // this.expectedNumArgs = 2; // Hard-coded for the operator +
        this.line = name.line;
        this.column = name.column;
    }
    call(expressionList, localEnvironment, globalInfo) {
        if ([
            '+',
            '-',
            '*',
            '/',
            '=',
            '<',
            '>',
            'number?',
            'symbol?',
            'list?',
            'null?',
            'string?',
            'cons',
            'car',
            'cdr',
            'list',
            'print',
            'floor',
            'random'
        ].indexOf(this.name.value) >= 0) {
            const operatorUsage = new lisp_operator_usage_1.LISPOperatorUsage(this.name, expressionList);
            return operatorUsage.evaluate(localEnvironment, globalInfo);
        }
        // First, check the number of arguments. (TODO)
        // Then check the argument types. (TODO)
        // Then:
        const evaluatedArguments = expressionList.value.map((expr) => expr.evaluate(localEnvironment, globalInfo));
        switch (this.name.value) {
            case 'primop?':
                return evaluatedArguments[0].isPrimOp()
                    ? globalInfo.trueValue
                    : globalInfo.falseValue;
            case 'closure?':
                return evaluatedArguments[0].isClosure()
                    ? globalInfo.trueValue
                    : globalInfo.falseValue;
            default:
                throw new evaluation_exception_1.EvaluationException(`PrimOp.call() : Unknown operator name '${this.name.value}'`, this.name.line, this.name.column);
        }
        // return globalInfo.falseValue;
    }
    isPrimOp() {
        return true;
    }
    toString() {
        return this.name.value;
    }
    evaluate(localEnvironment, globalInfo) {
        return this;
    }
}
exports.PrimOp = PrimOp;
