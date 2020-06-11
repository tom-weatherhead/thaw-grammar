"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.OperatorUsage = void 0;
// import { Variable } from './variable';
const argument_exception_1 = require("../../../common/exceptions/argument-exception");
const evaluation_exception_1 = require("../../../common/exceptions/evaluation-exception");
class OperatorUsage {
    // public readonly twoArgumentIntegerPredicates = new Map<string, (operand1: number, operand2: number) => boolean>();
    // public readonly twoArgumentIntegerOperators = new Map<string, (operand1: number, operand2: number) => number>();
    constructor(operatorName, expressionList) {
        if (operatorName.value !== '+') {
            throw new argument_exception_1.ArgumentException("OperatorUsage constructor: operator name is not '+'.", 'operatorName');
        }
        this.operatorName = operatorName;
        this.expressionList = expressionList;
        // this.twoArgumentIntegerPredicates.set('<', (operand1: number, operand2: number) => operand1 < operand2);
        // this.twoArgumentIntegerPredicates.set('>', (operand1: number, operand2: number) => operand1 > operand2);
        // this.twoArgumentIntegerOperators.set('+', (operand1: number, operand2: number) => operand1 + operand2);
        // this.twoArgumentIntegerOperators.set('-', (operand1: number, operand2: number) => operand1 - operand2);
        // this.twoArgumentIntegerOperators.set('*', (operand1: number, operand2: number) => operand1 * operand2);
        // this.twoArgumentIntegerOperators.set('/', (operand1: number, operand2: number) => operand1 / operand2);
    }
    // public override string ToString()
    // {
    // 	if (expressionList.Value.Count == 0)
    // 	{
    // 		return string.Format("({0})", operatorName);
    // 	}
    // 	return string.Format("({0} {1})", operatorName, expressionList);
    // }
    // This is virtual because Scheme.PrimOp overrides it.
    evaluate(localEnvironment, globalInfo) {
        const actualNumArgs = this.expressionList.value.length;
        if (actualNumArgs !== 2) {
            throw new evaluation_exception_1.EvaluationException(`OperatorUsage : Expected two argument(s) for operator '+', instead of the actual ${actualNumArgs} argument(s)`, this.operatorName.line, this.operatorName.column);
        }
        const evaluatedArguments = this.expressionList.value.map((expr) => expr.evaluate(localEnvironment, globalInfo));
        if (!globalInfo.valueIsInteger(evaluatedArguments[0])) {
            throw new evaluation_exception_1.EvaluationException(`EvaluateAux() : The first argument '${evaluatedArguments[0]}' is not an integer`, this.operatorName.line, this.operatorName.column);
        }
        else if (!globalInfo.valueIsInteger(evaluatedArguments[1])) {
            throw new evaluation_exception_1.EvaluationException(`EvaluateAux() : The second argument '${evaluatedArguments[1]}' is not an integer`, this.operatorName.line, this.operatorName.column);
        }
        else {
            // return globalInfo.ValueAsInteger(evaluatedArguments[0]) + globalInfo.ValueAsInteger(evaluatedArguments[1]);
            const sum = evaluatedArguments[0] + evaluatedArguments[1];
            // console.log(`OperatorUsage.Evaluate() : Returning sum ${sum}`);
            return sum;
        }
    }
}
exports.OperatorUsage = OperatorUsage;
