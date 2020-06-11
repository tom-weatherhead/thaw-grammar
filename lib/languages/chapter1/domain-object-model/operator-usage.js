"use strict";
// tom-weatherhead/thaw-ll1-parser/src/chapter1/domain-model/operator-usage.ts
Object.defineProperty(exports, "__esModule", { value: true });
exports.Chapter1OperatorUsage = void 0;
const operator_usage_1 = require("../../../common/domain-object-model/operator-usage");
const evaluation_exception_1 = require("../../../common/exceptions/evaluation-exception");
class Chapter1OperatorUsage extends operator_usage_1.OperatorUsage {
    tryGetExpectedNumArgs(globalInfo) {
        switch (this.operatorName.value) {
            // case 'list':
            // 	return -1;  // Any number of arguments is permitted.
            case 'random':
            case 'throw':
                return 1;
            // case 'cons':
            // 	return 2;
            default:
                return super.tryGetExpectedNumArgs(globalInfo);
        }
    }
    evaluateAux(evaluatedArguments, localEnvironment, globalInfo) {
        switch (this.operatorName.value) {
            // 2019-12-22: Hack:
            case '+':
                return evaluatedArguments.reduce((accumulator, evaluatedArgument) => accumulator + evaluatedArgument, 0);
            case '-':
                return evaluatedArguments[0] - evaluatedArguments[1];
            case '*':
                // return evaluatedArguments[0] * evaluatedArguments[1];
                return evaluatedArguments.reduce((accumulator, evaluatedArgument) => accumulator * evaluatedArgument, 1);
            case '/':
                if (evaluatedArguments[1] === 0) {
                    throw new evaluation_exception_1.EvaluationException('Division by zero error', this.operatorName.line, this.operatorName.column);
                }
                return Math.floor(evaluatedArguments[0] / evaluatedArguments[1]);
            case '=':
                return evaluatedArguments[0] === evaluatedArguments[1]
                    ? globalInfo.trueValue
                    : globalInfo.falseValue;
            case '<':
                return evaluatedArguments[0] < evaluatedArguments[1]
                    ? globalInfo.trueValue
                    : globalInfo.falseValue;
            case '>':
                return evaluatedArguments[0] > evaluatedArguments[1]
                    ? globalInfo.trueValue
                    : globalInfo.falseValue;
            case 'print':
                // console.log(evaluatedArguments[0].toString());
                // evaluatedArguments.forEach((evaluatedArgument: number) => {
                // 	console.log(evaluatedArgument.toString());
                // });
                // return globalInfo.getTrueValue();
                globalInfo.print(evaluatedArguments);
                return evaluatedArguments[0];
            case 'random':
                return Math.floor(evaluatedArguments[0] * Math.random());
            case 'throw':
                throw new evaluation_exception_1.EvaluationException('Exception thrown as requested', this.operatorName.line, this.operatorName.column);
            default:
                return super.evaluateAux(evaluatedArguments, localEnvironment, globalInfo); // This handles = for all types
        }
    }
}
exports.Chapter1OperatorUsage = Chapter1OperatorUsage;
