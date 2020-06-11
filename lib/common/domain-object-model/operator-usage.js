// tom-weatherhead/thaw-grammar/src/common/domain-object-model/operator-usage.ts
'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
exports.OperatorUsage = void 0;
const evaluation_exception_1 = require("../exceptions/evaluation-exception");
const environment_frame_1 = require("./environment-frame");
class OperatorUsage {
    constructor(operatorName, expressionList) {
        this.twoArgumentIntegerPredicates = new Map();
        this.twoArgumentIntegerOperators = new Map();
        this.operatorName = operatorName;
        this.expressionList = expressionList;
        this.twoArgumentIntegerPredicates.set('<', (operand1, operand2) => operand1 < operand2);
        this.twoArgumentIntegerPredicates.set('>', (operand1, operand2) => operand1 > operand2);
        this.twoArgumentIntegerOperators.set('+', (operand1, operand2) => operand1 + operand2);
        this.twoArgumentIntegerOperators.set('-', (operand1, operand2) => operand1 - operand2);
        this.twoArgumentIntegerOperators.set('*', (operand1, operand2) => operand1 * operand2);
        this.twoArgumentIntegerOperators.set('/', (operand1, operand2) => operand1 / operand2);
    }
    toString() {
        if (this.expressionList.value.length === 0) {
            return `(${this.operatorName})`;
        }
        return `(${this.operatorName} ${this.expressionList})`;
    }
    // This is virtual because Scheme.PrimOp overrides it.
    evaluate(localEnvironment, globalInfo) {
        const actualNumArgs = this.expressionList.value.length;
        const expectedNumArgs = this.tryGetExpectedNumArgs(globalInfo);
        if (expectedNumArgs === undefined) {
            throw new evaluation_exception_1.EvaluationException(`OperatorUsage : Unknown operator name '${this.operatorName.value}`, this.operatorName.line, this.operatorName.column);
        }
        else if (expectedNumArgs >= 0 &&
            actualNumArgs !== expectedNumArgs) {
            throw new evaluation_exception_1.EvaluationException(`OperatorUsage : Expected ${expectedNumArgs} argument(s) for operator '${this.operatorName.value}', instead of the actual ${actualNumArgs} argument(s)`, this.operatorName.line, this.operatorName.column);
        }
        // T macroResult;
        // if (TryInvokeMacro(expressionList.value, localEnvironment, globalInfo, out macroResult))
        // {
        // 	return macroResult;
        // }
        const evaluatedArguments = this.expressionList.value.map((expr) => expr.evaluate(localEnvironment, globalInfo));
        // var argTypesErrorMessage = CheckArgTypes(evaluatedArguments);
        // if (!string.IsNullOrEmpty(argTypesErrorMessage))
        // {
        // 	throw new EvaluationException(
        // 		string.Format("Operator '{0}': {1}", operatorName.Value, argTypesErrorMessage),
        // 		operatorName.Line, operatorName.Column);
        // }
        return this.evaluateAux(evaluatedArguments, localEnvironment, globalInfo);
    }
    tryGetExpectedNumArgs(globalInfo) {
        if (['<', '>', '+', '-', '*', '/'].indexOf(this.operatorName.value) >=
            0) {
            return 2;
        }
        const fnDefRaw = globalInfo.functionDefinitions.get(this.operatorName.value);
        // const macroDef = globalInfo.MacroDefinitions.get(this.operatorName);
        switch (this.operatorName.value) {
            case 'print':
                return -1; // Was 1. print now takes any number of arguments.
            case '=':
                return 2;
            default:
                if (fnDefRaw !== undefined) {
                    const fnDef = fnDefRaw;
                    return fnDef.argList.value.length;
                    // } else if (globalInfo.MacroDefinitions != null && globalInfo.MacroDefinitions.ContainsKey(this.operatorName)) {
                    // 	return globalInfo.MacroDefinitions[operatorName].ArgumentCount;
                }
                else {
                    return undefined;
                }
        }
    }
    checkArgTypes(evaluatedArguments) {
        return null;
    }
    // protected virtual bool TryInvokeMacro(
    // 	List<IExpression<T>> unevaluatedArguments,
    // 	EnvironmentFrame<T> localEnvironment,
    // 	IGlobalInfo<T> globalInfo,
    // 	out T macroResult)
    // {
    // 	macroResult = default(T);
    // 	return false;
    // }
    // protected virtual void UpdateStackTrace(EnvironmentFrame<T> oldEnvFrame, EnvironmentFrame<T> newEnvFrame,
    // 	int line, int column)
    // {
    // }
    evaluateAux(evaluatedArguments, localEnvironment, globalInfo) {
        const firstArgAsInt = evaluatedArguments.length > 0 &&
            globalInfo.valueIsInteger(evaluatedArguments[0])
            ? globalInfo.valueAsInteger(evaluatedArguments[0])
            : 0;
        const secondArgAsInt = evaluatedArguments.length > 1 &&
            globalInfo.valueIsInteger(evaluatedArguments[1])
            ? globalInfo.valueAsInteger(evaluatedArguments[1])
            : 0;
        const twoArgumentIntegerPredicateRaw = this.twoArgumentIntegerPredicates.get(this.operatorName.value);
        const twoArgumentIntegerOperatorRaw = this.twoArgumentIntegerOperators.get(this.operatorName.value);
        // if (IntegerOperatorKeeper.TwoArgumentOperators.ContainsKey(this.operatorName.Value))
        if (typeof twoArgumentIntegerOperatorRaw !== 'undefined') {
            // return globalInfo.IntegerAsValue(IntegerOperatorKeeper.TwoArgumentOperators[operatorName.Value](firstArgAsInt, secondArgAsInt));
            return globalInfo.integerAsValue(twoArgumentIntegerOperatorRaw(firstArgAsInt, secondArgAsInt));
            // } else if (IntegerOperatorKeeper.TwoArgumentPredicates.ContainsKey(this.operatorName.Value))
        }
        else if (typeof twoArgumentIntegerPredicateRaw !== 'undefined') {
            // return IntegerOperatorKeeper.TwoArgumentPredicates[operatorName.Value](firstArgAsInt, secondArgAsInt) ? globalInfo.TrueValue : globalInfo.FalseValue;
            return twoArgumentIntegerPredicateRaw(firstArgAsInt, secondArgAsInt)
                ? globalInfo.trueValue
                : globalInfo.falseValue;
        }
        const fnDefRaw = globalInfo.functionDefinitions.get(this.operatorName.value);
        switch (this.operatorName.value) {
            case '=':
                return evaluatedArguments[0] === evaluatedArguments[1]
                    ? globalInfo.trueValue
                    : globalInfo.falseValue;
            case 'print':
                globalInfo.print(evaluatedArguments);
                return evaluatedArguments[0];
            default:
                if (typeof fnDefRaw !== 'undefined') {
                    // Evaluate a user-defined function.
                    const newEnvironment = new environment_frame_1.EnvironmentFrame(globalInfo.dynamicScoping
                        ? localEnvironment
                        : globalInfo.globalEnvironment);
                    // if (globalInfo.Debug)
                    // {
                    // 	UpdateStackTrace(localEnvironment, newEnvironment, operatorName.Line, operatorName.Column);
                    // }
                    const fnDef = fnDefRaw;
                    newEnvironment.compose(fnDef.argList.value, evaluatedArguments);
                    return fnDef.body.evaluate(newEnvironment, globalInfo);
                }
                throw new evaluation_exception_1.EvaluationException(`EvaluateAux() : Unknown operator name '${this.operatorName.value}'`, this.operatorName.line, this.operatorName.column);
        }
    }
}
exports.OperatorUsage = OperatorUsage;
