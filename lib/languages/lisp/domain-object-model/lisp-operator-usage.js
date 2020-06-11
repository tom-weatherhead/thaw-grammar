// tom-weatherhead/thaw-grammar/src/languages/lisp/domain-object-model/lisp-operator-usage.ts
'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
exports.LISPOperatorUsage = void 0;
const thaw_common_utilities_ts_1 = require("thaw-common-utilities.ts");
const operator_usage_1 = require("../../../common/domain-object-model/operator-usage");
const evaluation_exception_1 = require("../../../common/exceptions/evaluation-exception");
const lisp_exception_1 = require("../exceptions/lisp-exception");
const integer_literal_1 = require("./integer-literal");
const lisp_string_1 = require("./lisp-string");
const sexpression_list_1 = require("./sexpression-list");
class LISPOperatorUsage extends operator_usage_1.OperatorUsage {
    constructor(operatorName, expressionList) {
        super(operatorName, expressionList);
        this.operatorsThatTakeEitherIntOrFloatArgs = new thaw_common_utilities_ts_1.Set();
        this.operatorsThatTakeEitherIntOrFloatArgs.add('<');
        // this.operatorsThatTakeEitherIntOrFloatArgs.add('>');
        this.operatorsThatTakeEitherIntOrFloatArgs.add('+');
        this.operatorsThatTakeEitherIntOrFloatArgs.add('-');
        this.operatorsThatTakeEitherIntOrFloatArgs.add('*');
        this.operatorsThatTakeEitherIntOrFloatArgs.add('/');
    }
    // TODO: Rename this function to getExpectedNumArgs()
    tryGetExpectedNumArgs(globalInfo) {
        // if (DoubleOperatorKeeper.OneArgumentOperators.ContainsKey(OperatorName.Value))
        // {
        // 	result = 1;
        // 	return true;
        // }
        // else if (DoubleOperatorKeeper.TwoArgumentOperators.ContainsKey(OperatorName.Value) ||
        // 	DoubleOperatorKeeper.TwoArgumentPredicates.ContainsKey(OperatorName.Value))
        // {
        // 	result = 2;
        // 	return true;
        // }
        switch (this.operatorName.value) {
            case 'list':
                return -1; // Any number of arguments is permitted.
            case 'car':
            case 'cdr':
            case 'number?':
            case 'symbol?':
            case 'list?':
            case 'null?':
            case 'string?':
            case 'random':
            case 'tostring':
            case 'listtostring':
            case 'stringtolist':
            case 'stringtosymbol':
            case 'floor':
            case 'throw':
                return 1;
            case 'cons':
            case 'rplaca':
            case 'rplacd':
            case 'string<':
                return 2;
            default:
                return super.tryGetExpectedNumArgs(globalInfo);
        }
    }
    // private bool IsListOfStrings(ISExpression arg)
    // {
    // 	if (arg is NullSExpression)
    // 	{
    // 		return true;
    // 	}
    // 	var argAsList = arg as SExpressionList;
    // 	if (argAsList == null || !(argAsList.Head is LISPString))
    // 	{
    // 		return false;
    // 	}
    // return IsListOfStrings(argAsList.Tail);
    // }
    checkArgTypes(evaluatedArguments) {
        switch (this.operatorName.value) {
            case 'number?':
            case 'symbol?':
            case 'list?':
            case 'null?':
            case 'string?':
                return null; // Type predicates take one argument of any type.
            case '+':
            case '-':
            case '*':
            case '/':
            case '<':
            // case '>':
            case 'pow':
            case 'atan2':
                if (!evaluatedArguments[0].isNumber()) {
                    return 'First argument is not a number';
                }
                if (!evaluatedArguments[1].isNumber()) {
                    return 'Second argument is not a number';
                }
                break;
            case 'car':
            case 'cdr':
            case 'rplaca':
            case 'rplacd':
                if (!evaluatedArguments[0].isList()) {
                    // throw new ArgumentException(string.Format("Operator {0} : Argument '{1}' of type {2} is not a list.",
                    //    OperatorName.Value, evaluatedArguments[0], evaluatedArguments[0].GetType().FullName));
                    return `Argument '${evaluatedArguments[0]}' of type ??? is not a list`;
                    // evaluatedArguments[0], evaluatedArguments[0].GetType().FullName);
                }
                break;
            case 'random':
            case 'exp':
            case 'ln':
            case 'sin':
            case 'cos':
            case 'tan':
            case 'floor':
                if (!evaluatedArguments[0].isNumber()) {
                    // throw new ArgumentException(string.Format("Operator {0} : Argument is not a number.", OperatorName.Value));
                    return 'Argument is not a number';
                }
                break;
            case 'listtostring':
                // if (!IsListOfStrings(evaluatedArguments[0]))
                if (!evaluatedArguments[0].isList() &&
                    !evaluatedArguments[0].isNull()) {
                    // throw new ArgumentException(string.Format("Operator {0} : Argument is not a list or a null.", OperatorName.Value));
                    return 'Argument is not a list or a null';
                }
                break;
            case 'stringtolist':
            case 'stringtosymbol':
            case 'throw':
                if (!evaluatedArguments[0].isString()) {
                    // throw new ArgumentException(string.Format("Operator {0} : Argument is not a string.", OperatorName.Value));
                    return 'Argument is not a string';
                }
                break;
            case 'string<':
                if (!evaluatedArguments[0].isString()) {
                    return 'First argument is not a string';
                }
                if (!evaluatedArguments[1].isString()) {
                    return 'Second argument is not a string';
                }
                break;
            default:
                // **** TODO 2019-12-21 : Continue translating here ****
                return super.checkArgTypes(evaluatedArguments);
        }
        return null;
    }
    // protected TryInvokeMacro(
    // 	List<IExpression<ISExpression>> unevaluatedArguments,
    // 	EnvironmentFrame<ISExpression> localEnvironment,
    // 	IGlobalInfo<ISExpression> globalInfo,
    // 	out ISExpression macroResult): boolean {
    // 	if (!globalInfo.MacroDefinitions.ContainsKey(OperatorName))
    // 	{
    // 		macroResult = null;
    // 		return false;
    // 	}
    // 	macroResult = globalInfo.MacroDefinitions[OperatorName].InvokeMacro(unevaluatedArguments, localEnvironment, globalInfo);
    // 	return true;
    // }
    // private static ListToStringHelper(arg: ISExpression, sb: string): void {
    // 	if (arg is NullSExpression) {
    // 		return;
    // 	}
    // 	var argAsList = arg as SExpressionList;
    // 	if (argAsList === null) {
    // 		sb.Append(arg.ToString());  // This should never get called.
    // 	} else {
    // 		sb.Append(argAsList.Head.ToString());
    // 		ListToStringHelper(argAsList.Tail, sb);
    // 	}
    // }
    // private static ISExpression StringToListHelper(string str, int i) {
    // 	if (i >= str.Length)
    // 	{
    // 		return new NullSExpression();
    // 	}
    // 	return new SExpressionList(
    // 		new LISPString(str.Substring(i, 1)),
    // 		StringToListHelper(str, i + 1));
    // }
    // protected override void UpdateStackTrace(EnvironmentFrame<ISExpression> oldEnvFrame, EnvironmentFrame<ISExpression> newEnvFrame,
    // 	int line, int column)
    // {
    // 	LISPGlobalInfo.CreateStackTraceInNewEnvironmentFrame(oldEnvFrame, newEnvFrame, line, column);
    // }
    // private ISExpression EvaluateAuxFloat(List<ISExpression> evaluatedArguments, EnvironmentFrame<ISExpression> localEnvironment, IGlobalInfo<ISExpression> globalInfo)
    // {
    // 	// Currently, EvaluateAuxFloat() is only called for two-argument functions.
    // 	var firstArgAsDouble = ((INumber)evaluatedArguments[0]).ToDouble();
    // 	var secondArgAsDouble = ((INumber)evaluatedArguments[1]).ToDouble();
    // 	if (DoubleOperatorKeeper.TwoArgumentOperators.ContainsKey(OperatorName.Value))
    // 	{
    // 		return new FloatLiteral(DoubleOperatorKeeper.TwoArgumentOperators[OperatorName.Value](firstArgAsDouble, secondArgAsDouble));
    // 	}
    // 	else if (DoubleOperatorKeeper.TwoArgumentPredicates.ContainsKey(OperatorName.Value))
    // 	{
    // 		return DoubleOperatorKeeper.TwoArgumentPredicates[OperatorName.Value](firstArgAsDouble, secondArgAsDouble) ? globalInfo.TrueValue : globalInfo.FalseValue;
    // 	}
    // 	throw new Exception(string.Format("LISPOperatorUsage.EvaluateAuxFloat() : Invalid operator {0}", OperatorName.Value));
    // }
    evaluateAux(evaluatedArguments, localEnvironment, globalInfo) {
        let sExprList;
        switch (this.operatorName.value) {
            // 2019-12-22: Hack:
            // case '+':
            // 	return new IntegerLiteral((evaluatedArguments[0] as IntegerLiteral).value + (evaluatedArguments[1] as IntegerLiteral).value);
            // 2019-12-22: Hack:
            case '+':
                return new integer_literal_1.IntegerLiteral(evaluatedArguments.reduce((accumulator, evaluatedArgument) => accumulator +
                    evaluatedArgument.value, 0));
            case '-':
                // return evaluatedArguments[0] - evaluatedArguments[1];
                return new integer_literal_1.IntegerLiteral(evaluatedArguments[0].value -
                    evaluatedArguments[1].value);
            case '*':
                return new integer_literal_1.IntegerLiteral(evaluatedArguments.reduce((accumulator, evaluatedArgument) => accumulator *
                    evaluatedArgument.value, 1));
            case '/':
                if (evaluatedArguments[1].value === 0) {
                    throw new evaluation_exception_1.EvaluationException('Division by zero error', this.operatorName.line, this.operatorName.column);
                }
                return new integer_literal_1.IntegerLiteral(Math.floor(evaluatedArguments[0].value /
                    evaluatedArguments[1].value));
            case '=':
                return evaluatedArguments[0].value ===
                    evaluatedArguments[1].value
                    ? globalInfo.trueValue
                    : globalInfo.falseValue;
            case '<':
                return evaluatedArguments[0].value <
                    evaluatedArguments[1].value
                    ? globalInfo.trueValue
                    : globalInfo.falseValue;
            case '>':
                return evaluatedArguments[0].value >
                    evaluatedArguments[1].value
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
                return new integer_literal_1.IntegerLiteral(Math.floor(evaluatedArguments[0].value *
                    Math.random()));
            case 'floor':
                // Or: return globalInfo.valueAsInteger(evaluatedArguments[0]);
                return new integer_literal_1.IntegerLiteral(Math.floor(evaluatedArguments[0].value));
            case 'throw':
                throw new lisp_exception_1.LISPException('Exception thrown as requested', this.operatorName.line, this.operatorName.column);
            case 'cons':
                return new sexpression_list_1.SExpressionList(evaluatedArguments[0], evaluatedArguments[1]);
            case 'car':
                sExprList = evaluatedArguments[0];
                return sExprList.head;
            case 'cdr':
                sExprList = evaluatedArguments[0];
                return sExprList.tail;
            case 'rplaca':
                sExprList = evaluatedArguments[0];
                sExprList.head = evaluatedArguments[1];
                return sExprList.head;
            case 'rplacd':
                sExprList = evaluatedArguments[0];
                sExprList.tail = evaluatedArguments[1];
                return sExprList.tail;
            case 'number?':
                return evaluatedArguments[0].isNumber()
                    ? globalInfo.trueValue
                    : globalInfo.falseValue;
            case 'symbol?':
                return evaluatedArguments[0].isSymbol()
                    ? globalInfo.trueValue
                    : globalInfo.falseValue;
            case 'list?':
                return evaluatedArguments[0].isList()
                    ? globalInfo.trueValue
                    : globalInfo.falseValue;
            case 'null?':
                return evaluatedArguments[0].isNull()
                    ? globalInfo.trueValue
                    : globalInfo.falseValue;
            case 'string?':
                return evaluatedArguments[0].isString()
                    ? globalInfo.trueValue
                    : globalInfo.falseValue;
            case 'list':
                return sexpression_list_1.SExpressionList.makeFromList(evaluatedArguments);
            case 'tostring':
                return new lisp_string_1.LISPString(`${evaluatedArguments[0]}`);
            // case 'listtostring':
            // 	var sbListToString = new StringBuilder();
            // 	ListToStringHelper(evaluatedArguments[0], sbListToString);
            // 	return new LISPString(sbListToString.ToString());
            // case 'stringtolist':
            // 	var str2list = (LISPString)evaluatedArguments[0];
            // 	return StringToListHelper(str2list.Value, 0);
            // case 'stringtosymbol':
            // 	var str2sym = (LISPString)evaluatedArguments[0];
            // 	return new LISPSymbol(str2sym.Value);
            // case 'string<': // See page 54.
            // 	return ((LISPString)evaluatedArguments[0]).Value.CompareTo(((LISPString)evaluatedArguments[1]).Value) < 0
            // 		? globalInfo.TrueValue : globalInfo.FalseValue;
            default:
                // if (operatorsThatTakeEitherIntOrFloatArgs.Contains(OperatorName.Value))
                // {
                // 	if (evaluatedArguments.Any(arg => arg is FloatLiteral))
                // 	{
                // 		return EvaluateAuxFloat(evaluatedArguments, localEnvironment, globalInfo); ;
                // 	}
                // 	else
                // 	{
                // 		// This line is a copy of a line below, but we must handle this here
                // 		// so that we don't fall through to the DoubleOperatorKeeper.TwoArgumentOperators case.
                // 		return base.EvaluateAux(evaluatedArguments, localEnvironment, globalInfo);
                // 	}
                // }
                // // The next two cases involve operators that must take arguments as doubles, not ints.
                // else if (DoubleOperatorKeeper.OneArgumentOperators.ContainsKey(OperatorName.Value))
                // {
                // 	return new FloatLiteral(DoubleOperatorKeeper.OneArgumentOperators[OperatorName.Value](((INumber)evaluatedArguments[0]).ToDouble()));
                // }
                // else if (DoubleOperatorKeeper.TwoArgumentOperators.ContainsKey(OperatorName.Value))
                // {
                // 	return new FloatLiteral(DoubleOperatorKeeper.TwoArgumentOperators[OperatorName.Value](
                // 		((INumber)evaluatedArguments[0]).ToDouble(),
                // 		((INumber)evaluatedArguments[1]).ToDouble()));
                // }
                return super.evaluateAux(evaluatedArguments, localEnvironment, globalInfo); // This handles = for all types
        }
    }
}
exports.LISPOperatorUsage = LISPOperatorUsage;
