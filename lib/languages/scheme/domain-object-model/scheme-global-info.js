// tom-weatherhead/thaw-grammar/src/languages/scheme/domain-object-model/scheme-global-info.ts
'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
exports.SchemeGlobalInfo = void 0;
const global_info_base_1 = require("../../../common/domain-object-model/global-info-base");
const argument_exception_1 = require("../../../common/exceptions/argument-exception");
const float_literal_1 = require("../../lisp/domain-object-model/float-literal");
const integer_literal_1 = require("../../lisp/domain-object-model/integer-literal");
const lisp_symbol_1 = require("../../lisp/domain-object-model/lisp-symbol");
const null_sexpression_1 = require("../../lisp/domain-object-model/null-sexpression");
class SchemeGlobalInfo extends global_info_base_1.GlobalInfoBase {
    // private readonly Dictionary<Name, IMacroDefinition<ISExpression>> MacroDefs = new Dictionary<Name, IMacroDefinition<ISExpression>>();
    // public static readonly Variable<ISExpression> varStackTrace = new Variable<ISExpression>("__STACK_TRACE__", 0, 0);
    constructor() {
        super();
        this.trueValueForAccessor = new lisp_symbol_1.LISPSymbol('T'); // Symbols are immutable
        this.falseValueForAccessor = new null_sexpression_1.NullSExpression(); // This is immutable too
    }
    // public override string LoadPreset(string presetName) {
    // }
    // public override void LoadPresets() {
    // }
    get falseValue() {
        return this.falseValueForAccessor;
    }
    get trueValue() {
        return this.trueValueForAccessor;
    }
    valueIsFalse(value) {
        return value.isNull();
    }
    // public override Dictionary<Name, IMacroDefinition<ISExpression>> MacroDefinitions
    // {
    // 	get
    // 	{
    // 		return MacroDefs;
    // 	}
    // }
    valueIsInteger(value) {
        // return (value as IntegerLiteral) !== undefined; // No.
        return value instanceof integer_literal_1.IntegerLiteral;
    }
    valueAsInteger(value) {
        // TODO: 2019-12-22 : It looks like we need to combine ISExpression and INumber:
        // const valueAsNumber = value as INumber;
        // const valueAsInt = value as IntegerLiteral;
        /*
        // if (valueAsInt === undefined) {
        if (!(value instanceof IntegerLiteral)) {
            throw new ArgumentException('valueAsInteger() : The value is not an IntegerLiteral.', 'value');
        }

        // return valueAsInt.toInteger();

        return (value as IntegerLiteral).toInteger();
         */
        if (value instanceof integer_literal_1.IntegerLiteral) {
            return value.toInteger();
        }
        else if (value instanceof float_literal_1.FloatLiteral) {
            return value.toInteger();
        }
        else {
            throw new argument_exception_1.ArgumentException('valueAsInteger() : The value is neither an IntegerLiteral nor a FloatLiteral.', 'value');
        }
    }
    integerAsValue(value) {
        return new integer_literal_1.IntegerLiteral(value);
    }
    valueIsNumber(value) {
        // return (value as IntegerLiteral) !== undefined;
        // return (value as IntegerLiteral) !== undefined || (value as FloatLiteral) !== undefined;
        // return (value as INumber) !== undefined;
        // return value instanceof INumber;
        return value.isNumber();
    }
    valueAsNumber(value) {
        // return (value as INumber).toDouble();
        /*
        const i = value as IntegerLiteral;
        const f = value as FloatLiteral;

        if (i !== undefined) {
            return i.value;
        } else if (f !== undefined) {
            return f.value;
        } else {
            throw new ArgumentException('valueAsNumber() : The value is neither an IntegerLiteral nor a FloatLiteral.', 'value');
        }
         */
        if (value instanceof integer_literal_1.IntegerLiteral) {
            return value.value;
        }
        else if (value instanceof float_literal_1.FloatLiteral) {
            return value.value;
        }
        else {
            throw new argument_exception_1.ArgumentException('valueAsNumber() : The value is neither an IntegerLiteral nor a FloatLiteral.', 'value');
        }
    }
    numberAsIntegerValue(value) {
        // Convert to the language's native integer data type
        return new integer_literal_1.IntegerLiteral(value);
    }
    numberAsFloatValue(value) {
        // Convert to the language's native floating-point number data type
        return new float_literal_1.FloatLiteral(value);
    }
    // public static CreateStackTraceInNewEnvironmentFrame(EnvironmentFrame<ISExpression> oldEnvFrame, EnvironmentFrame<ISExpression> newEnvFrame,
    // 	int line, int column): void {
    // 	var oldStackTrace = oldEnvFrame.Lookup(varStackTrace);
    // 	var list1 = new SExpressionList(new IntegerLiteral(column), new NullSExpression());
    // 	var list2 = new SExpressionList(new IntegerLiteral(line), list1);
    // 	var newStackTrace = new SExpressionList(list2, oldStackTrace);
    // 	newEnvFrame.Add(varStackTrace, newStackTrace); // Add(), not AddBubbleDown().
    // 	//Console.WriteLine("CreateStackTraceInNewEnvironmentFrame(): Added (line, column) = ({0}, {1}).", line, column);
    // 	//Console.WriteLine("newStackTrace = {0}", newStackTrace);
    // }
    setDebug(debug) {
        this.debug = debug;
        return true;
    }
}
exports.SchemeGlobalInfo = SchemeGlobalInfo;
