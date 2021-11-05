// tom-weatherhead/thaw-grammar/src/languages/scheme/domain-object-model/scheme-global-info.ts

import { ArgumentException } from 'thaw-interpreter-core';

import { GlobalInfoBase } from '../../../common/domain-object-model/global-info-base';

import { FloatLiteral } from '../../lisp/domain-object-model/float-literal';
import { IntegerLiteral } from '../../lisp/domain-object-model/integer-literal';
// import { INumber } from '../../lisp/domain-object-model/inumber';
import { ISExpression } from '../../lisp/domain-object-model/isexpression';
import { LISPSymbol } from '../../lisp/domain-object-model/lisp-symbol';
import { NullSExpression } from '../../lisp/domain-object-model/null-sexpression';

export class SchemeGlobalInfo extends GlobalInfoBase<ISExpression> {
	private readonly trueValueForAccessor: ISExpression = new LISPSymbol('T'); // Symbols are immutable
	private readonly falseValueForAccessor: ISExpression = new NullSExpression(); // This is immutable too
	// private readonly Dictionary<Name, IMacroDefinition<ISExpression>> MacroDefs = new Dictionary<Name, IMacroDefinition<ISExpression>>();
	// public static readonly Variable<ISExpression> varStackTrace = new Variable<ISExpression>("__STACK_TRACE__", 0, 0);

	constructor() {
		super();
	}

	// public override string LoadPreset(string presetName) {
	// }

	// public override void LoadPresets() {
	// }

	public get falseValue(): ISExpression {
		return this.falseValueForAccessor;
	}

	public get trueValue(): ISExpression {
		return this.trueValueForAccessor;
	}

	public override valueIsFalse(value: ISExpression): boolean {
		return value.isNull();
	}

	// public override Dictionary<Name, IMacroDefinition<ISExpression>> MacroDefinitions
	// {
	// 	get
	// 	{
	// 		return MacroDefs;
	// 	}
	// }

	public valueIsInteger(value: ISExpression): boolean {
		// return (value as IntegerLiteral) !== undefined; // No.

		return value instanceof IntegerLiteral;
	}

	public valueAsInteger(value: ISExpression): number {
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

		if (value instanceof IntegerLiteral) {
			return (value as IntegerLiteral).toInteger();
		} else if (value instanceof FloatLiteral) {
			return (value as FloatLiteral).toInteger();
		} else {
			throw new ArgumentException(
				'valueAsInteger() : The value is neither an IntegerLiteral nor a FloatLiteral.',
				'value'
			);
		}
	}

	public integerAsValue(value: number): ISExpression {
		return new IntegerLiteral(value);
	}

	public valueIsNumber(value: ISExpression): boolean {
		// return (value as IntegerLiteral) !== undefined;

		// return (value as IntegerLiteral) !== undefined || (value as FloatLiteral) !== undefined;

		// return (value as INumber) !== undefined;

		// return value instanceof INumber;

		return value.isNumber();
	}

	public valueAsNumber(value: ISExpression): number {
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

		if (value instanceof IntegerLiteral) {
			return (value as IntegerLiteral).value;
		} else if (value instanceof FloatLiteral) {
			return (value as FloatLiteral).value;
		} else {
			throw new ArgumentException(
				'valueAsNumber() : The value is neither an IntegerLiteral nor a FloatLiteral.',
				'value'
			);
		}
	}

	public numberAsIntegerValue(value: number): ISExpression {
		// Convert to the language's native integer data type
		return new IntegerLiteral(value);
	}

	public numberAsFloatValue(value: number): ISExpression {
		// Convert to the language's native floating-point number data type
		return new FloatLiteral(value);
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

	public override setDebug(debug: boolean): boolean {
		this.debug = debug;

		return true;
	}
}
