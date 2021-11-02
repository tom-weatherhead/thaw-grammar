// tom-weatherhead/thaw-grammar/src/languages/smalltalk/domain-object-model/integer.ts

// public interface ISmalltalkNumber
// {
// 	int ToInteger();
// 	double ToDouble();
// }

// public abstract class SmalltalkNumberBase : SmalltalkValueBase, ISmalltalkNumber
// {
// 	protected SmalltalkNumberBase(SmalltalkClass owner)
// 		: base(owner)
// 	{
// 	}
//
// 	public override bool IsNumber()
// 	{
// 		return true;
// 	}
//
// 	public abstract int ToInteger();
// 	public abstract double ToDouble();
// }

// SmalltalkIntegerValue objects are immutable.

// public class SmalltalkIntegerValue : SmalltalkNumberBase
// {
// 	public readonly int Value;
//
// 	public SmalltalkIntegerValue(int value)
// 		: base(SmalltalkObjectClassKeeper.ObjectClass)
// 	{
// 		Value = value;
// 	}
//
// 	public override string ToString()
// 	{
// 		return Value.ToString();
// 	}
//
// 	public override bool Equals(object obj)
// 	{
//
// 		if (object.ReferenceEquals(this, obj))
// 		{
// 			return true;
// 		}
//
// 		var otherIntVal = obj as SmalltalkIntegerValue;
//
// 		return otherIntVal != null && Value == otherIntVal.Value;
// 	}
//
// 	public override int GetHashCode()
// 	{
// 		return Value.GetHashCode();
// 	}
//
// 	public override string GetTypename()
// 	{
// 		return "int";
// 	}
//
// 	public override int ToInteger()
// 	{
// 		return Value;
// 	}
//
// 	public override double ToDouble()
// 	{
// 		return Convert.ToDouble(Value);
// 	}
//
// 	/*
// 	public override ISmalltalkValue Evaluate(SmalltalkEnvironmentFrame localEnvironment, ISmalltalkValue receiver, SmalltalkClass c, SmalltalkGlobalInfo globalInfo)
// 	{
// 		return this;
// 	}
// 	 */
// }

// SmalltalkFloatValue objects are immutable.

// public class SmalltalkFloatValue : SmalltalkNumberBase
// {
// 	public readonly double Value;
//
// 	public SmalltalkFloatValue(double value)
// 		: base(SmalltalkObjectClassKeeper.ObjectClass)
// 	{
// 		/*
// 		if (!(value is double))
// 		{
// 			throw new ArgumentException("FloatLiteral constructor: value is not a double.", "value");
// 		}
//
// 		Value = (double)value;
// 		 */
// 		Value = value;
// 	}
//
// 	public override string ToString()
// 	{
// 		// E.g. If d == 13.0, we want the string to be "13.0" rather than just "13", so that we can distinguish it from an integer.
// 		// Note: d == (tan (/ pi 4)) is a good test case; it should yield "1.0", not "1".
// 		var result = Value.ToString();
//
// 		if (result.IndexOf('.') < 0 && result.IndexOf('E') < 0) // I.e. If result does not contain either '.' or 'E'.
// 		{
// 			result = result + ".0";
// 		}
//
// 		return result;
// 	}
//
// 	public override bool Equals(object obj)
// 	{
//
// 		if (object.ReferenceEquals(this, obj))
// 		{
// 			return true;
// 		}
//
// 		SmalltalkFloatValue otherFltVal = obj as SmalltalkFloatValue;
//
// 		return otherFltVal != null && Value == otherFltVal.Value;
// 	}
//
// 	public override int GetHashCode()
// 	{
// 		return Value.GetHashCode();
// 	}
//
// 	public override string GetTypename()
// 	{
// 		return "float";
// 	}
//
// 	public override int ToInteger()
// 	{
// 		return Convert.ToInt32(Math.Floor(Value));
// 	}
//
// 	public override double ToDouble()
// 	{
// 		return Value;
// 	}
//
// 	/*
// 	public override ISmalltalkValue Evaluate(SmalltalkEnvironmentFrame localEnvironment, ISmalltalkValue receiver, SmalltalkClass c, SmalltalkGlobalInfo globalInfo)
// 	{
// 		return this;
// 	}
// 	 */
// }

// import { EnvironmentFrame } from '../../../common/domain-object-model/environment-frame';
// import { IExpression } from '../../../common/domain-object-model/iexpression';
// import { IGlobalInfo } from '../../../common/domain-object-model/iglobal-info';
import { ArgumentException } from '../../../common/exceptions/argument-exception';

// import { ISmalltalkClass, ISmalltalkEnvironmentFrame, ISmalltalkExpression, ISmalltalkGlobalInfo, ISmalltalkValue } from './interfaces/iexpression';
import {
	ISmalltalkExpression,
	ISmalltalkUserValue,
	ISmalltalkValue
} from './interfaces/iexpression';

// import { ISmalltalkValue } from './interfaces/ivalue';

import { SmalltalkValueBase } from './value-base';

export class SmalltalkIntegerValue
	extends SmalltalkValueBase
	implements ISmalltalkExpression, ISmalltalkValue
{
	public readonly value: number;
	// public readonly line: number;
	// public readonly column: number;

	constructor(value: unknown, public readonly line = 0, public readonly column = 0) {
		super();

		if (typeof value !== 'number') {
			throw new ArgumentException(
				`SmalltalkIntegerValue constructor: typeof value is not 'number'; it is '${typeof value}'.`,
				'value'
			);
		} else if (Number.isNaN(value)) {
			throw new ArgumentException(
				'SmalltalkIntegerValue constructor: value is not a number (NaN).',
				'value'
			);
		} else if (Math.floor(value) !== value) {
			throw new ArgumentException(
				'SmalltalkIntegerValue constructor: value is not an integer.',
				'value'
			);
		}

		this.value = value as number;
		// this.line = line;
		// this.column = column;
	}

	public override toString(): string {
		// Do not allow the output to be formatted as scientific notation.

		return `${this.value}`;
	}

	public override getTypename(): string {
		return 'int';
	}

	public override isNumber(): boolean {
		return true;
	}

	public override get isInteger(): boolean {
		return true;
	}

	public toInteger(): number | undefined {
		return this.value;
	}

	public toFloat(): number | undefined {
		return this.value;
	}

	public toStringX(): string | undefined {
		return undefined;
	}

	public toUserValue(): ISmalltalkUserValue | undefined {
		return undefined;
	}

	/* eslint-disable @typescript-eslint/no-unused-vars */
	// public evaluate(
	// 	localEnvironment: ISmalltalkEnvironmentFrame | undefined,
	// 	receiver: ISmalltalkValue, c: ISmalltalkClass, globalInfo: ISmalltalkGlobalInfo
	// ): ISmalltalkValue {
	// 	return this;
	// }
	/* eslint-enable @typescript-eslint/no-unused-vars */
}
