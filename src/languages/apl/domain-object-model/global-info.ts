// thaw-grammar/src/languages/apl/domain-object-model/global-info.ts

import { IParser, ITokenizer } from 'thaw-interpreter-types';

import { ArgumentException } from 'thaw-interpreter-core';

import { GlobalInfoBase } from '../../../common/domain-object-model/global-info-base';

import { IAPLValue } from './interfaces/ivalue';

import { APLValue } from './data-types/value';

export class APLGlobalInfo extends GlobalInfoBase<IAPLValue> {
	public readonly trueVal = APLValue.createScalar(1);
	public readonly falseVal = APLValue.createScalar(0);

	constructor(
		options: {
			parser?: IParser;
			tokenizer?: ITokenizer;
		} = {}
	) {
		super(options);
	}

	// 	public override string LoadPreset(string presetName)
	// 	{
	//
	// 		switch (presetName.ToLower())
	// 		{
	// 			case "min":
	// 				// min functions (from page 70)
	// 				Evaluate("(define neg (v) (- 0 v))");
	// 				Evaluate("(define min (v1 v2) (neg (max (neg v1) (neg v2))))");
	// 				Evaluate("(define min/ (v) (neg (max/ (neg v))))");
	// 				break;
	//
	// 			case "reverse":
	// 				// From page 71
	// 				Evaluate(@"
	// (define reverse (a)
	// (let ((size ([] (shape a) 1)))
	// 	([] a (+1 (- size (indx size))))))");
	// 				break;
	//
	// 			case "find":
	// 				// From page 72
	// 				LoadPreset("min");
	// 				//Evaluate("(define signum (x) (+ (* (< x 0) -1) (> x 0)))");
	// 				Evaluate("(define signum (x) (- (> x 0) (< x 0)))"); // ThAW 2014/01/24
	// 				Evaluate("(define abs (x) (* x (signum x)))");
	// 				Evaluate("(define find (x v) ([] (compress (= x v) (indx (shape v))) 1))");
	// 				Evaluate(@"
	// (define find-closest (x v)
	// (let ((absdiffs (abs (- v x))))
	// 	(find (min/ absdiffs) absdiffs)))");
	// 				break;
	//
	// 			case @"+\":
	// 				// +\ ("+-scan") functions (from page 74)
	// 				Evaluate("(define dropend (v) ([] v (indx (- (shape v) 1))))");
	// 				Evaluate(@"
	// (define +\ (v)
	// (if (= (shape v) 0) v
	// 	(cat (+\ (dropend v)) (+/ v))))");
	// 				break;
	//
	// 			case "<=":
	// 				Evaluate("(define <= (x y) (or (< x y) (= x y)))");
	// 				break;
	//
	// 			default:
	// 				return base.LoadPreset(presetName);
	// 		}
	//
	// 		return string.Format("The preset '{0}' has been successfully loaded.", presetName);
	// 	}

	// public override void LoadPresets()
	// {
	// 	// Define values for unit testing here.
	// 	Evaluate("(set testvector1 '(1 1 2 3))");
	// 	Evaluate("(set testvector2 '(5 8 13 21))");
	// 	Evaluate("(set logicalvector3 '(1 0 1))");
	// 	Evaluate("(set logicalvector4 '(0 1 1 0))");
	// 	Evaluate("(set floatvector5 '(1.0 1.25 1.5 1.75))");
	// 	Evaluate("(set floatvector6 '(2.0 3.5 5.0 7.5))");
	// 	Evaluate("(set testmatrix1 (restruct '(3 4) '(1 2 3 4 5 6 7 8 9 10 11 12)))");
	// 	Evaluate("(set testmatrix2 (restruct '(3 4) '(2 3 5 7 11 13 17 19 23 29 31 37)))");
	// 	Evaluate("(set logicalmatrix3 (restruct '(3 4) '(1 1 1 1 0 0 0 0 1 1 1 1)))");
	// 	Evaluate("(set logicalmatrix4 (restruct '(3 4) '(0 1 0 1 0 1 0 1 0 1 0 1)))");
	// 	Evaluate("(set floatmatrix5 (restruct '(3 4) '(1.0 2.5 3.0 4.5 5.0 6.5 7.0 8.5 9.0 10.5 11.0 12.5)))");
	// 	Evaluate("(set floatmatrix6 (restruct '(3 4) '(2.0 3.0 5.5 7.5 11.0 13.0 17.5 19.5 23.0 29.0 31.5 37.5)))");
	//
	// 	GlobalEnvironment.Add(new Variable<IAPLValue>("e", 0, 0), APLValue<double>.CreateScalar(Math.E));
	// 	GlobalEnvironment.Add(new Variable<IAPLValue>("pi", 0, 0), APLValue<double>.CreateScalar(Math.PI));
	//
	// 	// Define commonly-used functions here.
	// 	Evaluate("(define > (x y) (< y x))");
	// 	Evaluate("(define mod (m n) (- m (* n (/ m n))))");
	// 	Evaluate("(define +1 (n) (+ n 1))");
	// 	/* TODO:
	// 	Evaluate("(define )");
	// 	 */
	// }

	public override get falseValue(): IAPLValue {
		return this.falseVal;
	}

	public override get trueValue(): IAPLValue {
		return this.trueVal;
	}

	public override valueIsFalse(value: IAPLValue): boolean {
		return value.isFirstScalarEqualToZero;
	}

	public override valueIsInteger(value: IAPLValue): boolean {
		return value.isIntegerScalar;
	}

	public override valueAsInteger(value: IAPLValue): number {
		const valueAsScalar = value as APLValue;

		if (!valueAsScalar.isIntegerScalar) {
			throw new ArgumentException(
				'valueAsInteger() : The value is not an integer scalar.',
				'valueAsScalar'
			);
		} else if (valueAsScalar.isNull) {
			throw new ArgumentException(
				'valueAsInteger() : The value is a null scalar.',
				'valueAsScalar'
			);
		}

		return valueAsScalar.getFirstScalar();
	}

	public override integerAsValue(value: number): IAPLValue {
		return APLValue.createScalar(value);
	}

	// public static string APLDoubleToString(double d)
	// {
	// 	// E.g. If d == 13.0, we want the string to be "13.0" rather than just "13", so that we can distinguish it from an integer.
	// 	// Note: d == (tan (/ pi 4)) is a good test case; it should yield "1.0", not "1".
	// 	var result = d.ToString();
	//
	// 	if (result.IndexOf('.') < 0 && result.IndexOf('E') < 0) // I.e. If result does not contain either '.' or 'E'.
	// 	{
	// 		result = result + ".0";
	// 	}
	//
	// 	return result;
	// }
}
