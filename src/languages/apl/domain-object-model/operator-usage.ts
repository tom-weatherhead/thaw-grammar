// thaw-grammar/src/languages/apl/domain-object-model/operator-usage.ts

import { generateFirstNNaturalNumbers } from 'thaw-common-utilities.ts';

import { Name } from 'thaw-interpreter-core';

import { IEnvironmentFrame } from '../../../common/domain-object-model/environment-frame';

import { IExpression } from '../../../common/domain-object-model/iexpression';

import { IGlobalInfo } from '../../../common/domain-object-model/iglobal-info';

import { OperatorUsage } from '../../../common/domain-object-model/operator-usage';

import { IAPLValue } from './interfaces/ivalue';

import { APLValue } from './data-types/value';

export class APLOperatorUsage extends OperatorUsage<IAPLValue> {
	constructor(operatorName: Name, expressionList: IExpression<IAPLValue>[]) {
		super(operatorName, expressionList);
	}

	protected override tryGetExpectedNumArgs(
		globalInfo: IGlobalInfo<IAPLValue>
	): number | undefined {
		switch (this.operatorName.value) {
			case '+/':
			case '-/':
			case '*/':
			case '//':
			case 'max/':
			case 'or/':
			case 'and/':
			case 'shape':
			case 'ravel':
			case 'indx':
			case 'trans':
			case 'exp':
			case 'ln':
			case 'sin':
			case 'cos':
			case 'tan':
				return 1;

			case 'max':
			case 'or':
			case 'and':
			case 'compress':
			case 'restruct':
			case 'cat':
			case '[]':
			case 'random':
			case 'pow':
				return 2;

			case '[;]':
				return 3;

			default:
				return super.tryGetExpectedNumArgs(globalInfo);
		}
	}

	protected override checkArgTypes(evaluatedArguments: IAPLValue[]): string | undefined {
		switch (this.operatorName.value) {
			case '+/':
			case '-/':
			case '*/':
			case '//':
			case 'max/':
			case 'or/':
			case 'and/':
			case 'indx':
				if (evaluatedArguments[0].isNull) {
					return 'Argument is null';
				}

				break;

			case 'compress':
				if (evaluatedArguments[0].isNull) {
					return 'The first argument is null';
				} else if (!evaluatedArguments[0].isVector) {
					return 'The first argument is not a vector';
				} else if (evaluatedArguments[1].isNull) {
					return 'The second argument is null';
				} else if (evaluatedArguments[1].isScalar) {
					return 'The second argument is a scalar';
				} else if (evaluatedArguments[0].shape[0] !== evaluatedArguments[1].shape[0]) {
					return 'The length of the first argument is not equal to the number of slices in the second argument';
				}

				break;

			case '[]':
				if (evaluatedArguments[0].isNull) {
					return 'The first argument is null';
				} else if (evaluatedArguments[0].isScalar) {
					return 'The first argument is a scalar';
				}
				// ThAW 2012/12/10 : It is legal for the second argument to be null; the result will be a null vector or matrix.
				// See SparseVectorTest2().
				else if (evaluatedArguments[1].isMatrix) {
					return 'The second argument is a matrix';
				}

				break;

			case 'random':
				if (!evaluatedArguments[0].isIntegerScalar) {
					return 'The first argument is not an int scalar';
				} else if (!evaluatedArguments[1].isIntegerScalar) {
					return 'The second argument is not an int scalar';
				}

				break;

			default:
				return super.checkArgTypes(evaluatedArguments);
		}

		return undefined;
	}

	// private Func<double, double> GetExpLnEtcOperator(string operatorName)
	// {
	// 	if (DoubleOperatorKeeper.OneArgumentOperators.ContainsKey(operatorName))
	// 	{
	// 		return DoubleOperatorKeeper.OneArgumentOperators[operatorName];
	// 	}
	//
	// 	throw new Exception(string.Format(`GetExpLnOperator() : Unknown operator '{0}'.`, operatorName));
	// }

	// private APLValue<double> EvaluateExpLnEtcExpressionHelper<T>(APLValue<T> arg,
	// 	Func<T, double> conversionLambda, Func<double, double> operatorLambda)
	// {
	// 	var argAsValue = arg.ToScalarIfPossible() as APLValue<T>;
	//
	// 	return new APLValue<double>(argAsValue.Shape, argAsValue.Scalars.Select(n => operatorLambda(conversionLambda(n))).ToList());
	// }

	// private APLValue<double> EvaluateExpLnEtcExpression(IAPLValue arg, string operatorName)
	// {
	// 	var argAsIntType = arg as APLValue<int>;
	// 	var argAsDoubleType = arg as APLValue<double>;
	// 	Func<int, double> lambdaConvertIntToDouble = x => Convert.ToDouble(x);
	// 	Func<double, double> lambdaConvertDoubleToDouble = x => x;
	// 	Func<double, double> lambdaOperator = GetExpLnEtcOperator(operatorName);
	// 	APLValue<double> result;
	//
	// 	if (argAsIntType != null)
	// 	{
	// 		result = EvaluateExpLnEtcExpressionHelper(argAsIntType, lambdaConvertIntToDouble, lambdaOperator);
	// 	}
	// 	else
	// 	{
	// 		result = EvaluateExpLnEtcExpressionHelper(argAsDoubleType, lambdaConvertDoubleToDouble, lambdaOperator);
	// 	}
	//
	// 	return result;
	// }

	private getDyadicIntIntOperator(operatorName: string): (x: number, y: number) => number {
		switch (operatorName) {
			case '+':
				return (x, y) => x + y;
			case '-':
				return (x, y) => x - y;
			case '*':
				return (x, y) => x * y;
			case '/':
				return (x, y) => Math.floor(x / y);
			case 'max':
				return (x, y) => Math.max(x, y);
			case 'or':
				return (x, y) => (x !== 0 || y !== 0 ? 1 : 0);
			case 'and':
				return (x, y) => (x !== 0 && y !== 0 ? 1 : 0);
			case '=':
				return (x, y) => (x === y ? 1 : 0);
			case '<':
				return (x, y) => (x < y ? 1 : 0);
			case '>':
				return (x, y) => (x > y ? 1 : 0);
			// case 'pow': return (x, y) => (int)Math.Pow(x, y);
			default:
				throw new Error(`getDyadicIntIntOperator() : Unknown operator '${operatorName}'.`);
		}
	}

	// private getDyadicDoubleDoubleOperator(operatorName: string): (x: number, y: number) => number {
	//
	// 	switch (operatorName) {
	// 		case "+": return (x, y) => x + y;
	// 		case "-": return (x, y) => x - y;
	// 		case "*": return (x, y) => x * y;
	// 		case "/": return (x, y) => x / y;
	// 		case "max": return (x, y) => Math.Max(x, y);
	// 		case "or": return (x, y) => x != 0.0 || y != 0.0 ? 1.0 : 0.0;
	// 		case "and": return (x, y) => x != 0.0 && y != 0.0 ? 1.0 : 0.0;
	// 		case "=": return (x, y) => x == y ? 1.0 : 0.0;
	// 		case "<": return (x, y) => x < y ? 1.0 : 0.0;
	// 		//case ">": return (x, y) => x > y ? 1.0 : 0.0;
	// 		case "pow": return (x, y) => Math.Pow(x, y);
	// 		default: throw new Exception(string.Format("GetDyadicDoubleDoubleOperator() : Unknown operator '{0}'.", operatorName));
	// 	}
	// }

	// private dyadicOperatorMustReturnInt(operatorName: string): boolean {
	// 	return ['=', '<', '>', 'or', 'and'].indexOf(operatorName) >= 0;
	// }

	private getDyadicOperatorNameFromReductionName(reductionName: string): string {
		if (['+/', '-/', '*/', '//', 'max/', 'or/', 'and/'].indexOf(reductionName) >= 0) {
			return reductionName.substring(0, reductionName.length - 1);
		}

		throw new Error(
			`getDyadicOperatorNameFromReductionName() : Unknown reduction operator '${reductionName}'.`
		);
	}

	private evaluateDyadicExpressionHelper(
		arg1: IAPLValue,
		arg2: IAPLValue,
		// Func<T1, TOut> conversionLambda1,
		// Func<T2, TOut> conversionLambda2,
		// Func<TOut, TOut, TOut> operatorLambda
		operatorLambda: (x: number, y: number) => number
	): IAPLValue {
		// console.log(`evaluateDyadicExpressionHelper: arg1 is ${arg1}; arg2 is ${arg2}.`);

		// TODO: Use valueIfScalar
		const arg1AsValue = arg1.toScalarIfPossible(); // as APLValue<T1>;
		const arg2AsValue = arg2.toScalarIfPossible(); // as APLValue<T2>;

		// console.log(
		// 	`evaluateDyadicExpressionHelper: arg1AsValue is ${arg1AsValue}; arg2AsValue is ${arg2AsValue}.`
		// );
		// console.log(`evaluateDyadicExpressionHelper: arg1AsValue is ${arg1AsValue}.`);

		if (arg1AsValue.isScalar) {
			const fs1 = arg1AsValue.getFirstScalar();
			// const fs2 = arg2AsValue.getFirstScalar();

			// console.log(`evaluateDyadicExpressionHelper: fs1 is ${fs1}; fs2 is ${fs2}.`);
			// console.log(`evaluateDyadicExpressionHelper: fs1 is ${fs1}.`);

			// const resultScalar = operatorLambda(fs1, fs2);

			// console.log(`evaluateDyadicExpressionHelper: resultScalar is ${resultScalar}.`);

			// const result = APLValue.createScalar(resultScalar);

			// const shape2 = arg2.getShape();
			//
			// console.log(`evaluateDyadicExpressionHelper: shape2 is ${shape2}.`);

			const result = new APLValue(
				arg2.shape, // shape2.scalars,
				arg2.scalars.map((n2: number) => operatorLambda(fs1, n2))
			);

			// console.log(`evaluateDyadicExpressionHelper: result is ${result}.`);

			return result;
		} else if (arg2AsValue.isScalar) {
			const fs2 = arg2AsValue.getFirstScalar();

			return new APLValue(
				arg1.shape,
				arg1.scalars.map((n1: number) => operatorLambda(n1, fs2))
			);
		} else if (arg1.areShapesEqual(arg2)) {
			const newScalars = arg1.scalars.map((n1: number, i: number) =>
				operatorLambda(n1, arg2.scalars[i])
			);

			return new APLValue(arg1.shape, newScalars);
		} else {
			throw new Error(
				'Cannot perform a dyadic operation; neither value is a scalar, and the shapes are unequal.'
			);
		}

		// if (arg1AsValue.isScalar) {
		// 	// var convertedValue1 = conversionLambda1(arg1AsValue.GetFirstScalar());
		//
		// 	// return new APLValue<TOut>(arg2AsValue.GetShape().Scalars,
		// 	// 	arg2AsValue.Scalars.Select(n2 => operatorLambda(convertedValue1, conversionLambda2(n2))).ToList());
		//
		// 	return new APLValue(
		// 		arg2AsValue.getShape().scalars,
		// 		arg2AsValue.scalars.map((n2: number) =>
		// 			operatorLambda(arg1AsValue.getFirstScalar(), n2)
		// 		)
		// 	);
		// } else if (arg2AsValue.isScalar) {
		// 	// var convertedValue2 = conversionLambda2(arg2AsValue.GetFirstScalar());
		//
		// 	// return new APLValue<TOut>(arg1AsValue.GetShape().Scalars,
		// 	// 	arg1AsValue.Scalars.Select(n1 => operatorLambda(conversionLambda1(n1), convertedValue2)).ToList());
		//
		// 	return new APLValue(
		// 		arg1AsValue.getShape().scalars,
		// 		arg1AsValue.scalars.map((n1: number) =>
		// 			operatorLambda(n1, arg2AsValue.getFirstScalar())
		// 		)
		// 	);
		// } else if (arg1AsValue.areShapesEqual(arg2AsValue)) {
		// 	const newScalars: number[] = [];
		//
		// 	for (let i = 0; i < arg1AsValue.scalars.length; ++i) {
		// 		newScalars.push(operatorLambda(arg1AsValue.scalars[i], arg2AsValue.scalars[i]));
		// 	}
		//
		// 	return new APLValue(arg1AsValue.getShape().scalars, newScalars);
		// } else {
		// 	throw new Error(
		// 		'Cannot perform a dyadic operation; neither value is a scalar, and the shapes are unequal.'
		// 	);
		// }
	}

	// private IAPLValue EvaluateDyadicExpression(List<IAPLValue> evaluatedArguments, string operatorName)
	// {
	// 	var arg1 = evaluatedArguments[0];
	// 	var arg2 = evaluatedArguments[1];
	// 	var arg1AsIntType = arg1 as APLValue<int>;
	// 	var arg2AsIntType = arg2 as APLValue<int>;
	// 	var arg1AsDoubleType = arg1 as APLValue<double>;
	// 	var arg2AsDoubleType = arg2 as APLValue<double>;
	// 	Func<int, double> lambdaConvertIntToDouble = x => Convert.ToDouble(x);
	// 	Func<int, int> lambdaConvertIntToInt = x => x;
	// 	Func<double, double> lambdaConvertDoubleToDouble = x => x;
	// 	Func<double, double, double> lambdaDoubleOperator = GetDyadicDoubleDoubleOperator(operatorName);
	// 	IAPLValue result;
	//
	// 	if (arg1AsIntType != null)
	// 	{
	//
	// 		if (arg2AsIntType != null)
	// 		{
	// 			result = EvaluateDyadicExpressionHelper(arg1AsIntType, arg2AsIntType,
	// 				lambdaConvertIntToInt, lambdaConvertIntToInt, GetDyadicIntIntOperator(operatorName));
	// 		}
	// 		else
	// 		{
	// 			result = EvaluateDyadicExpressionHelper(arg1AsIntType, arg2AsDoubleType,
	// 				lambdaConvertIntToDouble, lambdaConvertDoubleToDouble, lambdaDoubleOperator);
	// 		}
	// 	}
	// 	else
	// 	{
	//
	// 		if (arg2AsIntType != null)
	// 		{
	// 			result = EvaluateDyadicExpressionHelper(arg1AsDoubleType, arg2AsIntType,
	// 				lambdaConvertDoubleToDouble, lambdaConvertIntToDouble, lambdaDoubleOperator);
	// 		}
	// 		else
	// 		{
	// 			result = EvaluateDyadicExpressionHelper(arg1AsDoubleType, arg2AsDoubleType,
	// 				lambdaConvertDoubleToDouble, lambdaConvertDoubleToDouble, lambdaDoubleOperator);
	// 		}
	// 	}
	//
	// 	if (!(result is APLValue<int>) && DyadicOperatorMustReturnInt(operatorName))
	// 	{
	// 		result = result.ConvertToIntEquivalent();
	// 	}
	//
	// 	return result;
	// }

	private evaluateDyadicExpression(
		evaluatedArguments: IAPLValue[],
		operatorName: string
	): IAPLValue {
		const arg1 = evaluatedArguments[0];
		const arg2 = evaluatedArguments[1];

		const op = this.getDyadicIntIntOperator(operatorName);

		return this.evaluateDyadicExpressionHelper(arg1, arg2, op);
	}

	private evaluateReductionExpressionHelper2(
		arg: IAPLValue,
		operatorLambda: (x: number, y: number) => number,
		newScalars: number[]
	): void {
		const shapeVector = arg.shape;

		if (shapeVector.length === 1) {
			const scalarsClone = arg.scalars.slice(0);
			const lastScalar = scalarsClone.pop();

			if (typeof lastScalar === 'undefined') {
				throw new Error(
					'evaluateReductionExpressionHelper2() : Called .pop() on an empty array.'
				);
			}

			newScalars.push(
				scalarsClone.reduceRight((x: number, y: number) => operatorLambda(y, x), lastScalar)
			);
		} else {
			for (let i = 1; i <= shapeVector[0]; ++i) {
				this.evaluateReductionExpressionHelper2(
					arg.createSlice(i),
					operatorLambda,
					newScalars
				);
			}
		}
	}

	private evaluateReductionExpressionHelper(
		arg: IAPLValue,
		operatorLambda: (x: number, y: number) => number
	): IAPLValue {
		if (arg.isScalar) {
			return arg;
		}

		const newScalars: number[] = [];

		this.evaluateReductionExpressionHelper2(arg, operatorLambda, newScalars);

		return new APLValue(arg.shape.slice(0, arg.numberOfDimensions - 1), newScalars);
	}

	private evaluateReductionExpression(arg: IAPLValue, operatorName: string): IAPLValue {
		const dyadicOperatorName = this.getDyadicOperatorNameFromReductionName(operatorName);
		// IAPLValue result;
		// let result: IAPLValue;

		// if (arg is APLValue<int>)
		// {
		// 	result = EvaluateReductionExpressionHelper((APLValue<int>)arg, GetDyadicIntIntOperator(dyadicOperatorName));
		// }
		// else
		// {
		// 	result = EvaluateReductionExpressionHelper((APLValue<double>)arg, GetDyadicDoubleDoubleOperator(dyadicOperatorName));
		// }

		/* const result = */
		return this.evaluateReductionExpressionHelper(
			arg,
			this.getDyadicIntIntOperator(dyadicOperatorName)
		);

		// if (!(result is APLValue<int>) && DyadicOperatorMustReturnInt(dyadicOperatorName))
		// {
		// 	result = result.ConvertToIntEquivalent();
		// }

		// return result;
	}

	private evaluateCompressHelper(vector1: IAPLValue, arg2: IAPLValue): IAPLValue {
		if (!vector1.scalars.every((int1) => int1 === 0 || int1 === 1)) {
			throw new Error(`evaluateCompress() : The vector ${vector1} is not a logical vector.`);
		}

		let numSlices = 0;
		let newScalars: number[] = [];

		for (let i = 0; i < vector1.scalars.length; ++i) {
			if (vector1.scalars[i] !== 0) {
				newScalars = newScalars.concat(arg2.createSlice(i + 1).scalars);
				++numSlices;
			}
		}

		const newShape = arg2.getShape().scalars.slice(0);

		newShape[0] = numSlices;

		return new APLValue(newShape, newScalars);
	}

	// private IAPLValue EvaluateCompress(IAPLValue arg1, IAPLValue arg2)
	// {
	// 	var vector1 = arg1 as APLValue<int>;
	//
	// 	if (vector1 == null)
	// 	{
	// 		vector1 = (APLValue<int>)arg1.ConvertToIntEquivalent();
	// 	}
	//
	// 	if (arg2 is APLValue<int>)
	// 	{
	// 		return EvaluateCompressHelper(vector1, (APLValue<int>)arg2);
	// 	}
	// 	else
	// 	{
	// 		return EvaluateCompressHelper(vector1, (APLValue<double>)arg2);
	// 	}
	// }
	private evaluateCompress(vector1: IAPLValue, arg2: IAPLValue): IAPLValue {
		return this.evaluateCompressHelper(vector1, arg2);
	}

	private evaluateRavelHelper(arg: IAPLValue): IAPLValue {
		return arg.toVector();
	}

	private evaluateRavel(arg: IAPLValue): IAPLValue {
		//
		// 	if (arg is APLValue<int>)
		// 	{
		// 		return EvaluateRavelHelper((APLValue<int>)arg);
		// 	}
		// 	else
		// 	{
		// 		return EvaluateRavelHelper((APLValue<double>)arg);
		// 	}

		return this.evaluateRavelHelper(arg);
	}

	private evaluateRestructHelper(arg1: IAPLValue, arg2: IAPLValue): IAPLValue {
		if (arg1.isMatrix) {
			throw new Error('EvaluateRestruct() : First argument must not be a matrix.');
		}

		if (arg2.isNull) {
			throw new Error('EvaluateRestruct() : Second argument must not be null.');
		}

		const shapeVector = arg1.toVector();
		const arg2Ravel = this.evaluateRavelHelper(arg2);

		return new APLValue(shapeVector.scalars, arg2Ravel.scalars);
	}

	private evaluateRestruct(arg1: IAPLValue, arg2: IAPLValue): IAPLValue {
		// var arg1AsIntType = arg1 as APLValue<int>;
		//
		// if (arg1AsIntType == null)
		// {
		// 	throw new Exception("EvaluateRestruct() : arg1 is not an APLValue<int>");
		// }
		//
		// if (arg2 is APLValue<int>)
		// {
		// 	return EvaluateRestructHelper(arg1AsIntType, (APLValue<int>)arg2);
		// }
		// else
		// {
		// 	return EvaluateRestructHelper(arg1AsIntType, (APLValue<double>)arg2);
		// }

		return this.evaluateRestructHelper(arg1, arg2);
	}

	private evaluateCatHelper(arg1: IAPLValue, arg2: IAPLValue): IAPLValue {
		return APLValue.createVector1(arg1.scalars.concat(arg2.scalars));
	}

	private evaluateCat(arg1: IAPLValue, arg2: IAPLValue): IAPLValue {
		// if (arg1 is APLValue<int> && arg2 is APLValue<int>)
		// {
		// 	return EvaluateCatHelper((APLValue<int>)arg1, (APLValue<int>)arg2);
		// }
		// else if (arg1 is APLValue<double> && arg2 is APLValue<double>)
		// {
		// 	return EvaluateCatHelper((APLValue<double>)arg1, (APLValue<double>)arg2);
		// }
		// else
		// {
		// 	throw new Exception("EvaluateCat() : arg1 and arg2 have different element types (int vs. double).");
		// }

		return this.evaluateCatHelper(arg1, arg2);
	}

	private evaluateIndxHelper(arg: IAPLValue): IAPLValue {
		return APLValue.createVector1(generateFirstNNaturalNumbers(arg.getFirstScalar()));
	}

	private evaluateIndx(arg: IAPLValue): IAPLValue {
		// if (arg is APLValue<int>)
		// {
		// 	return EvaluateIndxHelper((APLValue<int>)arg);
		// }
		// else
		// {
		// 	throw new Exception("EvaluateIndx() : arg's element type is not int.");
		// }

		return this.evaluateIndxHelper(arg);
	}

	// private void EvaluateTransHelper2<T>(int dimNum, List<int> shape, List<int> steps, int offset, List<T> oldScalars, List<T> newScalars)
	// {
	private evaluateTransHelper2(
		dimNum: number,
		shape: number[],
		steps: number[],
		offset: number,
		oldScalars: number[],
		newScalars: number[]
	): void {
		if (dimNum >= shape.length) {
			newScalars.push(oldScalars[offset]);
		} else {
			const length = shape[dimNum];
			const step = steps[dimNum];

			for (let i = 0; i < length; ++i) {
				this.evaluateTransHelper2(dimNum + 1, shape, steps, offset, oldScalars, newScalars);
				offset += step;
			}
		}
	}

	private evaluateTransHelper(arg: IAPLValue): IAPLValue {
		// 2014/01/20

		if (arg.numberOfDimensions < 2) {
			return arg;
		}

		// Rotate the Shape and Steps vectors.
		const modifiedShape = arg.shape.slice(0);
		const modifiedSteps = arg.steps.slice(0);
		const lastShape = modifiedShape.pop();
		const lastStep = modifiedSteps.pop();
		const newScalars: number[] = [];

		if (typeof lastShape === 'undefined' || typeof lastStep === 'undefined') {
			throw new Error('evaluateTransHelper() : Shape or steps is empty.');
		}

		modifiedShape.unshift(lastShape);
		modifiedSteps.unshift(lastStep);

		this.evaluateTransHelper2(0, modifiedShape, modifiedSteps, 0, arg.scalars, newScalars);

		return new APLValue(modifiedShape, newScalars);
	}

	private evaluateTrans(arg: IAPLValue): IAPLValue {
		// if (arg is APLValue<int>)
		// {
		// 	return EvaluateTransHelper((APLValue<int>)arg);
		// }
		// else
		// {
		// 	return EvaluateTransHelper((APLValue<double>)arg);
		// }

		return this.evaluateTransHelper(arg);
	}

	private evaluateSubscriptingHelper(arg1: IAPLValue, arg2: IAPLValue): IAPLValue {
		let vector2 = arg2;

		if (arg2.isIntegerScalar) {
			vector2 = arg2.toVector();
		}

		const arg1AsValue = arg1;
		let newScalars: number[] = [];

		for (const n2 of vector2.scalars) {
			newScalars = newScalars.concat(arg1AsValue.createSlice(n2).scalars);
		}

		const newShape = arg1AsValue.getShape().scalars.slice(0); // Clone the array

		newShape[0] = vector2.scalars.length;

		return new APLValue(newShape, newScalars);
	}

	private evaluateSubscripting(arg1: IAPLValue, arg2: IAPLValue): IAPLValue {
		// var arg2AsIntType = arg2 as APLValue<int>;
		//
		// if (arg2AsIntType == null)
		// {
		// 	throw new Exception("EvaluateSubscripting() : arg2's element type is not int.");
		// }
		//
		// if (arg1 is APLValue<int>)
		// {
		// 	return EvaluateSubscriptingHelper((APLValue<int>)arg1, arg2AsIntType);
		// }
		// else
		// {
		// 	return EvaluateSubscriptingHelper((APLValue<double>)arg1, arg2AsIntType);
		// }

		return this.evaluateSubscriptingHelper(arg1, arg2);
	}

	private evaluateDoubleSubscripting(a: IAPLValue, b: IAPLValue, c: IAPLValue): IAPLValue {
		// See Table 3.3 on page 86: ([;] A B C) = (trans ([] (trans ([] A B)) C))

		if (!a.isMatrix) {
			throw new Error('[;] : a is not a matrix');
		}

		const matrix1 = this.evaluateSubscripting(a, b);
		const matrix2 = this.evaluateTrans(matrix1);
		const matrix3 = this.evaluateSubscripting(matrix2, c);

		return this.evaluateTrans(matrix3);
	}

	private evaluateRandom(n: IAPLValue, limit: IAPLValue): IAPLValue {
		const intList: number[] = [];
		// var r = new Random();
		const nValue = n.getFirstScalar();
		const limitValue = limit.getFirstScalar();

		for (let i = 0; i < nValue; ++i) {
			intList.push(Math.floor(limitValue * Math.random()));
		}

		return APLValue.createVector1(intList);
	}

	// protected override evaluateAux(
	// 	evaluatedArguments: IAPLValue[],
	// 	localEnvironment: EnvironmentFrame<IAPLValue>,
	// 	globalInfo: IGlobalInfo<IAPLValue>
	// ): IAPLValue {
	protected override evaluateAux(
		evaluatedArguments: IAPLValue[],
		globalInfo: IGlobalInfo<IAPLValue>,
		localEnvironment?: IEnvironmentFrame<IAPLValue>,
		options?: unknown
	): IAPLValue {
		switch (this.operatorName.value) {
			case '+':
			case '-':
			case '*':
			case '/':
			case 'max':
			case 'or':
			case 'and':
			case '=': // Note: E.g. (= '(2 3 5 7) '(2 3 5 7)) yields '(1 1 1 1), not 1
			case '<':
			case '>':
			case 'pow':
				return this.evaluateDyadicExpression(evaluatedArguments, this.operatorName.value);

			// case 'exp':
			// case 'ln':
			// case 'sin':
			// case 'cos':
			// case 'tan':
			// 	return this.evaluateExpLnEtcExpression(evaluatedArguments[0], this.operatorName.value);

			case '+/':
			case '-/':
			case '*/':
			case '//':
			case 'max/':
			case 'or/':
			case 'and/':
				return this.evaluateReductionExpression(
					evaluatedArguments[0],
					this.operatorName.value
				);

			case 'compress':
				return this.evaluateCompress(evaluatedArguments[0], evaluatedArguments[1]);

			case 'shape':
				return evaluatedArguments[0].getShape();

			case 'ravel':
				return this.evaluateRavel(evaluatedArguments[0]);

			case 'restruct':
				return this.evaluateRestruct(evaluatedArguments[0], evaluatedArguments[1]);

			case 'cat':
				return this.evaluateCat(evaluatedArguments[0], evaluatedArguments[1]);

			case 'indx':
				return this.evaluateIndx(evaluatedArguments[0]);

			case 'trans':
				return this.evaluateTrans(evaluatedArguments[0]);

			case '[]':
				return this.evaluateSubscripting(evaluatedArguments[0], evaluatedArguments[1]);

			case '[;]':
				return this.evaluateDoubleSubscripting(
					evaluatedArguments[0],
					evaluatedArguments[1],
					evaluatedArguments[2]
				);

			case 'random':
				return this.evaluateRandom(evaluatedArguments[0], evaluatedArguments[1]);

			default:
				return super.evaluateAux(evaluatedArguments, globalInfo, localEnvironment, options);
		}
	}
}
