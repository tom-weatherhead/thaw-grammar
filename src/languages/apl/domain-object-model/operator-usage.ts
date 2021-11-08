// thaw-grammar/src/languages/apl/domain-object-model/operator-usage.ts

import { /* ArgumentException, */ Name } from 'thaw-interpreter-core';

import { EnvironmentFrame } from '../../../common/domain-object-model/environment-frame';

import { ExpressionList } from '../../../common/domain-object-model/expression-list';

// import { IExpression } from '../../../common/domain-object-model/iexpression';

import { IGlobalInfo } from '../../../common/domain-object-model/iglobal-info';

import { OperatorUsage } from '../../../common/domain-object-model/operator-usage';

// import { Variable } from '../../../common/domain-object-model/variable';

import { IAPLValue } from './interfaces/ivalue';

import { APLValue } from './data-types/value';

export class APLOperatorUsage extends OperatorUsage<IAPLValue> {
	constructor(operatorName: Name, expressionList: ExpressionList<IAPLValue>) {
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
				/* if (evaluatedArguments[0].IsNull)
				{
					return 'The first argument is null';
				}
				else */ if (!evaluatedArguments[0].isVector) {
					return 'The first argument is not a vector';
				} else if (evaluatedArguments[1].isScalar) {
					/* else if (evaluatedArguments[1].IsNull)
				{
					return 'The second argument is null';
				} */
					return 'The second argument is a scalar';
				} else if (
					(evaluatedArguments[0].getShape() as APLValue).scalars[0] !==
					(evaluatedArguments[1].getShape() as APLValue).scalars[0]
				) {
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
				return (x, y) => (x != 0 || y != 0 ? 1 : 0);
			case 'and':
				return (x, y) => (x != 0 && y != 0 ? 1 : 0);
			case '=':
				return (x, y) => (x == y ? 1 : 0);
			case '<':
				return (x, y) => (x < y ? 1 : 0);
			//case '>': return (x, y) => x > y ? 1 : 0;
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
	// 	switch (operatorName) {
	// 		case 'or':
	// 		case 'and':
	// 		case '=':
	// 		case '<':
	// 			//case '>':
	// 			return true;
	//
	// 		default:
	// 			return false;
	// 	}
	// }

	// private getDyadicOperatorNameFromReductionName(reductionName: string): string {
	// 	switch (reductionName) {
	// 		case '+/':
	// 		case '-/':
	// 		case '*/':
	// 		case '//':
	// 		case 'max/':
	// 		case 'or/':
	// 		case 'and/':
	// 			return reductionName.substring(0, reductionName.length - 1);
	//
	// 		default:
	// 			throw new Error(
	// 				`getDyadicOperatorNameFromReductionName() : Unknown reduction operator '${reductionName}'.`
	// 			);
	// 	}
	// }

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

			console.log(`evaluateDyadicExpressionHelper: result is ${result}.`);

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

			// for (let i = 0; i < arg1AsValue.scalars.length; ++i) {
			// 	newScalars.push(operatorLambda(arg1AsValue.scalars[i], arg2AsValue.scalars[i]));
			// }

			// return new APLValue(arg1AsValue.getShape().scalars, newScalars);

			return new APLValue(arg1.shape, newScalars);
		} else {
			// throw new Error('evaluateDyadicExpressionHelper() : Fscked.');
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

		// if (!arg1.isIntegerScalar) ...

		const op = this.getDyadicIntIntOperator(operatorName);

		return this.evaluateDyadicExpressionHelper(arg1, arg2, op);
	}

	// private void EvaluateReductionExpressionHelper2<T>(APLValue<T> arg, Func<T, T, T> operatorLambda, List<T> newScalars)
	// {
	// 	var shapeVector = arg.GetShape().Scalars;
	//
	// 	if (shapeVector.Count == 1)
	// 	{
	// 		var vector = arg.Scalars;
	// 		var result = vector[vector.Count - 1];
	//
	// 		for (var i = vector.Count - 2; i >= 0; --i)
	// 		{
	// 			result = operatorLambda(vector[i], result);
	// 		}
	//
	// 		newScalars.Add(result);
	// 	}
	// 	else
	// 	{
	//
	// 		for (var i = 1; i <= shapeVector[0]; ++i)
	// 		{
	// 			EvaluateReductionExpressionHelper2<T>(arg.CreateSlice(i), operatorLambda, newScalars);
	// 		}
	// 	}
	// }

	// private APLValue<T> EvaluateReductionExpressionHelper<T>(APLValue<T> arg, Func<T, T, T> operatorLambda)
	// {
	//
	// 	if (arg.IsScalar)
	// 	{
	// 		return arg;
	// 	}
	//
	// 	var newScalars = new List<T>();
	//
	// 	EvaluateReductionExpressionHelper2<T>(arg, operatorLambda, newScalars);
	//
	// 	return new APLValue<T>(arg.GetShape().Scalars.Take(arg.NumberOfDimensions - 1).ToList(), newScalars);
	// }

	// private IAPLValue EvaluateReductionExpression(IAPLValue arg, string operatorName)
	// {
	// 	var dyadicOperatorName = GetDyadicOperatorNameFromReductionName(operatorName);
	// 	IAPLValue result;
	//
	// 	if (arg is APLValue<int>)
	// 	{
	// 		result = EvaluateReductionExpressionHelper((APLValue<int>)arg, GetDyadicIntIntOperator(dyadicOperatorName));
	// 	}
	// 	else
	// 	{
	// 		result = EvaluateReductionExpressionHelper((APLValue<double>)arg, GetDyadicDoubleDoubleOperator(dyadicOperatorName));
	// 	}
	//
	// 	if (!(result is APLValue<int>) && DyadicOperatorMustReturnInt(dyadicOperatorName))
	// 	{
	// 		result = result.ConvertToIntEquivalent();
	// 	}
	//
	// 	return result;
	// }

	// private IAPLValue EvaluateCompressHelper<T2>(APLValue<int> vector1, APLValue<T2> arg2)
	// {
	//
	// 	if (!vector1.Scalars.All(int1 => int1 == 0 || int1 == 1))
	// 	{
	// 		throw new Exception(string.Format("EvaluateCompress() : The vector {0} is not a logical vector.", vector1));
	// 	}
	//
	// 	var numSlices = 0;
	// 	var newScalars = new List<T2>();
	//
	// 	for (int i = 0; i < vector1.Scalars.Count; ++i)
	// 	{
	//
	// 		if (vector1.Scalars[i] != 0)
	// 		{
	// 			newScalars.AddRange(arg2.CreateSlice(i + 1).Scalars);
	// 			++numSlices;
	// 		}
	// 	}
	//
	// 	var newShape = new List<int>(arg2.GetShape().Scalars);
	//
	// 	newShape[0] = numSlices;
	// 	return new APLValue<T2>(newShape, newScalars);
	// }

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

	// private APLValue<T> EvaluateCatHelper<T>(APLValue<T> arg1, APLValue<T> arg2)
	// {
	// 	var arg1Ravel = EvaluateRavelHelper(arg1);
	// 	var arg2Ravel = EvaluateRavelHelper(arg2);
	// 	var newScalars = new List<T>(arg1Ravel.Scalars);
	//
	// 	newScalars.AddRange(arg2Ravel.Scalars);
	//
	// 	return APLValue<T>.CreateVector(newScalars);
	// }

	// private IAPLValue EvaluateCat(IAPLValue arg1, IAPLValue arg2)
	// {
	//
	// 	if (arg1 is APLValue<int> && arg2 is APLValue<int>)
	// 	{
	// 		return EvaluateCatHelper((APLValue<int>)arg1, (APLValue<int>)arg2);
	// 	}
	// 	else if (arg1 is APLValue<double> && arg2 is APLValue<double>)
	// 	{
	// 		return EvaluateCatHelper((APLValue<double>)arg1, (APLValue<double>)arg2);
	// 	}
	// 	else
	// 	{
	// 		throw new Exception("EvaluateCat() : arg1 and arg2 have different element types (int vs. double).");
	// 	}
	// }

	// private IAPLValue EvaluateIndxHelper(APLValue<int> arg)
	// {
	// 	return APLValue<int>.CreateVector(1.To(arg.GetFirstScalar()));
	// }

	// private IAPLValue EvaluateIndx(IAPLValue arg)
	// {
	//
	// 	if (arg is APLValue<int>)
	// 	{
	// 		return EvaluateIndxHelper((APLValue<int>)arg);
	// 	}
	// 	else
	// 	{
	// 		throw new Exception("EvaluateIndx() : arg's element type is not int.");
	// 	}
	// }

	// private void EvaluateTransHelper2<T>(int dimNum, List<int> shape, List<int> steps, int offset, List<T> oldScalars, List<T> newScalars)
	// {
	//
	// 	if (dimNum >= shape.Count)
	// 	{
	// 		newScalars.Add(oldScalars[offset]);
	// 	}
	// 	else
	// 	{
	// 		var length = shape[dimNum];
	// 		var step = steps[dimNum];
	//
	// 		for (var i = 0; i < length; ++i)
	// 		{
	// 			EvaluateTransHelper2<T>(dimNum + 1, shape, steps, offset, oldScalars, newScalars);
	// 			offset += step;
	// 		}
	// 	}
	// }

	// 	private IAPLValue EvaluateTransHelper<T>(APLValue<T> arg)
	// 	{
	// #if DEAD_CODE
	// 		if (arg.NumberOfDimensions != 2) // TODO: Consider supporting more than 2 dimensions in the future, perhaps using CreateSlice().
	// 		{
	// 			return arg;
	// 		}
	//
	// 		var numRows = arg.Shape[0];
	// 		var numColumns = arg.Shape[1];
	// 		var newScalars = new List<T>();
	//
	// 		for (var col = 0; col < numColumns; ++col)
	// 		{
	//
	// 			for (var row = 0; row < numRows; ++row)
	// 			{
	// 				newScalars.Add(arg.Scalars[row * numColumns + col]);
	// 			}
	// 		}
	//
	// 		return new APLValue<T>(new List<int>() { numColumns, numRows }, newScalars);
	// #else
	// 		// 2014/01/20
	//
	// 		if (arg.NumberOfDimensions < 2)
	// 		{
	// 			return arg;
	// 		}
	//
	// 		// Rotate the Shape and Steps vectors.
	// 		var modifiedShape = new List<int>(arg.Shape);
	// 		var modifiedSteps = new List<int>(arg.Steps);
	// 		var lastShape = modifiedShape[modifiedShape.Count - 1];
	// 		var lastStep = modifiedSteps[modifiedSteps.Count - 1];
	// 		var newScalars = new List<T>();
	//
	// 		modifiedShape.RemoveAt(modifiedShape.Count - 1);
	// 		modifiedSteps.RemoveAt(modifiedSteps.Count - 1);
	// 		modifiedShape.Insert(0, lastShape);
	// 		modifiedSteps.Insert(0, lastStep);
	//
	// 		EvaluateTransHelper2<T>(0, modifiedShape, modifiedSteps, 0, arg.Scalars, newScalars);
	//
	// 		return new APLValue<T>(modifiedShape, newScalars);
	// #endif
	// 	}

	// private IAPLValue EvaluateTrans(IAPLValue arg)
	// {
	//
	// 	if (arg is APLValue<int>)
	// 	{
	// 		return EvaluateTransHelper((APLValue<int>)arg);
	// 	}
	// 	else
	// 	{
	// 		return EvaluateTransHelper((APLValue<double>)arg);
	// 	}
	// }

	// private IAPLValue EvaluateSubscriptingHelper<T1>(APLValue<T1> arg1, APLValue<int> arg2)
	// {
	// 	var vector2 = (APLValue<int>)arg2;
	//
	// 	if (arg2.IsIntScalar)
	// 	{
	// 		vector2 = arg2.ToVector();
	// 	}
	//
	// 	var arg1AsValue = (APLValue<T1>)arg1;
	// 	var newScalars = new List<T1>();
	//
	// 	foreach (var n2 in vector2.Scalars)
	// 	{
	// 		newScalars.AddRange(arg1AsValue.CreateSlice(n2).Scalars);
	// 	}
	//
	// 	var newShape = new List<int>(arg1AsValue.GetShape().Scalars);
	//
	// 	newShape[0] = vector2.Scalars.Count;
	//
	// 	return new APLValue<T1>(newShape, newScalars);
	// }

	// private IAPLValue EvaluateSubscripting(IAPLValue arg1, IAPLValue arg2)
	// {
	// 	var arg2AsIntType = arg2 as APLValue<int>;
	//
	// 	if (arg2AsIntType == null)
	// 	{
	// 		throw new Exception("EvaluateSubscripting() : arg2's element type is not int.");
	// 	}
	//
	// 	if (arg1 is APLValue<int>)
	// 	{
	// 		return EvaluateSubscriptingHelper((APLValue<int>)arg1, arg2AsIntType);
	// 	}
	// 	else
	// 	{
	// 		return EvaluateSubscriptingHelper((APLValue<double>)arg1, arg2AsIntType);
	// 	}
	// }

	// private IAPLValue EvaluateDoubleSubscripting(IAPLValue A, IAPLValue B, IAPLValue C)
	// {
	// 	// See Table 3.3 on page 86: ([;] A B C) = (trans ([] (trans ([] A B)) C))
	//
	// 	if (!A.IsMatrix)
	// 	{
	// 		throw new Exception("[;] : A is not a matrix");
	// 	}
	//
	// 	var matrix1 = EvaluateSubscripting(A, B);
	// 	var matrix2 = EvaluateTrans(matrix1);
	// 	var matrix3 = EvaluateSubscripting(matrix2, C);
	//
	// 	return EvaluateTrans(matrix3);
	// }

	// private APLValue<int> EvaluateRandom(APLValue<int> n, APLValue<int> limit)
	// {
	// 	var intList = new List<int>();
	// 	var r = new Random();
	// 	var nValue = n.GetFirstScalar();
	// 	var limitValue = limit.GetFirstScalar();
	//
	// 	for (int i = 0; i < nValue; ++i)
	// 	{
	// 		intList.Add(r.Next(limitValue));
	// 	}
	//
	// 	return APLValue<int>.CreateVector(intList);
	// }

	protected override evaluateAux(
		evaluatedArguments: IAPLValue[],
		localEnvironment: EnvironmentFrame<IAPLValue>,
		globalInfo: IGlobalInfo<IAPLValue>
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
			//case '>':
			case 'pow':
				return this.evaluateDyadicExpression(evaluatedArguments, this.operatorName.value);

			// case 'exp':
			// case 'ln':
			// case 'sin':
			// case 'cos':
			// case 'tan':
			// 	return EvaluateExpLnEtcExpression(evaluatedArguments[0], OperatorName.Value);
			//
			// case '+/':
			// case '-/':
			// case '*/':
			// case '//':
			// case 'max/':
			// case 'or/':
			// case 'and/':
			// 	return EvaluateReductionExpression(evaluatedArguments[0], OperatorName.Value);
			//
			// case 'compress':
			// 	return EvaluateCompress(evaluatedArguments[0], evaluatedArguments[1]);

			case 'shape':
				return evaluatedArguments[0].getShape();

			case 'ravel':
				return this.evaluateRavel(evaluatedArguments[0]);

			case 'restruct':
				return this.evaluateRestruct(evaluatedArguments[0], evaluatedArguments[1]);

			// case 'cat':
			// 	return EvaluateCat(evaluatedArguments[0], evaluatedArguments[1]);
			//
			// case 'indx':
			// 	return EvaluateIndx(evaluatedArguments[0]);
			//
			// case 'trans':
			// 	return EvaluateTrans(evaluatedArguments[0]);
			//
			// case '[]':
			// 	return EvaluateSubscripting(evaluatedArguments[0], evaluatedArguments[1]);
			//
			// case '[;]':
			// 	return EvaluateDoubleSubscripting(evaluatedArguments[0], evaluatedArguments[1], evaluatedArguments[2]);
			//
			// case 'random':
			// 	return EvaluateRandom((APLValue<int>)evaluatedArguments[0], (APLValue<int>)evaluatedArguments[1]);

			default:
				return super.evaluateAux(evaluatedArguments, localEnvironment, globalInfo);
		}
	}
}
