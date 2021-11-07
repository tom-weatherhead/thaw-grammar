// thaw-grammar/src/languages/apl/domain-object-model/data-types/value.ts

// import { product } from 'thaw-common-utilities.ts';

import { EnvironmentFrame } from '../../../../common/domain-object-model/environment-frame';

import { IGlobalInfo } from '../../../../common/domain-object-model/iglobal-info';

import { IAPLValue } from '../interfaces/ivalue';

// export class APLValue<T> implements IAPLValue, IExpression<IAPLValue> {
export class APLValue implements IAPLValue {
	public static createScalar(n: number): IAPLValue {
		return new APLValue([], [n]);
	}

	public static createVector2(length: number, srcList: number[]): IAPLValue {
		return new APLValue([length], srcList);
	}

	public static createVector1(srcList: number[]): IAPLValue {
		return APLValue.createVector2(srcList.length, srcList);
	}

	// public readonly scalars: T[] = [];
	public readonly scalars: number[] = [];
	// public readonly shape: number[] = [];
	public readonly numberOfContainedScalars: number;
	public readonly steps: number[] = [];

	constructor(public readonly shape: number[], srcList?: number[]) {
		console.log(`shape (length ${shape.length}) is:`, shape);
		console.log('srcList is', typeof srcList, srcList);

		if (this.shape.some((s) => s < 0)) {
			throw new Error(
				'APLValue constructor: Shape vector contains one or more negative elements'
			);
		}

		// Shape.AddRange(srcShape);

		this.numberOfContainedScalars = 1;

		for (const s of this.shape) {
			this.numberOfContainedScalars *= s;
		}
		// this.numberOfContainedScalars = product(...this.shape);

		if (this.shape.length > 0) {
			this.steps.push(1);
		}

		for (let i = this.shape.length - 1; i > 0; --i) {
			this.steps.unshift(this.steps[0] * this.shape[i]);
		}

		if (this.shape.length !== this.steps.length) {
			throw new Error(
				'APLValue constructor: The Shape vector and the Steps vector have different lengths'
			);
		}

		if (typeof srcList !== 'undefined') {
			if (this.numberOfContainedScalars === 0) {
				throw new Error('APLValue constructor: numberOfContainedScalars === 0');
			}

			if (srcList.length == 0) {
				throw new Error('APLValue constructor: srcList is empty');
			}

			let srcIndex = 0;

			while (this.scalars.length < this.numberOfContainedScalars) {
				this.scalars.push(srcList[srcIndex]);

				if (++srcIndex >= srcList.length) {
					srcIndex = 0;
				}
			}
		}
	}

	public get numberOfDimensions(): number {
		return this.shape.length;
	}

	public get isNull(): boolean {
		return this.numberOfContainedScalars === 0;
	}

	public get isScalar(): boolean {
		return this.numberOfDimensions === 0;
	}

	public get isVector(): boolean {
		return this.numberOfDimensions === 1;
	}

	public get isMatrix(): boolean {
		return this.numberOfDimensions >= 2;
	}

	public get isIntegerScalar(): boolean {
		return this.isScalar && this.containsIntegersOnly;
	}

	public get isIntegerVector(): boolean {
		return this.isVector && this.containsIntegersOnly;
	}

	public get containsIntegersOnly(): boolean {
		return this.scalars.every((n) => !Number.isNaN(n) && Math.round(n) === n);
	}

	public areShapesEqual(otherValue: IAPLValue): boolean {
		// const otherShape = (otherValue.getShape() as APLValue).scalars;

		// if (this.shape.length !== otherShape.length) {
		// 	return false;
		// }
		//
		// for (var i = 0; i < this.shape.length; ++i)
		// {
		//
		// 	if (Shape[i] != otherShape[i])
		// 	{
		// 		return false;
		// 	}
		// }

		return (
			this.shape.length === otherValue.shape.length &&
			this.shape.every((n, i) => n === otherValue.shape[i])
		);
	}

	// public override bool Equals(object obj)
	// {
	//
	// 	if (object.ReferenceEquals(this, obj))
	// 	{
	// 		return true;
	// 	}
	//
	// 	var otherValue = obj as APLValue<T>;
	//
	// 	if (otherValue == null || !AreShapesEqual(otherValue))
	// 	{
	// 		return false;
	// 	}
	//
	// 	for (var i = 0; i < Scalars.Count; ++i)
	// 	{
	//
	// 		if (!Scalars[i].Equals(otherValue.Scalars[i]))
	// 		{
	// 			return false;
	// 		}
	// 	}
	//
	// 	return true;
	// }

	// private string ValueToString(T value)
	// {
	//
	// 	if (this is APLValue<double>)
	// 	{
	// 		return APLGlobalInfo.APLDoubleToString(Convert.ToDouble(value));
	// 	}
	//
	// 	return value.ToString();
	// }

	// private void ToStringHelper(StringBuilder sb, List<int> offsetVector, int offset, bool firstSlice)
	// {
	// 	var dimNum = offsetVector.Count;
	//
	// 	switch (NumberOfDimensions - dimNum)
	// 	{
	// 		case 0:
	// 			sb.Append(ValueToString(Scalars[offset]));
	// 			break;
	//
	// 		case 1:
	// 			sb.Append(string.Join(" ", Scalars.Skip(offset).Take(Shape[dimNum]).Select(n => ValueToString(n))));
	// 			break;
	//
	// 		case 2:
	//
	// 			if (dimNum > 0)
	// 			{
	//
	// 				if (!firstSlice)
	// 				{
	// 					sb.AppendLine();    // This one provides a CR/LF after the previous slice's last row.
	// 					sb.AppendLine();    // This one provides a blank line between the slices.
	// 				}
	//
	// 				sb.AppendLine(string.Format("Slice ({0}) :", string.Join(", ", offsetVector)));
	// 			}
	//
	// 			for (var row = 0; row < Shape[dimNum]; ++row)
	// 			{
	//
	// 				if (row > 0)
	// 				{
	// 					sb.AppendLine();
	// 				}
	//
	// 				sb.Append(string.Join(" ", Scalars.Skip(offset).Take(Shape[dimNum + 1]).Select(n => ValueToString(n))));
	// 				offset += Steps[dimNum];
	// 			}
	//
	// 			break;
	//
	// 		default:
	// 			var newOffsetVector = new List<int>(offsetVector);
	//
	// 			newOffsetVector.Add(0);
	//
	// 			for (var slice = 0; slice < Shape[dimNum]; ++slice)
	// 			{
	// 				newOffsetVector[newOffsetVector.Count - 1] = slice;
	// 				ToStringHelper(sb, newOffsetVector, offset, firstSlice);
	// 				offset += Steps[dimNum];
	// 				firstSlice = false;
	// 			}
	//
	// 			break;
	// 	}
	// }

	public /* override */ toString(): string {
		// var sb = new StringBuilder();
		//
		// ToStringHelper(sb, new List<int>(), 0, true);
		// return sb.ToString();

		// return '<APLValue>';

		return `<APLValue: shape is [${this.shape.join(' ')}]; scalars are [${this.scalars.join(
			' '
		)}]>`;
	}

	public getShape(): IAPLValue {
		return APLValue.createVector1(this.shape);
	}

	public getFirstScalar(): number {
		if (this.isNull) {
			throw new Error('APLValue.getFirstScalar: Value is APL null');
		}

		return this.scalars[0];
	}

	public get isFirstScalarEqualToZero(): boolean {
		return this.getFirstScalar() === 0; // .equals(default(T));
	}

	public convertToIntegerEquivalent(): IAPLValue {
		// if (this is APLValue<int>)
		// {
		// 	return this as APLValue<int>;
		// }

		if (this.containsIntegersOnly) {
			return this;
		}

		// return new APLValue<int>(Shape, Scalars.Select(n => Convert.ToInt32(n)).ToList());

		return new APLValue(
			this.shape,
			this.scalars.map((n) => Math.round(n))
		);
	}

	public toScalarIfPossible(): IAPLValue {
		if (this.numberOfContainedScalars !== 1) {
			return this;
		}

		return APLValue.createScalar(this.getFirstScalar());
	}

	public toVector(): IAPLValue {
		// I.e. ravel
		return APLValue.createVector2(this.numberOfContainedScalars, this.scalars);
	}

	// public APLValue<T> CreateSlice(int n)
	// {
	//
	// 	if (NumberOfDimensions == 0)
	// 	{
	// 		throw new Exception("APLValue<T>.CreateSlice() : Cannot create a slice of a scalar");
	// 	}
	// 	else if (n <= 0 || n > Shape[0])
	// 	{
	// 		throw new Exception(string.Format("APLValue<T>.CreateSlice() : Index out of bounds: {0} is not in the range from 1 to {1}",
	// 			n, Shape[0]));
	// 	}
	//
	// 	var step = Steps[0];
	//
	// 	return new APLValue<T>(Shape.Skip(1).ToList(), Scalars.Skip((n - 1) * step).Take(step).ToList());
	// }

	/* eslint-disable @typescript-eslint/no-unused-vars */
	public evaluate(
		localEnvironment: EnvironmentFrame<IAPLValue>,
		globalInfo: IGlobalInfo<IAPLValue>
	): IAPLValue {
		return this;
	}
	/* eslint-enable @typescript-eslint/no-unused-vars */
}
