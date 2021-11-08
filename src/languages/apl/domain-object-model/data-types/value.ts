// thaw-grammar/src/languages/apl/domain-object-model/data-types/value.ts

// import { product } from 'thaw-common-utilities.ts';

import { EnvironmentFrame } from '../../../../common/domain-object-model/environment-frame';

import { IGlobalInfo } from '../../../../common/domain-object-model/iglobal-info';

import { IAPLValue } from '../interfaces/ivalue';

// export class APLValue<T> implements IAPLValue, IExpression<IAPLValue> {
export class APLValue implements IAPLValue {
	public static createNull(): IAPLValue {
		return new APLValue([], []);
	}

	public static createScalar(n: number): IAPLValue {
		return new APLValue([], [n]);
	}

	public static createVector2(length: number, srcList: number[]): IAPLValue {
		return new APLValue([length], srcList);
	}

	public static createVector1(srcList: number[]): IAPLValue {
		return APLValue.createVector2(srcList.length, srcList);
	}

	public readonly scalars: number[] = [];
	public readonly numberOfContainedScalars: number;
	public readonly steps: number[] = [];

	constructor(public readonly shape: number[], srcList?: number[]) {
		// console.log(`shape (length ${shape.length}) is:`, shape);
		// console.log('srcList is', typeof srcList, srcList);

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
			if (srcList.length === 0 && this.shape.length === 0) {
				// We are constructing a Null value.
				this.numberOfContainedScalars = 0;
			} else {
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
	}

	public get numberOfDimensions(): number {
		return this.shape.length;
	}

	public get isNull(): boolean {
		// TODO: How do we create a null APLValue? And what should a null value's shape be?
		// How about: null APLValue := new APLValue([], []);
		return this.numberOfContainedScalars === 0;
	}

	public get isScalar(): boolean {
		return !this.isNull && this.numberOfDimensions === 0 && this.scalars.length === 1; // TODO? : return this.numberOfContainedScalars === 1; Or this.scalars.length === 1;
	}

	public get valueIfScalar(): number | undefined {
		return this.isScalar ? this.scalars[0] : undefined;
	}

	public get isVector(): boolean {
		return this.numberOfDimensions === 1; // && this.numberOfContainedScalars > 1; ?
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
		return (
			this.shape.length === otherValue.shape.length &&
			this.shape.every((n, i) => n === otherValue.shape[i])
		);
	}

	public /* override */ equals(other: unknown): boolean {
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

		const otherValue = other as IAPLValue;

		return (
			typeof otherValue !== 'undefined' &&
			typeof otherValue.shape !== 'undefined' &&
			typeof otherValue.scalars !== 'undefined' &&
			this.areShapesEqual(otherValue) &&
			otherValue.scalars.length === this.scalars.length &&
			this.scalars.every((n, i) => n === otherValue.scalars[i])
		);
	}

	private valueToString(value: number | IAPLValue): string {
		// if (this is APLValue<double>)
		// {
		// 	return APLGlobalInfo.APLDoubleToString(Convert.ToDouble(value));
		// }

		return value.toString();
	}

	private toStringHelper(
		sb: string,
		offsetVector: number[],
		offset: number,
		firstSlice: boolean
	): string {
		const dimNum = offsetVector.length;

		switch (this.numberOfDimensions - dimNum) {
			case 0:
				sb = sb + this.valueToString(this.scalars[offset]);
				break;

			case 1:
				// sb = sb + string.Join(" ", this.scalars.Skip(offset).Take(this.shape[dimNum]).map(n => this.valueToString(n))));
				sb =
					sb +
					this.scalars
						.slice(offset, offset + this.shape[dimNum])
						.map((n) => this.valueToString(n))
						.join(' ');
				break;

			case 2:
				if (dimNum > 0) {
					if (!firstSlice) {
						// sb.AppendLine();    // This one provides a CR/LF after the previous slice's last row.
						// sb.AppendLine();    // This one provides a blank line between the slices.
						sb = sb + '\n\n';
					}

					// sb = sb + string.Format("Slice ({0}) :", string.Join(", ", offsetVector)));
					sb = sb + `Slice (${offsetVector.join(', ')}) :\n`;
				}

				for (let row = 0; row < this.shape[dimNum]; ++row) {
					if (row > 0) {
						sb = sb + '\n';
					}

					// sb = sb + string.Join(" ", this.scalars.slice(offset).Take(this.shape[dimNum + 1]).map(n => this.valueToString(n)));

					sb =
						sb +
						this.scalars
							.slice(offset, offset + this.shape[dimNum + 1])
							.map((n) => this.valueToString(n))
							.join(' ');

					offset += this.steps[dimNum];
				}

				break;

			default:
				const newOffsetVector = offsetVector.slice(0); // Clone offsetVector

				newOffsetVector.push(0);

				for (let slice = 0; slice < this.shape[dimNum]; ++slice) {
					newOffsetVector[newOffsetVector.length - 1] = slice;
					sb = this.toStringHelper(sb, newOffsetVector, offset, firstSlice);
					offset += this.steps[dimNum];
					firstSlice = false;
				}

				break;
		}

		return sb;
	}

	public /* override */ toString(): string {
		// var sb = new StringBuilder();
		//
		// ToStringHelper(sb, new List<int>(), 0, true);

		return this.toStringHelper('', [], 0, true);

		// return `<APLValue: shape is [${this.shape.join(' ')}]; scalars are [${this.scalars.join(
		// 	' '
		// )}]>`;
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

export function createAPLNullValue(): IAPLValue {
	return APLValue.createNull();
}
