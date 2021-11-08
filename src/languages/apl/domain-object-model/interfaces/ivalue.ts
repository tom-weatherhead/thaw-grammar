// thaw-grammar/src/languages/apl/domain-object-model/interfaces/ivalue.ts

import { IEqualityComparable, IStringifiable } from 'thaw-common-utilities.ts';

import { IExpression } from '../../../../common/domain-object-model/iexpression';

export interface IAPLValue extends IEqualityComparable, IExpression<IAPLValue>, IStringifiable {
	readonly scalars: number[];
	readonly shape: number[];

	isNull: boolean;
	isScalar: boolean;
	isVector: boolean;
	isMatrix: boolean;
	isIntegerScalar: boolean;
	isIntegerVector: boolean;

	containsIntegersOnly: boolean;
	isFirstScalarEqualToZero: boolean;
	numberOfDimensions: number;
	valueIfScalar: number | undefined;

	areShapesEqual(otherValue: IAPLValue): boolean;
	getFirstScalar(): number;
	getShape(): IAPLValue; // APLValue<int>;
	convertToIntegerEquivalent(): IAPLValue; // APLValue<int>;
	toScalarIfPossible(): IAPLValue;
	toVector(): IAPLValue;
}

export type IAPLExpression = IExpression<IAPLValue>;
