// thaw-grammar/src/languages/apl/domain-object-model/interfaces/ivalue.ts

import { IExpression } from '../../../../common/domain-object-model/iexpression';

export interface IAPLValue extends IExpression<IAPLValue> {
	readonly scalars: number[];

	isNull: boolean;
	isScalar: boolean;
	isVector: boolean;
	isMatrix: boolean;
	isIntegerScalar: boolean;
	isIntegerVector: boolean;

	containsIntegersOnly: boolean;
	isFirstScalarEqualToZero: boolean;
	numberOfDimensions: number;

	areShapesEqual(otherValue: IAPLValue): boolean;
	getFirstScalar(): number;
	getShape(): IAPLValue; // APLValue<int>;
	convertToIntegerEquivalent(): IAPLValue; // APLValue<int>;
	toScalarIfPossible(): IAPLValue;
	toVector(): IAPLValue;
}
