// thaw-grammar/src/languages/apl/domain-object-model/interfaces/ivalue.ts

import { IExpression } from '../../../../common/domain-object-model/iexpression';

export interface IAPLValue extends IExpression<IAPLValue> {
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
	getShape(): IAPLValue; // APLValue<int>;
	convertToIntegerEquivalent(): IAPLValue; // APLValue<int>;
}
