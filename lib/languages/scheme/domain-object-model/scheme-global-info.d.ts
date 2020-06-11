import { GlobalInfoBase } from '../../../common/domain-object-model/global-info-base';
import { ISExpression } from '../../lisp/domain-object-model/isexpression';
export declare class SchemeGlobalInfo extends GlobalInfoBase<ISExpression> {
    private readonly trueValueForAccessor;
    private readonly falseValueForAccessor;
    constructor();
    get falseValue(): ISExpression;
    get trueValue(): ISExpression;
    valueIsFalse(value: ISExpression): boolean;
    valueIsInteger(value: ISExpression): boolean;
    valueAsInteger(value: ISExpression): number;
    integerAsValue(value: number): ISExpression;
    valueIsNumber(value: ISExpression): boolean;
    valueAsNumber(value: ISExpression): number;
    numberAsIntegerValue(value: number): ISExpression;
    numberAsFloatValue(value: number): ISExpression;
    setDebug(debug: boolean): boolean;
}
