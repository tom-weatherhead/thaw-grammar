import { GlobalInfoBase } from '../../../common/domain-object-model/global-info-base';
import { ISExpression } from './isexpression';
export declare class LISPGlobalInfo extends GlobalInfoBase<ISExpression> {
    private readonly trueValueForAccessor;
    private readonly falseValueForAccessor;
    constructor();
    get falseValue(): ISExpression;
    get trueValue(): ISExpression;
    valueIsFalse(value: ISExpression): boolean;
    valueIsInteger(value: ISExpression): boolean;
    valueAsInteger(value: ISExpression): number;
    integerAsValue(value: number): ISExpression;
    setDebug(debug: boolean): boolean;
}
