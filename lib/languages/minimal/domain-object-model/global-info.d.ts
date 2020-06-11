import { GlobalInfoBase } from '../../../common/domain-object-model/global-info-base';
export declare class MinimalLanguageGlobalInfo extends GlobalInfoBase<number> {
    private readonly trueValueForAccessor;
    private readonly falseValueForAccessor;
    constructor();
    get falseValue(): number;
    get trueValue(): number;
    valueIsFalse(value: number): boolean;
    valueIsInteger(value: number): boolean;
    valueAsInteger(value: number): number;
    integerAsValue(value: number): number;
}
