import { IExpression } from '../../../common/domain-object-model/iexpression';
import { INumber } from './inumber';
import { ISExpression } from './isexpression';
import { SExpressionBase } from './sexpression-base';
export declare class IntegerLiteral extends SExpressionBase implements INumber {
    readonly value: number;
    constructor(value: any);
    toString(): string;
    toInteger(): number;
    toDouble(): number;
    isNumber(): boolean;
    convertToGraph(): IExpression<ISExpression>;
}
