import { EnvironmentFrame } from '../../../common/domain-object-model/environment-frame';
import { IExpression } from '../../../common/domain-object-model/iexpression';
import { IGlobalInfo } from '../../../common/domain-object-model/iglobal-info';
import { ISExpression } from './isexpression';
export declare class QuotedConstantWithApostrophe implements IExpression<ISExpression> {
    readonly sexpression: ISExpression;
    constructor(sexpression: ISExpression);
    toString(): string;
    evaluate(localEnvironment: EnvironmentFrame<ISExpression>, globalInfo: IGlobalInfo<ISExpression>): ISExpression;
}
