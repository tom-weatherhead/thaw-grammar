import { EnvironmentFrame } from '../../../common/domain-object-model/environment-frame';
import { IExpression } from '../../../common/domain-object-model/iexpression';
import { IGlobalInfo } from '../../../common/domain-object-model/iglobal-info';
import { ISExpression } from '../../lisp/domain-object-model/isexpression';
export declare class CallCCUsage implements IExpression<ISExpression> {
    readonly body: IExpression<ISExpression>;
    constructor(body: IExpression<ISExpression>);
    evaluate(localEnvironment: EnvironmentFrame<ISExpression>, globalInfo: IGlobalInfo<ISExpression>): ISExpression;
}
