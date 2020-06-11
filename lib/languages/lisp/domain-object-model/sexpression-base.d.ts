import { EnvironmentFrame } from '../../../common/domain-object-model/environment-frame';
import { IExpression } from '../../../common/domain-object-model/iexpression';
import { IGlobalInfo } from '../../../common/domain-object-model/iglobal-info';
import { ISExpression } from './isexpression';
import { SExpressionBareBase } from './sexpression-bare-base';
export declare abstract class SExpressionBase extends SExpressionBareBase implements IExpression<ISExpression> {
    evaluate(localEnvironment: EnvironmentFrame<ISExpression>, globalInfo: IGlobalInfo<ISExpression>): ISExpression;
}
