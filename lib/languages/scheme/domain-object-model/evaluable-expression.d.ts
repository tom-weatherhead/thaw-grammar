import { EnvironmentFrame } from '../../../common/domain-object-model/environment-frame';
import { ExpressionList } from '../../../common/domain-object-model/expression-list';
import { IExpression } from '../../../common/domain-object-model/iexpression';
import { IGlobalInfo } from '../../../common/domain-object-model/iglobal-info';
import { ISExpression } from '../../lisp/domain-object-model/isexpression';
export declare class EvaluableExpression implements IExpression<ISExpression> {
    readonly firstExpression: IExpression<ISExpression>;
    readonly expressionList: ExpressionList<ISExpression>;
    constructor(firstExpression: IExpression<ISExpression>, expressionList: ExpressionList<ISExpression>);
    evaluate(localEnvironment: EnvironmentFrame<ISExpression>, globalInfo: IGlobalInfo<ISExpression>): ISExpression;
}
