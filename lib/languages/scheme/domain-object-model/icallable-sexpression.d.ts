import { EnvironmentFrame } from '../../../common/domain-object-model/environment-frame';
import { ExpressionList } from '../../../common/domain-object-model/expression-list';
import { IGlobalInfo } from '../../../common/domain-object-model/iglobal-info';
import { ISExpression } from '../../lisp/domain-object-model/isexpression';
export interface ICallableSExpression extends ISExpression {
    line: number;
    column: number;
    call(expressionList: ExpressionList<ISExpression>, localEnvironment: EnvironmentFrame<ISExpression>, globalInfo: IGlobalInfo<ISExpression>): ISExpression;
}
