import { EnvironmentFrame } from '../../../common/domain-object-model/environment-frame';
import { ExpressionList } from '../../../common/domain-object-model/expression-list';
import { IExpression } from '../../../common/domain-object-model/iexpression';
import { IGlobalInfo } from '../../../common/domain-object-model/iglobal-info';
import { Name } from '../../../common/domain-object-model/name';
export declare class OperatorUsage implements IExpression<number> {
    readonly operatorName: Name;
    readonly expressionList: ExpressionList<number>;
    constructor(operatorName: Name, expressionList: ExpressionList<number>);
    evaluate(localEnvironment: EnvironmentFrame<number>, globalInfo: IGlobalInfo<number>): number;
}
