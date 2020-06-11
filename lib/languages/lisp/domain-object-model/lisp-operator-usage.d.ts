import { EnvironmentFrame } from '../../../common/domain-object-model/environment-frame';
import { ExpressionList } from '../../../common/domain-object-model/expression-list';
import { IGlobalInfo } from '../../../common/domain-object-model/iglobal-info';
import { Name } from '../../../common/domain-object-model/name';
import { OperatorUsage } from '../../../common/domain-object-model/operator-usage';
import { ISExpression } from './isexpression';
export declare class LISPOperatorUsage extends OperatorUsage<ISExpression> {
    private readonly operatorsThatTakeEitherIntOrFloatArgs;
    constructor(operatorName: Name, expressionList: ExpressionList<ISExpression>);
    protected tryGetExpectedNumArgs(globalInfo: IGlobalInfo<ISExpression>): number | undefined;
    protected checkArgTypes(evaluatedArguments: ISExpression[]): string | null;
    protected evaluateAux(evaluatedArguments: ISExpression[], localEnvironment: EnvironmentFrame<ISExpression>, globalInfo: IGlobalInfo<ISExpression>): ISExpression;
}
