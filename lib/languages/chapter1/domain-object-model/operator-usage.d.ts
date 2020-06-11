import { EnvironmentFrame } from '../../../common/domain-object-model/environment-frame';
import { IGlobalInfo } from '../../../common/domain-object-model/iglobal-info';
import { OperatorUsage } from '../../../common/domain-object-model/operator-usage';
export declare class Chapter1OperatorUsage extends OperatorUsage<number> {
    protected tryGetExpectedNumArgs(globalInfo: IGlobalInfo<number>): number | undefined;
    protected evaluateAux(evaluatedArguments: number[], localEnvironment: EnvironmentFrame<number>, globalInfo: IGlobalInfo<number>): number;
}
