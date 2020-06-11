import { EnvironmentFrame } from './environment-frame';
import { ExpressionList } from './expression-list';
import { IExpression } from './iexpression';
import { IGlobalInfo } from './iglobal-info';
import { Name } from './name';
export declare class OperatorUsage<T> implements IExpression<T> {
    readonly operatorName: Name;
    readonly expressionList: ExpressionList<T>;
    private readonly twoArgumentIntegerPredicates;
    private readonly twoArgumentIntegerOperators;
    constructor(operatorName: Name, expressionList: ExpressionList<T>);
    toString(): string;
    evaluate(localEnvironment: EnvironmentFrame<T>, globalInfo: IGlobalInfo<T>): T;
    protected tryGetExpectedNumArgs(globalInfo: IGlobalInfo<T>): number | undefined;
    protected checkArgTypes(evaluatedArguments: T[]): string | null;
    protected evaluateAux(evaluatedArguments: T[], localEnvironment: EnvironmentFrame<T>, globalInfo: IGlobalInfo<T>): T;
}
