import { EnvironmentFrame } from './environment-frame';
import { IExpression } from './iexpression';
import { IGlobalInfo } from './iglobal-info';
import { Variable } from './variable';
export declare class SetUsage<T> implements IExpression<T> {
    readonly variableName: Variable<T>;
    readonly expression: IExpression<T>;
    constructor(variableName: Variable<T>, expression: IExpression<T>);
    toString(): string;
    evaluate(localEnvironment: EnvironmentFrame<T>, globalInfo: IGlobalInfo<T>): T;
}
