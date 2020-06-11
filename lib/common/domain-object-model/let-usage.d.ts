import { EnvironmentFrame } from './environment-frame';
import { IExpression } from './iexpression';
import { IGlobalInfo } from './iglobal-info';
import { Variable } from './variable';
export declare class LetUsage<T> implements IExpression<T> {
    readonly bindings: Array<[Variable<T>, IExpression<T>]>;
    readonly expression: IExpression<T>;
    constructor(bindings: Array<[Variable<T>, IExpression<T>]>, expression: IExpression<T>);
    toString(): string;
    evaluate(localEnvironment: EnvironmentFrame<T>, globalInfo: IGlobalInfo<T>): T;
}
