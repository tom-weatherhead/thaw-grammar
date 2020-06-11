import { EnvironmentFrame } from './environment-frame';
import { IExpression } from './iexpression';
import { IGlobalInfo } from './iglobal-info';
export declare class WhileUsage<T> implements IExpression<T> {
    readonly condition: IExpression<T>;
    readonly body: IExpression<T>;
    constructor(condition: IExpression<T>, body: IExpression<T>);
    toString(): string;
    evaluate(localEnvironment: EnvironmentFrame<T>, globalInfo: IGlobalInfo<T>): T;
}
