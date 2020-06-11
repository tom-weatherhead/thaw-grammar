import { EnvironmentFrame } from './environment-frame';
import { IExpression } from './iexpression';
import { IGlobalInfo } from './iglobal-info';
import { Name } from './name';
import { VariableList } from './variable-list';
export declare class FunctionDefinition<T> implements IExpression<T> {
    readonly functionName: Name;
    readonly argList: VariableList<T>;
    readonly body: IExpression<T>;
    constructor(functionName: Name, argList: VariableList<T>, body: IExpression<T>);
    toString(): string;
    evaluate(localEnvironment: EnvironmentFrame<T>, globalInfo: IGlobalInfo<T>): T;
}
