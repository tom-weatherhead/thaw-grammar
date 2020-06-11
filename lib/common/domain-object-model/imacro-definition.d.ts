import { EnvironmentFrame } from './environment-frame';
import { IExpression } from './iexpression';
import { IGlobalInfo } from './iglobal-info';
export interface IMacroDefinition<T> {
    argumentCount: number;
    invokeMacro(unevaluatedArguments: Array<IExpression<T>>, localEnvironment: EnvironmentFrame<T>, globalInfo: IGlobalInfo<T>): T;
}
