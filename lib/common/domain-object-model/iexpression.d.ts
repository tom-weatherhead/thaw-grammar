import { EnvironmentFrame } from './environment-frame';
import { IGlobalInfo } from './iglobal-info';
export interface IExpression<T> {
    evaluate(localEnvironment: EnvironmentFrame<T>, globalInfo: IGlobalInfo<T>): T;
}
