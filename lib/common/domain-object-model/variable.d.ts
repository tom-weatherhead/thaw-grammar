import { EnvironmentFrame } from './environment-frame';
import { IExpression } from './iexpression';
import { IGlobalInfo } from './iglobal-info';
export declare class Variable<T> implements IExpression<T> {
    readonly name: string;
    readonly line: number;
    readonly column: number;
    constructor(name: string, line: number, column: number);
    toString(): string;
    evaluate(localEnvironment: EnvironmentFrame<T>, globalInfo: IGlobalInfo<T>): T;
}
