import { EnvironmentFrame } from '../../../common/domain-object-model/environment-frame';
import { IExpression } from '../../../common/domain-object-model/iexpression';
import { IGlobalInfo } from '../../../common/domain-object-model/iglobal-info';
export declare class IntegerLiteral implements IExpression<number> {
    readonly value: number;
    readonly line: number;
    readonly column: number;
    constructor(value: any, line?: number, column?: number);
    toString(): string;
    evaluate(localEnvironment: EnvironmentFrame<number>, globalInfo: IGlobalInfo<number>): number;
}
