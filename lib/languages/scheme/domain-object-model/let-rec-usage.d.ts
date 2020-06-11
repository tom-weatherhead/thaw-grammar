import { EnvironmentFrame } from '../../../common/domain-object-model/environment-frame';
import { IExpression } from '../../../common/domain-object-model/iexpression';
import { IGlobalInfo } from '../../../common/domain-object-model/iglobal-info';
import { Variable } from '../../../common/domain-object-model/variable';
export declare class LetRecUsage<T> implements IExpression<T> {
    readonly bindings: Array<[Variable<T>, IExpression<T>]>;
    readonly expression: IExpression<T>;
    constructor(bindings: Array<[Variable<T>, IExpression<T>]>, expression: IExpression<T>);
    toString(): string;
    evaluate(localEnvironment: EnvironmentFrame<T>, globalInfo: IGlobalInfo<T>): T;
}
