import { EnvironmentFrame } from '../../../common/domain-object-model/environment-frame';
import { IExpression } from '../../../common/domain-object-model/iexpression';
import { IGlobalInfo } from '../../../common/domain-object-model/iglobal-info';
import { VariableList } from '../../../common/domain-object-model/variable-list';
import { ISExpression } from '../../lisp/domain-object-model/isexpression';
export declare class LambdaExpression implements IExpression<ISExpression> {
    readonly argList: VariableList<ISExpression>;
    readonly body: IExpression<ISExpression>;
    readonly line: number;
    readonly column: number;
    constructor(argList: VariableList<ISExpression>, body: IExpression<ISExpression>, line?: number, column?: number);
    toString(): string;
    evaluate(localEnvironment: EnvironmentFrame<ISExpression>, globalInfo: IGlobalInfo<ISExpression>): ISExpression;
}
