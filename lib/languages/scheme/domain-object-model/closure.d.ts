import { EnvironmentFrame } from '../../../common/domain-object-model/environment-frame';
import { ExpressionList } from '../../../common/domain-object-model/expression-list';
import { IExpression } from '../../../common/domain-object-model/iexpression';
import { IGlobalInfo } from '../../../common/domain-object-model/iglobal-info';
import { VariableList } from '../../../common/domain-object-model/variable-list';
import { ISExpression } from '../../lisp/domain-object-model/isexpression';
import { SExpressionBase } from '../../lisp/domain-object-model/sexpression-base';
import { ICallableSExpression } from './icallable-sexpression';
export declare class Closure extends SExpressionBase implements ICallableSExpression {
    readonly argList: VariableList<ISExpression>;
    readonly body: IExpression<ISExpression>;
    readonly closureEnvironment: EnvironmentFrame<ISExpression>;
    readonly line: number;
    readonly column: number;
    constructor(argList: VariableList<ISExpression>, body: IExpression<ISExpression>, closureEnvironment: EnvironmentFrame<ISExpression>, line?: number, column?: number);
    call(expressionList: ExpressionList<ISExpression>, localEnvironment: EnvironmentFrame<ISExpression>, globalInfo: IGlobalInfo<ISExpression>): ISExpression;
    isClosure(): boolean;
    toString(): string;
    evaluate(localEnvironment: EnvironmentFrame<ISExpression>, globalInfo: IGlobalInfo<ISExpression>): ISExpression;
}
