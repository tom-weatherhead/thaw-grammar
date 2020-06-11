import { EnvironmentFrame } from '../../../common/domain-object-model/environment-frame';
import { ExpressionList } from '../../../common/domain-object-model/expression-list';
import { IGlobalInfo } from '../../../common/domain-object-model/iglobal-info';
import { Name } from '../../../common/domain-object-model/name';
import { ISExpression } from '../../lisp/domain-object-model/isexpression';
import { SExpressionBase } from '../../lisp/domain-object-model/sexpression-base';
import { ICallableSExpression } from './icallable-sexpression';
export declare class PrimOp extends SExpressionBase implements ICallableSExpression {
    readonly name: Name;
    readonly line: number;
    readonly column: number;
    constructor(name: Name);
    call(expressionList: ExpressionList<ISExpression>, localEnvironment: EnvironmentFrame<ISExpression>, globalInfo: IGlobalInfo<ISExpression>): ISExpression;
    isPrimOp(): boolean;
    toString(): string;
    evaluate(localEnvironment: EnvironmentFrame<ISExpression>, globalInfo: IGlobalInfo<ISExpression>): ISExpression;
}
