import { EnvironmentFrame } from '../../../common/domain-object-model/environment-frame';
import { ExpressionList } from '../../../common/domain-object-model/expression-list';
import { IGlobalInfo } from '../../../common/domain-object-model/iglobal-info';
import { ISExpression } from '../../lisp/domain-object-model/isexpression';
import { SExpressionBase } from '../../lisp/domain-object-model/sexpression-base';
import { ICallableSExpression } from './icallable-sexpression';
export declare class Continuation extends SExpressionBase implements ICallableSExpression {
    readonly ccGuid: number;
    readonly line: number;
    readonly column: number;
    constructor(ccGuid: number, line?: number, column?: number);
    toString(): string;
    isClosure(): boolean;
    call(args: ExpressionList<ISExpression>, localEnvironment: EnvironmentFrame<ISExpression>, globalInfo: IGlobalInfo<ISExpression>): ISExpression;
}
