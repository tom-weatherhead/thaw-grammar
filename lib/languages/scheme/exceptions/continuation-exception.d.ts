import { ExceptionBase } from '../../../common/exceptions/exception-base';
import { ISExpression } from '../../lisp/domain-object-model/isexpression';
export declare class ContinuationException extends ExceptionBase {
    readonly ccGuid: number;
    readonly returnValue: ISExpression;
    constructor(ccGuid: number, returnValue: ISExpression, line?: number, column?: number);
}
