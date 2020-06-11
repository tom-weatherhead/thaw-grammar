import { ExceptionBase } from './exception-base';
export declare class EvaluationException extends ExceptionBase {
    constructor(message: string, line?: number, column?: number);
}
