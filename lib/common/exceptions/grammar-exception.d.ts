import { ExceptionBase } from './exception-base';
export declare class GrammarException extends ExceptionBase {
    constructor(message: string, line?: number, column?: number);
}
