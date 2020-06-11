import { ExceptionBase } from './exception-base';
export declare class NotImplementedException extends ExceptionBase {
    constructor(message: string, line?: number, column?: number);
}
