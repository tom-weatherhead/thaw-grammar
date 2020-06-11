import { ExceptionBase } from './exception-base';
export declare class KeyNotFoundException extends ExceptionBase {
    constructor(message: string, line?: number, column?: number);
}
