import { ExceptionBase } from './exception-base';
export declare class ArgumentException extends ExceptionBase {
    readonly argumentName: string;
    constructor(message: string, argumentName: string, line?: number, column?: number);
}
