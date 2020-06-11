import { ExceptionBase } from '../../../common/exceptions/exception-base';
export declare class LISPException extends ExceptionBase {
    constructor(message: string, line: number, column: number);
}
