export declare abstract class ExceptionBase {
    readonly message: string;
    readonly line: number;
    readonly column: number;
    protected constructor(typeName: string, message: string, line?: number, column?: number);
    toString(): string;
}
