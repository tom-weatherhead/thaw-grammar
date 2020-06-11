export declare class Production {
    lhs: number;
    rhs: Array<number | string>;
    private readonly num;
    constructor(l: number, r: Array<number | string>, n?: number);
    toString(): string;
    RHSWithNoSemanticActions(): number[];
    StripOutSemanticActions(): Production;
    ContainsSymbol(symbol: number): boolean;
}
