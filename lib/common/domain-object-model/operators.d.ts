export declare enum OperatorType {
    BinaryNumericOperator = 0,
    BinaryNumericPredicate = 1,
    TypePredicate = 2,
    UnlimitedNumericOperator = 3
}
export declare class Operators {
    static getInstance(): Operators;
    private static instance;
    private readonly mapOperatorNameToDetails;
    private constructor();
    getOperator(name: string): [OperatorType, number, any] | undefined;
}
