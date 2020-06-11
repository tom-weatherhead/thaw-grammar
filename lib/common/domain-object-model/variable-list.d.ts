import { Variable } from './variable';
export declare class VariableList<T> {
    readonly value: Array<Variable<T>>;
    toString(): string;
}
