import { Variable } from './variable';
export declare class EnvironmentFrame<T> {
    readonly dict: Map<string, T>;
    readonly next: EnvironmentFrame<T> | null;
    constructor(next?: EnvironmentFrame<T> | null);
    isDefined(key: Variable<T>): boolean;
    lookup(key: Variable<T>): T;
    add(key: Variable<T>, value: T): void;
    addBubbleDown(key: Variable<T>, value: T): void;
    compose(keys: Array<Variable<T>>, values: T[]): void;
    private dictionaryContainsKey;
}
