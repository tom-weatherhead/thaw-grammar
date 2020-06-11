import { EnvironmentFrame } from './environment-frame';
import { FunctionDefinition } from './function-definition';
import { IGlobalInfo } from './iglobal-info';
export declare abstract class GlobalInfoBase<T> implements IGlobalInfo<T> {
    readonly globalEnvironment: EnvironmentFrame<T>;
    readonly functionDefinitions: Map<string, FunctionDefinition<T>>;
    dynamicScoping: boolean;
    debug: boolean;
    private printedText;
    protected constructor();
    initialize(): void;
    abstract get falseValue(): T;
    abstract get trueValue(): T;
    abstract valueIsInteger(value: T): boolean;
    abstract valueAsInteger(value: T): number;
    abstract integerAsValue(value: number): T;
    valueIsFalse(value: T): boolean;
    loadPreset(presetName: string): string;
    loadPresets(): void;
    setScoping(dynamicScoping: boolean): void;
    setDebug(debug: boolean): void;
    clearPrintedText(): void;
    print(evaluatedArguments: T[]): void;
    getPrintedText(): string;
}
