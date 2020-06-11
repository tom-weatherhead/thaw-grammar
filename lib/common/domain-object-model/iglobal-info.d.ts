import { EnvironmentFrame } from './environment-frame';
import { FunctionDefinition } from './function-definition';
export interface IGlobalInfo<T> {
    globalEnvironment: EnvironmentFrame<T>;
    functionDefinitions: Map<string, FunctionDefinition<T>>;
    dynamicScoping: boolean;
    debug: boolean;
    falseValue: T;
    trueValue: T;
    initialize(): void;
    valueIsFalse(value: T): boolean;
    valueIsInteger(value: T): boolean;
    valueAsInteger(value: T): number;
    integerAsValue(value: number): T;
    loadPreset(presetName: string): string;
    loadPresets(): void;
    clearPrintedText(): void;
    print(evaluatedArguments: T[]): void;
    getPrintedText(): string;
}
