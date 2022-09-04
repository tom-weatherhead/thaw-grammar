// tom-weatherhead/thaw-grammar/src/common/domain-object-model/iglobal-info.ts

import { IEnvironmentFrame } from './environment-frame';
import { IFunctionDefinition } from './function-definition';
import { IMacroDefinition } from './imacro-definition';

// T is the language's value type.

export interface IGlobalInfoForInterpreter {
	initialize(): void; // Restore the state of the global info object to its newly-created state.

	evaluateToString(str: string): string;

	// **** Presets ****
	loadPreset(presetName: string): string;

	// **** Printing text to stdout ****
	clearPrintedText(): void;
	getPrintedText(): string;
}

export interface IGlobalInfo<T> extends IGlobalInfoForInterpreter {
	// **** Public instance fields ****
	globalEnvironment: IEnvironmentFrame<T>;
	functionDefinitions: Map<string, IFunctionDefinition<T>>;
	macroDefinitions: Map<string, IMacroDefinition<T>>;
	dynamicScoping: boolean;
	debug: boolean;

	falseValue: T;
	trueValue: T;

	// **** Public instance methods ****

	booleanAsValue(value: boolean): T;
	// valueAsBoolean(value: T): boolean;

	valueIsFalse(value: T): boolean;

	valueIsInteger(value: T): boolean;
	valueAsInteger(value: T): number; // Shoud we return Number.NaN if value is not a (safe) integer?
	integerAsValue(value: number): T;

	// valueIsFloat(value: T): boolean;
	// valueAsFloat(value: T): number;
	// floatAsValue(value: number): T;

	// valueIsNumber(value: T): boolean;
	// valueAsNumber(value: T): number;
	// numberAsIntegerValue(value: number): T; // Convert to the language's native integer data type
	// numberAsFloatValue(value: number): T; // Convert to the language's native floating-point number data type

	// **** Presets ****
	// loadPreset(presetName: string): string;
	loadPresets(): void;

	// **** Printing text to stdout ****
	// clearPrintedText(): void;
	print(evaluatedArguments: T[]): void;
	// getPrintedText(): string;
}
