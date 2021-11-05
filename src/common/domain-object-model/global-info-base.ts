// tom-weatherhead/thaw-grammar/src/common/domain-object-model/global-info-base.ts

import { ArgumentException } from 'thaw-interpreter-core';

import { EnvironmentFrame } from './environment-frame';
import { FunctionDefinition } from './function-definition';
import { IGlobalInfo } from './iglobal-info';
// import { IGlobalInfoOps } from './iglobal-info-ops';

// export abstract class GlobalInfoBase<T> implements IGlobalInfo<T>, IGlobalInfoOps {
export abstract class GlobalInfoBase<T> implements IGlobalInfo<T> {
	public readonly globalEnvironment = new EnvironmentFrame<T>();
	public readonly functionDefinitions = new Map<string, FunctionDefinition<T>>();
	public dynamicScoping: boolean;
	public debug: boolean;
	private printedText: string;

	protected constructor() {
		this.dynamicScoping = false;
		this.debug = false;
		this.printedText = '';
	}

	public initialize(): void {
		// Restore the state of the interpreter to its newly-created state.
		this.globalEnvironment.dict.clear();
		this.functionDefinitions.clear();

		// if (MacroDefinitions !== null) {
		// 	MacroDefinitions.clear();
		// }

		this.setScoping(false); // Set the scope rules to "static" rather than "dynamic".
		this.setDebug(false); // Turn debug mode off.
		this.clearPrintedText();
	}

	public abstract get falseValue(): T;
	public abstract get trueValue(): T;

	public abstract valueIsInteger(value: T): boolean;
	public abstract valueAsInteger(value: T): number; // Shoud we return Number.NaN if value is not a (safe) integer?
	public abstract integerAsValue(value: number): T;

	// public abstract valueIsFloat(value: T): boolean;
	// public abstract valueAsFloat(value: T): number;
	// public abstract floatAsValue(value: number): T;

	// public abstract valueIsNumber(value: T): boolean;
	// public abstract valueAsNumber(value: T): number;
	// public abstract numberAsIntegerValue(value: number): T; // Convert to the language's native integer data type
	// public abstract numberAsFloatValue(value: number): T; // Convert to the language's native floating-point number data type

	public valueIsFalse(value: T): boolean {
		return value === this.falseValue;
	}

	public loadPreset(presetName: string): string {
		throw new ArgumentException(
			`GlobalInfoBase<T>.loadPreset() : Unknown preset name '${presetName}'.`,
			'presetName'
		);
	}

	public loadPresets(): void {}

	public setScoping(dynamicScoping: boolean): void {
		this.dynamicScoping = dynamicScoping;
	}

	public setDebug(debug: boolean): void {
		this.debug = debug;
	}

	public clearPrintedText(): void {
		this.printedText = '';
	}

	public print(evaluatedArguments: T[]): void {
		this.printedText =
			this.printedText +
			evaluatedArguments.map((evaluatedArgument: T) => `${evaluatedArgument}`).join(', ') +
			'\n';
	}

	protected printDirect(str: string): void {
		// ThAW 2021-06-24 : Temporary
		this.printedText = this.printedText + str + '\n';
	}

	public getPrintedText(): string {
		return this.printedText;
	}
}
