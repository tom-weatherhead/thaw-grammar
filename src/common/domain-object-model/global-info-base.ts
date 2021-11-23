// tom-weatherhead/thaw-grammar/src/common/domain-object-model/global-info-base.ts

import { IParser, ITokenizer } from 'thaw-interpreter-types';

import { ArgumentException } from 'thaw-interpreter-core';

import { EnvironmentFrame, IEnvironmentFrame } from './environment-frame';
import { IExpression } from './iexpression';
import { IFunctionDefinition } from './function-definition';
import { IGlobalInfo } from './iglobal-info';
// import { IGlobalInfoOps } from './iglobal-info-ops';
import { IMacroDefinition } from './imacro-definition';

// export abstract class GlobalInfoBase<T> implements IGlobalInfo<T>, IGlobalInfoOps {
export abstract class GlobalInfoBase<T> implements IGlobalInfo<T> {
	protected readonly tokenizer: ITokenizer | undefined;
	protected readonly parser: IParser | undefined;
	public readonly globalEnvironment: IEnvironmentFrame<T> = new EnvironmentFrame<T>();
	public readonly functionDefinitions = new Map<string, IFunctionDefinition<T>>();
	public readonly macroDefinitions = new Map<string, IMacroDefinition<T>>();
	public dynamicScoping = false;
	public debug = false;
	private printedText = '';

	// 2021-11-20 : TODO: Uncomment these lines:

	// protected readonly abstract tv: T;
	// protected readonly abstract fv: T;

	// ... and then make these functions non-abstract:

	// public abstract get falseValue(): T;
	// public abstract get trueValue(): T;

	// ... and then set the values of tv and fv in each languages GlobalInfo constructor.

	protected constructor(
		options: {
			parser?: IParser;
			tokenizer?: ITokenizer;
		} = {}
	) {
		this.tokenizer = options.tokenizer;
		this.parser = options.parser;

		// if (typeof this.tokenizer !== 'undefined' && typeof this.parser !== 'undefined') {
		// 	this.loadPresets();
		// }
	}

	public initialize(): void {
		// Restore the state of the interpreter to its newly-created state.
		this.globalEnvironment.dict.clear();
		this.functionDefinitions.clear();

		// if (typeof this.macroDefinitions !== 'undefined') {
		this.macroDefinitions.clear();
		// }

		this.setScoping(false); // Set the scope rules to "static" rather than "dynamic".
		this.setDebug(false); // Turn debug mode off.
		this.clearPrintedText();

		if (typeof this.tokenizer !== 'undefined' && typeof this.parser !== 'undefined') {
			this.loadPresets();
		}
	}

	public abstract get falseValue(): T;
	public abstract get trueValue(): T;

	// public valueIsFalse(value: T): boolean {
	// 	return value === this.falseValue;
	// }
	public abstract valueIsFalse(value: T): boolean;

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

	public evaluate(str: string): T {
		if (typeof this.tokenizer === 'undefined') {
			throw new Error('GlobalInfoBase.evaluate() : this.tokenizer is undefined.');
		} else if (typeof this.parser === 'undefined') {
			throw new Error('GlobalInfoBase.evaluate() : this.parser is undefined.');
		}

		const parseResult = this.parser.parse(this.tokenizer.tokenize(str));
		const expr = parseResult as IExpression<T>;

		return expr.evaluate(this, this.globalEnvironment);
	}

	public evaluateToString(str: string): string {
		// return this.evaluate(str).toString();

		return `${this.evaluate(str)}`;
	}
}
