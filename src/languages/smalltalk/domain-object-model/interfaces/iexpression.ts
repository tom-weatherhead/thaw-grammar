// tom-weatherhead/thaw-grammar/src/languages/smalltalk/domain-object-model/interfaces/iexpression.ts

// public interface ISmalltalkExpression
// {
// 	//"SmalltalkClass c" was added here as the third parameter in order to support "super"; see Exercise 11 on pages 347-348.
// 	ISmalltalkValue Evaluate(SmalltalkEnvironmentFrame localEnvironment, ISmalltalkValue receiver, SmalltalkClass c, SmalltalkGlobalInfo globalInfo);
// }

// import { IExpression } from '../../../../common/domain-object-model/iexpression';

// import { IGlobalInfo } from '../../../../common/domain-object-model/iglobal-info';

// import { ISmalltalkValue } from './ivalue';

export interface ISmalltalkValue extends ISmalltalkExpression {
	isInteger: boolean;
	owner: ISmalltalkClass | undefined;

	getTypename(): string;
	isNumber(): boolean;
	isSymbol(): boolean;
	isCharacter(): boolean;
	isString(): boolean;
	isObject(): boolean;
	isArray(): boolean;

	toInteger(): number | undefined;
	toFloat(): number | undefined;
	toStringX(): string | undefined;
	toUserValue(): ISmalltalkUserValue | undefined;
}

export interface ISmalltalkFunctionDefinition {
	readonly functionName: string;
	readonly argList: ISmalltalkVariable[];
	readonly body: ISmalltalkExpression;
}

export interface ISmalltalkGlobalInfo {
	// **** Public instance fields ****
	readonly globalEnvironment: ISmalltalkEnvironmentFrame;
	readonly functionDefinitions: Map<string, ISmalltalkFunctionDefinition>;
	readonly classDict: Map<string, ISmalltalkClass>;
	readonly objectInstance: ISmalltalkUserValue;
	// dynamicScoping: boolean;
	// debug: boolean;

	zeroValue: ISmalltalkValue;
	falseValue: ISmalltalkValue;
	trueValue: ISmalltalkValue;

	// **** Public instance methods ****
	// initialize(): void; // Restore the state of the global info object to its newly-created state.

	valueIsFalse(value: ISmalltalkValue): boolean;

	valueIsInteger(value: ISmalltalkValue): boolean;
	valueAsInteger(value: ISmalltalkValue): number; // Shoud we return Number.NaN if value is not a (safe) integer?
	integerAsValue(value: number): ISmalltalkValue;

	// valueIsFloat(value: T): boolean;
	// valueAsFloat(value: T): number;
	// floatAsValue(value: number): T;

	// valueIsNumber(value: T): boolean;
	// valueAsNumber(value: T): number;
	// numberAsIntegerValue(value: number): T; // Convert to the language's native integer data type
	// numberAsFloatValue(value: number): T; // Convert to the language's native floating-point number data type

	// **** Presets ****
	// loadPreset(presetName: string): string;
	// loadPresets(): void;

	// **** Printing text to stdout ****
	// clearPrintedText(): void;
	// print(evaluatedArguments: ISmalltalkValue[]): void;
	// getPrintedText(): string;
}

export interface ISmalltalkVariable {
	readonly typename: string;
	readonly name: string;
}

export interface ISmalltalkEnvironmentFrame {
	readonly dict: Map<string, ISmalltalkValue>;

	isDefined(key: ISmalltalkVariable): boolean;
	lookup(key: ISmalltalkVariable): ISmalltalkValue;
	add(key: ISmalltalkVariable, value: ISmalltalkValue): void;
	addBubbleDown(key: ISmalltalkVariable, value: ISmalltalkValue): void;
}

export interface ISmalltalkClass extends ISmalltalkExpression {
	readonly className: string;
	readonly superClassName: string | undefined;
	superClass: ISmalltalkClass | undefined;
	readonly clRep: ISmalltalkVariable[];

	findMethod(methodName: string): {
		method: ISmalltalkFunctionDefinition | undefined;
		classInWhichMethodWasFound: ISmalltalkClass | undefined;
	};
	findClassVariableValue(variable: ISmalltalkVariable): ISmalltalkValue | undefined;
	trySetClassVariableValue(variable: ISmalltalkVariable, value: ISmalltalkValue): boolean;
}

export interface ISmalltalkStringValue extends ISmalltalkValue {
	readonly value: string;
}

export interface ISmalltalkUserValue extends ISmalltalkValue {
	readonly value: ISmalltalkEnvironmentFrame;
}

// export type ISmalltalkExpression = IExpression<ISmalltalkValue>;

export interface ISmalltalkExpression {
	evaluate(
		localEnvironment: ISmalltalkEnvironmentFrame | undefined,
		receiver: ISmalltalkValue,
		c: ISmalltalkClass | undefined,
		globalInfo: ISmalltalkGlobalInfo
	): ISmalltalkValue;
}
