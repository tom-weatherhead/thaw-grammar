// tom-weatherhead/thaw-grammar/src/languages/smalltalk/domain-object-model/interfaces/iexpression.ts

import { IEqualityComparable } from 'thaw-common-utilities.ts';

import { IParser, ITokenizer } from 'thaw-interpreter-types';

// import { Name } from 'thaw-interpreter-core';

import { IEnvironmentFrame } from '../../../../common/domain-object-model/environment-frame';

import { IFunctionDefinition } from '../../../../common/domain-object-model/function-definition';

import { IExpression } from '../../../../common/domain-object-model/iexpression';

import { IGlobalInfo } from '../../../../common/domain-object-model/iglobal-info';

import { IVariable } from '../../../../common/domain-object-model/variable';

export interface ISmalltalkEvaluateOptions {
	c?: ISmalltalkClass;
	receiver: ISmalltalkValue;
}

export interface ISmalltalkValue
	extends IEqualityComparable,
		ISmalltalkExpression /*, IStringifiable */ {
	readonly typename: string;
	isInteger: boolean;
	readonly owner: ISmalltalkClass | undefined;

	getTypename(): string;

	isArray(): boolean;
	isCharacter(): boolean;
	isNumber(): boolean;
	isObject(): boolean;
	isString(): boolean;
	isSymbol(): boolean;

	toArray(): ISmalltalkValue[] | undefined;
	toInteger(): number | undefined;
	toFloat(): number | undefined;
	toStringX(): string | undefined;
	toUserValue(): ISmalltalkUserValue | undefined;
}

export interface ISmalltalkArray extends ISmalltalkValue {
	getElement(i: number): ISmalltalkValue;
	setElement(i: number, elementValue: ISmalltalkValue): ISmalltalkValue;
}

export interface ISmalltalkString extends ISmalltalkValue {
	index(idx: ISmalltalkValue): ISmalltalkValue;
}

// export interface ISmalltalkFunctionDefinition {
// 	readonly functionName: string;
// 	readonly argList: ISmalltalkVariable[];
// 	readonly body: ISmalltalkExpression;
// }
export type ISmalltalkFunctionDefinition = IFunctionDefinition<ISmalltalkValue>;

export interface ISmalltalkGlobalInfo extends IGlobalInfo<ISmalltalkValue> {
	// **** Public instance fields ****
	// readonly globalEnvironment: ISmalltalkEnvironmentFrame;
	// readonly functionDefinitions: Map<string, ISmalltalkFunctionDefinition>;
	readonly classDict: Map<string, ISmalltalkClass>;
	readonly objectInstance: ISmalltalkUserValue;

	// falseValue: ISmalltalkValue;
	// trueValue: ISmalltalkValue;

	// **** Public instance methods ****
	// initialize(): void; // Restore the state of the global info object to its newly-created state.

	// valueIsFalse(value: ISmalltalkValue): boolean;
	// valueIsTrue(value: ISmalltalkValue): boolean;
	//
	// valueIsInteger(value: ISmalltalkValue): boolean;
	// valueAsInteger(value: ISmalltalkValue): number; // Shoud we return Number.NaN if value is not a (safe) integer?
	// integerAsValue(value: number): ISmalltalkValue;

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

	// evaluate(expr: ISmalltalkExpression): ISmalltalkValue;
}

export interface ISmalltalkVariable extends IEqualityComparable, IVariable<ISmalltalkValue> {
	readonly typename: string;
	// readonly name: string;
}
// export type ISmalltalkVariable = IVariable<ISmalltalkValue>;

// export interface ISmalltalkEnvironmentFrame {
// 	readonly dict: Map<string, ISmalltalkValue>;
//
// 	isDefined(key: ISmalltalkVariable): boolean;
// 	lookup(key: ISmalltalkVariable): ISmalltalkValue;
// 	add(key: ISmalltalkVariable, value: ISmalltalkValue): void;
// 	addBubbleDown(key: ISmalltalkVariable, value: ISmalltalkValue): void;
// }
export type ISmalltalkEnvironmentFrame = IEnvironmentFrame<ISmalltalkValue>;

export interface ISmalltalkClass extends ISmalltalkExpression {
	readonly className: string; // Or Name;
	readonly superClassName: string | undefined; // Or Name | undefined;
	superClass: ISmalltalkClass | undefined;
	readonly clRep: ISmalltalkVariable[];

	addFunction(tokenizer: ITokenizer, parser: IParser, functionAsString: string): void;
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

// export interface ISmalltalkExpression {
// 	evaluate(
// 		localEnvironment: ISmalltalkEnvironmentFrame | undefined,
// 		receiver: ISmalltalkValue, // | undefined,
// 		// "SmalltalkClass c" was added here as the third parameter in order to support "super"; see Exercise 11 on pages 347-348.
// 		c: ISmalltalkClass | undefined,
// 		globalInfo: ISmalltalkGlobalInfo
// 	): ISmalltalkValue;
// }
export type ISmalltalkExpression = IExpression<ISmalltalkValue>;
