// tom-weatherhead/thaw-grammar/src/languages/clu/domain-object-model/interfaces/ivalue.ts

import { IEqualityComparable, IStringifiable } from 'thaw-common-utilities.ts';

export interface ICLUFunctionName {
	readonly functionPart: string;
}

// eslint-disable-next-line @typescript-eslint/no-empty-interface
export interface ICLUValue extends ICLUExpression, IEqualityComparable, IStringifiable {}

export interface ICLUVariable extends ICLUExpression {
	readonly name: string;
}

export interface ICLUEnvironmentFrame {
	add(key: ICLUVariable, value: ICLUValue): void;
	addBubbleDown(key: ICLUVariable, value: ICLUValue): void;
	has(key: ICLUVariable): boolean;
	lookup(variable: ICLUVariable): ICLUValue;
}

export interface ICluster {
	readonly clusterName: string;
	readonly clRep: ICLUVariable[];
	readonly exportedDict: Map<string, ICLUFunctionDefinition>;
	readonly nonExportedDict: Map<string, ICLUFunctionDefinition>;
}

// TODO? : Rename this to ICLUFunctionDefinition(Base) ?
export interface ICLUFunctionDefinition extends ICLUExpression {
	readonly functionName: string;
}

export interface ICLUGlobalInfo {
	readonly trueValue: ICLUValue;
	readonly falseValue: ICLUValue;
	readonly clusterDict: Map<string, ICluster>;
	readonly functionDefinitions: Map<string, ICLUFunctionDefinition>;
	readonly globalEnvironment: ICLUEnvironmentFrame;

	valueIsFalse(value: ICLUValue): boolean;
	valueIsInteger(value: ICLUValue): boolean;
	valueAsInteger(value: ICLUValue): number;
	integerAsValue(value: number): ICLUValue;
	evaluate(expr: ICLUExpression): ICLUValue;
}

export interface ICLUExpression {
	evaluate(
		localEnvironment: ICLUEnvironmentFrame,
		cluster: ICluster | undefined,
		globalInfo: ICLUGlobalInfo
	): ICLUValue;
}
