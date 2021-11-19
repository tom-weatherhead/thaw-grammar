// tom-weatherhead/thaw-grammar/src/languages/clu/domain-object-model/interfaces/ivalue.ts

import { IEqualityComparable, IStringifiable } from 'thaw-common-utilities.ts';

import { Name } from 'thaw-interpreter-core';

import { IEnvironmentFrame } from '../../../../common/domain-object-model/environment-frame';

// import { IFunctionDefinition } from '../../../../common/domain-object-model/function-definition';

import { IExpression } from '../../../../common/domain-object-model/iexpression';

import { IGlobalInfo } from '../../../../common/domain-object-model/iglobal-info';

import { IVariable } from '../../../../common/domain-object-model/variable';

export interface ICLUFunctionName {
	readonly functionPart: string;
}

// eslint-disable-next-line @typescript-eslint/no-empty-interface
export interface ICLUValue extends ICLUExpression, IEqualityComparable, IStringifiable {}

// export interface ICLUVariable extends ICLUExpression {
// 	readonly name: string;
// }
export type ICLUVariable = IVariable<ICLUValue>;

// export interface ICLUEnvironmentFrame {
// 	add(key: ICLUVariable, value: ICLUValue): void;
// 	addBubbleDown(key: ICLUVariable, value: ICLUValue): void;
// 	has(key: ICLUVariable): boolean;
// 	lookup(variable: ICLUVariable): ICLUValue;
// }
export type ICLUEnvironmentFrame = IEnvironmentFrame<ICLUValue>;

export interface ICluster extends ICLUExpression {
	readonly clusterName: Name;
	readonly clRep: ICLUVariable[];
	readonly exportedDict: Map<string, ICLUFunctionDefinition>;
	readonly nonExportedDict: Map<string, ICLUFunctionDefinition>;
}

export interface ICluEvaluateOptions {
	readonly cluster?: ICluster;
}

// TODO? : Rename this to ICLUFunctionDefinition(Base) ?
export interface ICLUFunctionDefinition extends ICLUExpression {
	readonly functionName: Name;
}
// export type ICLUFunctionDefinition = IFunctionDefinition<ICLUValue>;

export interface ICLUGlobalInfo extends IGlobalInfo<ICLUValue> {
	// readonly trueValue: ICLUValue;
	// readonly falseValue: ICLUValue;
	readonly clusterDict: Map<string, ICluster>;
	// readonly functionDefinitions: Map<string, ICLUFunctionDefinition>;
	// readonly globalEnvironment: ICLUEnvironmentFrame;
	//
	// valueIsFalse(value: ICLUValue): boolean;
	// valueIsInteger(value: ICLUValue): boolean;
	// valueAsInteger(value: ICLUValue): number;
	// integerAsValue(value: number): ICLUValue;
	// evaluate(expr: ICLUExpression): ICLUValue;
}
// export type ICLUGlobalInfo = IGlobalInfo<ICLUValue>;

// export interface ICLUExpression {
// 	evaluate(
// 		localEnvironment: ICLUEnvironmentFrame,
// 		cluster: ICluster | undefined,
// 		globalInfo: ICLUGlobalInfo
// 	): ICLUValue;
// }
export type ICLUExpression = IExpression<ICLUValue>;
