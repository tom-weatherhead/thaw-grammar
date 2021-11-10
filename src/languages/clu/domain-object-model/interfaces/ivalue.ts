// tom-weatherhead/thaw-grammar/src/languages/clu/domain-object-model/interfaces/ivalue.ts

export interface ICLUFunctionName {
}

export interface ICLUValue extends ICLUExpression {
}

export interface ICLUVariable extends ICLUExpression {
	readonly name: string;
}

export interface ICLUEnvironmentFrame {
	add(key: ICLUVariable,  value: ICLUValue): void;
	addBubbleDown(key: ICLUVariable,  value: ICLUValue): void;
	lookup(varaible: ICLUVariable): ICLUValue;
}

export interface ICluster {
}

export interface ICLUGlobalInfo {
	readonly falseValue: ICLUValue;

	valueIsFalse(value: ICLUValue): boolean;
}

export interface ICLUExpression {
	evaluate(localEnvironment: ICLUEnvironmentFrame, cluster: ICluster, globalInfo: ICLUGlobalInfo): ICLUValue;
}
