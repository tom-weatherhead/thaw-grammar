// tom-weatherhead/thaw-grammar/src/common/domain-object-model/function-definition.ts

import { Name } from 'thaw-interpreter-core';

import { IEnvironmentFrame } from './environment-frame';
import { IExpression } from './iexpression';
import { IGlobalInfo } from './iglobal-info';
import { VariableList } from './variable-list';

export class FunctionDefinition<T> implements IExpression<T> {
	public readonly functionName: Name;
	public readonly argList: VariableList<T>;
	public readonly body: IExpression<T>;

	constructor(functionName: Name, argList: VariableList<T>, body: IExpression<T>) {
		this.functionName = functionName;
		this.argList = argList;
		this.body = body;
	}

	public toString(): string {
		return `(define ${this.functionName} ${this.argList} ${this.body})`;
	}

	// public evaluate(localEnvironment: EnvironmentFrame<T>, globalInfo: IGlobalInfo<T>): T {
	public evaluate(
		globalInfo: IGlobalInfo<T>,
		// eslint-disable-next-line @typescript-eslint/no-unused-vars
		localEnvironment?: IEnvironmentFrame<T>,
		// eslint-disable-next-line @typescript-eslint/no-unused-vars
		options?: unknown
	): T {
		globalInfo.functionDefinitions.set(this.functionName.value, this);

		return globalInfo.trueValue;
	}
}
