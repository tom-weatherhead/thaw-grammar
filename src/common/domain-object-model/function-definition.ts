// tom-weatherhead/thaw-grammar/src/common/domain-object-model/function-definition.ts

import { Name } from 'thaw-interpreter-core';

import { IEnvironmentFrame } from './environment-frame';
import { IExpression } from './iexpression';
import { IGlobalInfo } from './iglobal-info';
import { IVariable } from './variable';

const typenameFunctionDefinition = 'FunctionDefinition';

export function isFunctionDefinition<T>(obj: unknown): obj is FunctionDefinition<T> {
	const str = obj as FunctionDefinition<T>;

	return typeof str !== 'undefined' && str.typename === typenameFunctionDefinition;
}

export interface IFunctionDefinition<T> extends IExpression<T> {
	readonly functionName: Name;
	readonly argList: IVariable<T>[];
	readonly body: IExpression<T>;
}

export class FunctionDefinition<T> implements IFunctionDefinition<T> {
	public readonly typename: string = typenameFunctionDefinition;

	constructor(
		public readonly functionName: Name,
		public readonly argList: IVariable<T>[],
		public readonly body: IExpression<T>
	) {}

	public toString(): string {
		return `(define ${this.functionName} ${this.argList} ${this.body})`;
	}

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
