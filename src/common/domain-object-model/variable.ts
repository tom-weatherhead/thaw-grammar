// tom-weatherhead/thaw-grammar/src/common/domain-object-model/variable.ts

import { ifDefinedThenElse, IStringifiable } from 'thaw-common-utilities.ts';

import { ArgumentException, EvaluationException } from 'thaw-interpreter-core';

// import { ArgumentNullException } from '../exceptions/argument-null-exception';
// import { KeyNotFoundException } from '../exceptions/key-not-found-exception';

import { IEnvironmentFrame } from './environment-frame';
import { IExpression } from './iexpression';
import { IGlobalInfo } from './iglobal-info';

const typenameVariableT = 'Variable<T>';

export function isVariableT<T>(obj: unknown): obj is Variable<T> {
	const otherVariable = obj as Variable<T>;

	return typeof otherVariable !== 'undefined' && otherVariable.typename === typenameVariableT;
}

export interface IVariable<T> extends IExpression<T>, IStringifiable {
	readonly name: string;
	readonly line: number;
	readonly column: number;
}

export class Variable<T> implements IVariable<T> {
	// , IConvertibleToGraph
	public readonly typename: string = typenameVariableT;
	// public readonly name: string;
	// public readonly line: number;
	// public readonly column: number;

	constructor(
		public readonly name: string,
		public readonly line = 0,
		public readonly column = 0
	) {
		if (!this.name) {
			throw new ArgumentException('A Variable cannot have an empty name', 'name');
		}

		// Console.WriteLine("Creating a Variable named '{0}'.", name);

		// if (name.StartsWith("Inference")) {
		// 	throw new Exception(string.Format("Error: Creating variable named '{0}'.", name));
		// }

		// this.name = name;
		// this.line = line;
		// this.column = column;
	}

	public toString(): string {
		return this.name;
	}

	public evaluate(
		globalInfo: IGlobalInfo<T>,
		localEnvironment?: IEnvironmentFrame<T>,
		// eslint-disable-next-line @typescript-eslint/no-unused-vars
		options?: unknown
	): T {
		try {
			return ifDefinedThenElse(localEnvironment, globalInfo.globalEnvironment).lookup(this);
		} catch (KeyNotFoundException) {
			throw new EvaluationException(
				`Variable<T>.Evaluate() : No value found for variable ${this.name}`,
				this.line,
				this.column
			);
		}
	}

	// public ConvertToGraph(): IExpression<ISExpression> {
	// 	const iex = this as IExpression<ISExpression>;

	// 	if (iex !== undefined)
	// 	{
	// 		return iex;
	// 	}

	// 	throw new NotImplementedException('Variable<T>.ConvertToGraph() : T is not ISExpression.');
	// }
}
