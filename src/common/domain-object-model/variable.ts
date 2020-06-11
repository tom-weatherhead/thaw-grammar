// tom-weatherhead/thaw-grammar/src/common/domain-object-model/variable.ts

'use strict';

import { ArgumentNullException } from '../exceptions/argument-null-exception';
import { EvaluationException } from '../exceptions/evaluation-exception';
import { KeyNotFoundException } from '../exceptions/key-not-found-exception';

import { EnvironmentFrame } from './environment-frame';
import { IExpression } from './iexpression';
import { IGlobalInfo } from './iglobal-info';

export class Variable<T> implements IExpression<T> {
	// , IConvertibleToGraph
	public readonly name: string;
	public readonly line: number;
	public readonly column: number;

	constructor(name: string, line: number, column: number) {
		if (!name) {
			throw new ArgumentNullException(
				'A Variable cannot have a null or empty name',
				'name'
			);
		}

		// Console.WriteLine("Creating a Variable named '{0}'.", name);

		// if (name.StartsWith("Inference")) {
		// 	throw new Exception(string.Format("Error: Creating variable named '{0}'.", name));
		// }

		this.name = name;
		this.line = line;
		this.column = column;
	}

	public toString(): string {
		return this.name;
	}

	public evaluate(
		localEnvironment: EnvironmentFrame<T>,
		globalInfo: IGlobalInfo<T>
	): T {
		try {
			return localEnvironment.lookup(this);
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
