// tom-weatherhead/thaw-grammar/src/languages/smalltalk/domain-object-model/variable.ts

import { ArgumentNullException } from '../../../common/exceptions/argument-null-exception';
import { EvaluationException } from '../../../common/exceptions/evaluation-exception';
// import { KeyNotFoundException } from '../exceptions/key-not-found-exception';

import { EnvironmentFrame } from '../../../common/domain-object-model/environment-frame';
// import { IExpression } from '../../../common/domain-object-model/iexpression';
import { IGlobalInfo } from '../../../common/domain-object-model/iglobal-info';

import { ISmalltalkExpression } from './interfaces/iexpression';
import { ISmalltalkValue } from './interfaces/ivalue';

export class SmalltalkVariable implements ISmalltalkExpression {
	public readonly name: string;
	public readonly line: number;
	public readonly column: number;

	constructor(name: string, line: number, column: number) {
		if (!name) {
			throw new ArgumentNullException('A Variable cannot have a null or empty name', 'name');
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
		localEnvironment: EnvironmentFrame<ISmalltalkValue>,
		// eslint-disable-next-line @typescript-eslint/no-unused-vars
		globalInfo: IGlobalInfo<ISmalltalkValue>
	): ISmalltalkValue {
		try {
			return localEnvironment.lookup(this);
		} catch (KeyNotFoundException) {
			throw new EvaluationException(
				`Variable.Evaluate() : No value found for variable ${this.name}`,
				this.line,
				this.column
			);
		}
	}
}
