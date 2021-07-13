import { EnvironmentFrame } from '../../../common/domain-object-model/environment-frame';
import { IExpression } from '../../../common/domain-object-model/iexpression';
import { IGlobalInfo } from '../../../common/domain-object-model/iglobal-info';
import { ArgumentException } from '../../../common/exceptions/argument-exception';

export class IntegerLiteral implements IExpression<number> {
	public readonly value: number;
	public readonly line: number;
	public readonly column: number;

	constructor(value: any, line = 0, column = 0) {
		if (typeof value !== 'number') {
			throw new ArgumentException(
				`IntegerLiteral constructor: typeof value is not 'number'; it is '${typeof value}'.`,
				'value'
			);
		} else if (Number.isNaN(value)) {
			throw new ArgumentException(
				'IntegerLiteral constructor: value is not a number (NaN).',
				'value'
			);
		} else if (Math.floor(value) !== value) {
			throw new ArgumentException(
				'IntegerLiteral constructor: value is not an integer.',
				'value'
			);
		}

		this.value = value as number;
		this.line = line;
		this.column = column;
	}

	public toString(): string {
		// Do not allow the output to be formatted as scientific notation.

		return `${this.value}`;
	}

	/* eslint-disable @typescript-eslint/no-unused-vars */
	public evaluate(
		localEnvironment: EnvironmentFrame<number>,
		globalInfo: IGlobalInfo<number>
	): number {
		return this.value;
	}
	/* eslint-enable @typescript-eslint/no-unused-vars */
}
