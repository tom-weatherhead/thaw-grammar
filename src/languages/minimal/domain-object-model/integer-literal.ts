import { EnvironmentFrame } from '../../../common/domain-object-model/environment-frame';
import { IExpression } from '../../../common/domain-object-model/iexpression';
import { IGlobalInfo } from '../../../common/domain-object-model/iglobal-info';

import { ArgumentException } from '../../../common/exceptions/argument-exception';

export class IntegerLiteral implements IExpression<number> {
	public readonly value: number;

	constructor(value: any) {
		if (typeof value !== 'number') {
			throw new ArgumentException(
				'IntegerLiteral constructor: value is not a number.',
				'value'
			);
		} else if (Math.floor(value) !== value) {
			throw new ArgumentException(
				'IntegerLiteral constructor: value is not an integer.',
				'value'
			);
		}

		this.value = value as number;
	}

	public toString(): string {
		return `${this.value}`;
	}

	// public override bool Equals(object obj)
	// {

	// 	if (object.ReferenceEquals(this, obj))
	// 	{
	// 		return true;
	// 	}

	// 	IntegerLiteral otherIntLit = obj as IntegerLiteral;

	// 	return otherIntLit != null && Value == otherIntLit.Value;
	// }

	// public override int GetHashCode()
	// {
	// 	return Value.GetHashCode();
	// }

	// public ToInteger(): number {
	// 	return this.Value;
	// }

	public evaluate(
		localEnvironment: EnvironmentFrame<number>,
		globalInfo: IGlobalInfo<number>
	): number {
		return this.value;
	}
}
