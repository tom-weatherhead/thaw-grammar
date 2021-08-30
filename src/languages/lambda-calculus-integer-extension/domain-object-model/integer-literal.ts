// tom-weatherhead/thaw-grammar/src/languages/lambda-calculus-integer-extension/domain-object-model/integer-literal.ts

import { createSet, IImmutableSet } from 'thaw-common-utilities.ts';

import { ArgumentException } from '../../../common/exceptions/argument-exception';

import {
	ILCExpression,
	ISubstitution,
	IUnifiable
} from '../../lambda-calculus/domain-object-model/interfaces/expression';

// export interface ILCIntegerExpression extends ILCExpression {
// 	deltaReduce(): ILCExpression; // number | LCLambdaExpression;
// }

const typenameIntegerLiteral = 'LCIntegerLiteral';

export function isLCIntegerLiteral(obj: unknown): obj is LCIntegerLiteral {
	const otherIntegerLiteral = obj as LCIntegerLiteral;

	return (
		typeof otherIntegerLiteral !== 'undefined' &&
		otherIntegerLiteral.typename === typenameIntegerLiteral
	);
}

export class LCIntegerLiteral implements ILCExpression {
	public readonly typename = typenameIntegerLiteral;
	public readonly value: number;

	// constructor(public readonly value: number) {}

	constructor(value: unknown) {
		// super();

		// HACK ThAW 2020-01-06 : The value in an IntegerLiteral can be int or float.
		// In the future, we will need to properly support FloatLiterals
		// and distinguish them from IntegerLiterals.

		if (typeof value !== 'number') {
			throw new ArgumentException(
				`IntegerLiteral constructor: typeof value '${value}' is not 'number'; it is '${typeof value}'.`,
				'value'
			);
		} else if (Number.isNaN(value)) {
			throw new ArgumentException(
				'IntegerLiteral constructor: value is not a number (NaN).',
				'value'
			);
			// } else if (Math.floor(value) !== value) {
			// throw new ArgumentException(`IntegerLiteral constructor: value '${value}' is not an integer.`, 'value');
		}

		this.value = value as number;
	}

	public toString(): string {
		// Do not allow the output to be formatted as scientific notation.

		return `${this.value}`;
	}

	public applySubstitution(substitution: ISubstitution<ILCExpression>): ILCExpression {
		return this;
	}

	public unify(other: IUnifiable<ILCExpression>): ISubstitution<ILCExpression> | undefined {
		return undefined; // TODO FIXME
	}

	public isIsomorphicTo(other: IUnifiable<ILCExpression>): boolean {
		return false; // TODO FIXME
	}

	public containsVariableNamed(name: string): boolean {
		return false;
	}

	public containsBoundVariableNamed(name: string): boolean {
		return false;
	}

	public containsUnboundVariableNamed(
		name: string,
		boundVariableNames: IImmutableSet<string>
	): boolean {
		return false;
	}

	public renameBoundVariable(newName: string, oldName: string): ILCExpression {
		// Alpha-conversion

		return this;
	}

	public substituteForUnboundVariable(name: string, value: ILCExpression): ILCExpression {
		return this;
	}

	public betaReduce(generateNewVariableName: () => string): ILCExpression {
		return this;
	}

	public deltaReduce(): ILCExpression {
		return this;
	}

	public etaReduce(): ILCExpression {
		return this;
	}

	public getSetOfAllVariableNames(): IImmutableSet<string> {
		return createSet<string>();
	}
}
