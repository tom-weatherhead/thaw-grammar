// tom-weatherhead/thaw-grammar/src/languages/scheme/domain-object-model/primitive-operator.ts

import { EvaluationException, Name } from 'thaw-interpreter-core';

import { IEnvironmentFrame } from '../../../common/domain-object-model/environment-frame';
import { IExpression } from '../../../common/domain-object-model/iexpression';
import { IGlobalInfo } from '../../../common/domain-object-model/iglobal-info';

import { ISExpression } from '../../lisp/domain-object-model/isexpression';
import { LISPOperatorUsage } from '../../lisp/domain-object-model/lisp-operator-usage';
import { SExpressionBase } from '../../lisp/domain-object-model/sexpression-base';

import { ICallableSExpression } from './icallable-sexpression';

// TODO? Create static methods in common/domain-object-model/operator-usage
// that export the following functionality:

// - getNumExpectedArgs(operator: string): number | undefined
//   - return -1 for 'any number of arguments is acceptable'
//   - return undefined for 'unknown operator'
// - getBinaryNumericalOperator(operator: string) : (a: number, b: number) => number
// - getBinaryNumericalPredicate(operator: string) : (a: number, b: number) => boolean
// - enum OperatorType { BinaryNumericalOperator, BinaryNumericalPredicate, ..., Unknown }
// - getOperatorType(operator: string): OperatorType | undefined
// - Then PrimOp.evaluate() looks like this:

// switch (operatorType) {
// 	case OperatorType.BinaryNumericalOperator:
// 		const fn: (a: number, b: number) => number = getBinaryNumericalOperator(operatorName.value);

// 		// return new NumberLiteral(fn((evaluatedArguments[0] as IntegerLiteral).value, (evaluatedArguments[1] as IntegerLiteral).value));
// 		// or:

// 		const evaluatedArgumentsAsNumbers: number[] = evaluatedArguments.map((arg: ISExpression) => (arg as IntegerLiteral).value);

// 		return new NumberLiteral(fn(...evaluatedArgumentsAsNumbers));

// 	case ...
// }

export class PrimOp extends SExpressionBase implements ICallableSExpression {
	// Old (C#) comment: We cannot inherit from SExpressionBase here because we already inherit from LISPOperatorUsage.
	public readonly line: number;
	public readonly column: number;

	constructor(public readonly name: Name) {
		super();
		// this.expectedNumArgs = 2; // Hard-coded for the operator +
		this.line = this.name.line;
		this.column = this.name.column;
	}

	public call(
		expressionList: IExpression<ISExpression>[],
		localEnvironment: IEnvironmentFrame<ISExpression>,
		globalInfo: IGlobalInfo<ISExpression>
	): ISExpression {
		if (
			[
				'+',
				'-',
				'*',
				'/',
				'=',
				'<',
				// '>',
				'number?',
				'symbol?',
				'list?',
				'null?',
				'string?',
				'cons',
				'car',
				'cdr',
				'list',
				'print',
				'floor',
				'random',
				'rplaca',
				'rplacd',
				'string<'
			].indexOf(this.name.value) >= 0
		) {
			const operatorUsage = new LISPOperatorUsage(this.name, expressionList);

			return operatorUsage.evaluate(globalInfo, localEnvironment);
		}

		// First, check the number of arguments. (TODO)
		// Then check the argument types. (TODO)
		// Then:
		const evaluatedArguments = expressionList.map((expr: IExpression<ISExpression>) =>
			expr.evaluate(globalInfo, localEnvironment)
		);

		switch (this.name.value) {
			case 'primop?':
				return evaluatedArguments[0].isPrimOp()
					? globalInfo.trueValue
					: globalInfo.falseValue;

			case 'closure?':
				return evaluatedArguments[0].isClosure()
					? globalInfo.trueValue
					: globalInfo.falseValue;

			default:
				throw new EvaluationException(
					`PrimOp.call() : Unknown operator name '${this.name.value}'`,
					this.name.line,
					this.name.column
				);
		}
	}

	public override isPrimOp(): boolean {
		return true;
	}

	public toString(): string {
		return this.name.value;
	}

	/* eslint-disable @typescript-eslint/no-unused-vars */
	public override evaluate(
		globalInfo: IGlobalInfo<ISExpression>,
		localEnvironment?: IEnvironmentFrame<ISExpression>,
		options?: unknown
	): ISExpression {
		globalInfo;
		localEnvironment;
		options;

		return this;
	}
	/* eslint-enable @typescript-eslint/no-unused-vars */

	public override isEqualTo(other: unknown): boolean {
		other;

		return false; // Or isPrimOp(other) && other.name.value === this.name.value;
	}
}
