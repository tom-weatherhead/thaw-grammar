// tom-weatherhead/thaw-grammar/src/languages/scheme/domain-object-model/continuation.ts

import { EvaluationException } from 'thaw-interpreter-core';

import { EnvironmentFrame } from '../../../common/domain-object-model/environment-frame';
import { ExpressionList } from '../../../common/domain-object-model/expression-list';
import { IGlobalInfo } from '../../../common/domain-object-model/iglobal-info';

import { ISExpression } from '../../lisp/domain-object-model/isexpression';
import { SExpressionBase } from '../../lisp/domain-object-model/sexpression-base';

import { ContinuationException } from '../exceptions/continuation-exception';

import { ICallableSExpression } from './icallable-sexpression';

export class Continuation extends SExpressionBase implements ICallableSExpression {
	public readonly ccGuid: number; // was Guid in C#
	public readonly line: number;
	public readonly column: number;

	constructor(ccGuid: number, line = 0, column = 0) {
		super();

		this.ccGuid = ccGuid;
		this.line = line;
		this.column = column;
	}

	public toString(): string {
		return '<continuation>';
	}

	public override isClosure(): boolean {
		return true;
	}

	public call(
		args: ExpressionList<ISExpression>,
		localEnvironment: EnvironmentFrame<ISExpression>,
		globalInfo: IGlobalInfo<ISExpression>
	): ISExpression {
		const actualNumArgs = args.value.length;

		if (actualNumArgs !== 1) {
			throw new EvaluationException(
				`Continuation.call() : Expected 1 argument, instead of the actual ${actualNumArgs} arguments`,
				this.line,
				this.column
			);
		}

		throw new ContinuationException(
			this.ccGuid,
			args.value[0].evaluate(localEnvironment, globalInfo)
		);
	}
}
