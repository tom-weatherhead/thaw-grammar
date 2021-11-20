// tom-weatherhead/thaw-grammar/src/languages/scheme/domain-object-model/continuation.ts

import { EvaluationException } from 'thaw-interpreter-core';

import { IEnvironmentFrame } from '../../../common/domain-object-model/environment-frame';
import { IExpression } from '../../../common/domain-object-model/iexpression';
import { IGlobalInfo } from '../../../common/domain-object-model/iglobal-info';

import { ISExpression } from '../../lisp/domain-object-model/isexpression';
import { SExpressionBase } from '../../lisp/domain-object-model/sexpression-base';

import { ContinuationException } from '../exceptions/continuation-exception';

import { ICallableSExpression } from './icallable-sexpression';

export class Continuation extends SExpressionBase implements ICallableSExpression {
	constructor(
		public readonly ccGuid: number, // type was Guid in C#
		public readonly line = 0,
		public readonly column = 0
	) {
		super();
	}

	public toString(): string {
		return '<continuation>';
	}

	public override isClosure(): boolean {
		return true;
	}

	public call(
		args: IExpression<ISExpression>[],
		localEnvironment: IEnvironmentFrame<ISExpression>,
		globalInfo: IGlobalInfo<ISExpression>
	): ISExpression {
		const actualNumArgs = args.length;

		if (actualNumArgs !== 1) {
			throw new EvaluationException(
				`Continuation.call() : Expected 1 argument, instead of the actual ${actualNumArgs} arguments`,
				this.line,
				this.column
			);
		}

		throw new ContinuationException(
			this.ccGuid,
			args[0].evaluate(globalInfo, localEnvironment)
		);
	}
}
