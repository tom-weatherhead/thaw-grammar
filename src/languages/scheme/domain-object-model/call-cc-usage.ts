// tom-weatherhead/thaw-grammar/src/languages/scheme/domain-object-model/call-cc-usage.ts

import { ifDefinedThenElse } from 'thaw-common-utilities.ts';

import { EvaluationException } from 'thaw-interpreter-core';

import { IEnvironmentFrame } from '../../../common/domain-object-model/environment-frame';
import { IExpression } from '../../../common/domain-object-model/iexpression';
import { IGlobalInfo } from '../../../common/domain-object-model/iglobal-info';

import { ISExpression } from '../../lisp/domain-object-model/isexpression';

import { ContinuationException } from '../exceptions/continuation-exception';

import { Closure } from './closure';
import { Continuation } from './continuation';

const typenameCallCCUsage = 'CallCCUsage';

export function isCallCCUsage(obj: unknown): obj is CallCCUsage {
	const cccu = obj as CallCCUsage;

	return typeof cccu !== 'undefined' && cccu.typename === typenameCallCCUsage;
}

export class CallCCUsage implements IExpression<ISExpression> {
	public readonly typename: string = typenameCallCCUsage;

	constructor(public readonly body: IExpression<ISExpression>) {}

	public evaluate(
		globalInfo: IGlobalInfo<ISExpression>,
		localEnvironment?: IEnvironmentFrame<ISExpression>,
		// eslint-disable-next-line @typescript-eslint/no-unused-vars
		options?: unknown
	): ISExpression {
		const evaluatedBody = this.body.evaluate(globalInfo, localEnvironment);

		options;

		if (!(evaluatedBody instanceof Closure)) {
			throw new EvaluationException(
				'CallCCUsage.evaluate() : Body does not evaluate to a Closure.'
			);
		}

		const closure = evaluatedBody as Closure;

		if (closure.argList.length !== 1) {
			throw new EvaluationException(
				`CallCCUsage.evaluate() : The Closure takes ${closure.argList.length} arguments instead of the expected 1`,
				closure.line,
				closure.column
			);
		}

		// TODO: Use a singleton class named SequenceNumberGenerator to create the 'guids'.
		const ccGuid = Math.random(); // Guid.NewGuid();
		const exprList = [] as IExpression<ISExpression>[];

		exprList.push(new Continuation(ccGuid, closure.line, closure.column));

		try {
			return closure.call(
				exprList,
				ifDefinedThenElse(localEnvironment, globalInfo.globalEnvironment),
				globalInfo
			);
		} catch (ex) {
			if (!(ex instanceof ContinuationException)) {
				throw ex;
			}

			const ce = ex as ContinuationException;

			if (ce.ccGuid !== ccGuid) {
				throw ex;
			}

			return ce.returnValue;
		}
	}
}
