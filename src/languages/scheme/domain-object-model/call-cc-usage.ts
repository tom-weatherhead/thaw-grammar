// tom-weatherhead/thaw-grammar/src/languages/scheme/domain-object-model/call-cc-usage.ts

'use strict';

import { EnvironmentFrame } from '../../../common/domain-object-model/environment-frame';
import { ExpressionList } from '../../../common/domain-object-model/expression-list';
import { IExpression } from '../../../common/domain-object-model/iexpression';
import { IGlobalInfo } from '../../../common/domain-object-model/iglobal-info';

import { EvaluationException } from '../../../common/exceptions/evaluation-exception';

import { ISExpression } from '../../lisp/domain-object-model/isexpression';

import { ContinuationException } from '../exceptions/continuation-exception';

import { Closure } from './closure';
import { Continuation } from './continuation';

export class CallCCUsage implements IExpression<ISExpression> {
	public readonly body: IExpression<ISExpression>;

	constructor(body: IExpression<ISExpression>) {
		this.body = body;
	}

	public evaluate(
		localEnvironment: EnvironmentFrame<ISExpression>,
		globalInfo: IGlobalInfo<ISExpression>
	): ISExpression {
		const evaluatedBody = this.body.evaluate(localEnvironment, globalInfo);

		if (!(evaluatedBody instanceof Closure)) {
			throw new EvaluationException(
				'CallCCUsage.evaluate() : Body does not evaluate to a Closure.'
			);
		}

		const closure = evaluatedBody as Closure;

		if (closure.argList.value.length !== 1) {
			throw new EvaluationException(
				`CallCCUsage.evaluate() : The Closure takes ${closure.argList.value.length} arguments instead of the expected 1`,
				closure.line,
				closure.column
			);
		}

		// TODO: Use a singleton class named SequenceNumberGenerator to create the 'guids'.
		const ccGuid = Math.random(); // Guid.NewGuid();
		const exprList = new ExpressionList<ISExpression>();

		exprList.value.push(
			new Continuation(ccGuid, closure.line, closure.column)
		);

		try {
			return closure.call(exprList, localEnvironment, globalInfo);
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
