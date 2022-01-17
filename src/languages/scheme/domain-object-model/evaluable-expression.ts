// tom-weatherhead/thaw-grammar/src/languages/scheme/domain-object-model/evaluable-expression.ts

import { ifDefinedThenElse } from 'thaw-common-utilities.ts';

import { EvaluationException } from 'thaw-interpreter-core';

import { IEnvironmentFrame } from '../../../common/domain-object-model/environment-frame';
import { IExpression } from '../../../common/domain-object-model/iexpression';
import { IGlobalInfo } from '../../../common/domain-object-model/iglobal-info';
import { isVariableT, IVariable } from '../../../common/domain-object-model/variable';

import { ISExpression } from '../../lisp/domain-object-model/isexpression';

import { ICallableSExpression } from './icallable-sexpression';

export class EvaluableExpression implements IExpression<ISExpression> {
	constructor(
		public readonly firstExpression: IExpression<ISExpression>,
		public readonly expressionList: IExpression<ISExpression>[]
	) {}

	public evaluate(
		globalInfo: IGlobalInfo<ISExpression>,
		localEnvironment?: IEnvironmentFrame<ISExpression>,
		// eslint-disable-next-line @typescript-eslint/no-unused-vars
		options?: unknown
	): ISExpression {
		const env = ifDefinedThenElse(localEnvironment, globalInfo.globalEnvironment);

		if (
			!isVariableT(this.firstExpression) ||
			env.isDefined(this.firstExpression as IVariable<ISExpression>)
		) {
			const firstExprValue = this.firstExpression.evaluate(globalInfo, localEnvironment);

			const callableSExpr = firstExprValue as ICallableSExpression;

			if (callableSExpr === undefined) {
				// TODO: FIXME :
				// if (typeof callableSExpr === 'undefined' || typeof callableSExpr.call === 'undefined' || callableSExpr.call.length !== 3) {
				// Or create a type guard: isCallableSExpression()
				throw new EvaluationException(
					'EvaluableExpression.evaluate() : FirstExpression is not a callable S-Expression'
				);
			}

			// console.log('firstExprValue is callable. Calling it...');

			return callableSExpr.call(this.expressionList, env, globalInfo);
		}

		// throw new EvaluationException('EvaluableExpression.evaluate() : This expression is either bad or not yet supported');

		const firstExprValueX = this.firstExpression.evaluate(globalInfo, localEnvironment);
		const firstExprValueIsCallable = (firstExprValueX as ICallableSExpression) !== undefined;

		throw new EvaluationException(
			`EvaluableExpression.evaluate() : Foo. firstExprValueIsCallable: ${firstExprValueIsCallable}`
		);
	}
}
