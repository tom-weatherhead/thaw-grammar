// tom-weatherhead/thaw-grammar/src/languages/scheme/domain-object-model/evaluable-expression.ts

import { ifDefinedThenElse } from 'thaw-common-utilities.ts';

import { EvaluationException } from 'thaw-interpreter-core';

import { IEnvironmentFrame } from '../../../common/domain-object-model/environment-frame';
import { IExpression } from '../../../common/domain-object-model/iexpression';
import { IGlobalInfo } from '../../../common/domain-object-model/iglobal-info';
// import { IMacroDefinition } from '../../../common/domain-object-model/imacro-definition';
import { isVariableT } from '../../../common/domain-object-model/variable';

import { ISExpression } from '../../lisp/domain-object-model/isexpression';

import { ICallableSExpression } from './icallable-sexpression';

const typenameEvaluableExpression = 'EvaluableExpression';

export function isEvaluableExpression(obj: unknown): obj is EvaluableExpression {
	const ee = obj as EvaluableExpression;

	return typeof ee !== 'undefined' && ee.typename === typenameEvaluableExpression;
}

export class EvaluableExpression implements IExpression<ISExpression> {
	public readonly typename: string = typenameEvaluableExpression;

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
		// let macroDef: IMacroDefinition | undefined;

		if (isVariableT(this.firstExpression)) {
			const macroDef = globalInfo.macroDefinitions.get(this.firstExpression.name);

			if (typeof macroDef !== 'undefined') {
				return macroDef.invokeMacro(this.expressionList, env, globalInfo);
			}
		}

		if (!isVariableT(this.firstExpression) || env.isDefined(this.firstExpression)) {
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
