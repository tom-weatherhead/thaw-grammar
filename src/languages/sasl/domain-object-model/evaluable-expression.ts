// evaluable-expression.ts

import { IExpression } from '../../../common/domain-object-model/iexpression';

import { ISExpression } from '../../lisp/domain-object-model/isexpression';

import { EvaluableExpression } from '../../scheme/domain-object-model/evaluable-expression';

import { GraphReductionNode } from '../graph-reduction';

import { SASLGlobalInfo } from './global-info';
import { IConvertibleToGraph, isIConvertibleToGraph } from './iconvertible-to-graph';

const typenameSASLEvaluableExpression = 'SASLEvaluableExpression';

export function isSASLEvaluableExpression(obj: unknown): obj is SASLEvaluableExpression {
	const otherSASLEvaluableExpression = obj as SASLEvaluableExpression;

	return (
		typeof otherSASLEvaluableExpression !== 'undefined' &&
		typeof otherSASLEvaluableExpression.typename !== 'undefined' &&
		otherSASLEvaluableExpression.typename === typenameSASLEvaluableExpression
	);
}

export class SASLEvaluableExpression extends EvaluableExpression implements IConvertibleToGraph {
	public override readonly typename = typenameSASLEvaluableExpression;

	protected deThunkSExpression(
		sexpression: ISExpression,
		globalInfo: SASLGlobalInfo
	): ISExpression {
		return globalInfo.dethunk(sexpression);
	}

	public convertToGraph(): IExpression<ISExpression> {
		if (this.expressionList.length !== 1) {
			throw new Error(
				`SASLEvaluableExpression.ConvertToGraph() : Length of ExpressionList is ${this.expressionList.length}; expected 1.`
			);
		}

		const e1 = this.firstExpression as unknown as IConvertibleToGraph;
		const e2 = this.expressionList[0] as unknown as IConvertibleToGraph;

		if (!isIConvertibleToGraph(e1)) {
			throw new Error(
				'SASLEvaluableExpression.ConvertToGraph() : FirstExpression is not an IConvertibleToGraph.'
			);
		} else if (!isIConvertibleToGraph(e2)) {
			throw new Error(
				'SASLEvaluableExpression.ConvertToGraph() : ExpressionList.Value[0] is not an IConvertibleToGraph.'
			);
		}

		return new GraphReductionNode(e1.convertToGraph(), e2.convertToGraph());
	}
}
