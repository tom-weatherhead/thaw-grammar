// lambda-expression.ts

import { EnvironmentFrame } from '../../../common/domain-object-model/environment-frame';
// import { ExpressionList } from '../../../common/domain-object-model/expression-list';
import { IExpression } from '../../../common/domain-object-model/iexpression';
import { IGlobalInfo } from '../../../common/domain-object-model/iglobal-info';
import { Variable } from '../../../common/domain-object-model/variable';
import { VariableList } from '../../../common/domain-object-model/variable-list';

// import { EvaluationException } from '../../../common/exceptions/evaluation-exception';

import { ISExpression } from '../../lisp/domain-object-model/isexpression';

import { LambdaExpression } from '../../scheme/domain-object-model/lambda-expression';

import { GraphReductionNode, isGraphReductionNode, SKIOp } from '../graph-reduction';

import { SASLClosure } from './closure';
import { IConvertibleToGraph, isIConvertibleToGraph } from './iconvertible-to-graph';

export class SASLLambdaExpression extends LambdaExpression implements IConvertibleToGraph {
	constructor(
		argList: VariableList<ISExpression>,
		body: IExpression<ISExpression>,
		line = 0,
		column = 0
	) {
		super(argList, body, line, column);
	}

	public override evaluate(
		localEnvironment: EnvironmentFrame<ISExpression>,
		globalInfo: IGlobalInfo<ISExpression>
	): ISExpression {
		return new SASLClosure(this.argList, this.body, localEnvironment, this.line, this.column);
	}

	private makeAbstraction(
		n: IExpression<ISExpression>,
		variable: Variable<ISExpression>
	): IExpression<ISExpression> {
		const nAsGraphReductionNode = n as unknown as GraphReductionNode;

		if (isGraphReductionNode(nAsGraphReductionNode)) {
			const n1 = new GraphReductionNode(
				new SKIOp('S'),
				this.makeAbstraction(nAsGraphReductionNode.leftChild, variable)
			);

			return new GraphReductionNode(
				n1,
				this.makeAbstraction(nAsGraphReductionNode.rightChild, variable)
			);
		}

		const nAsVariable = n as Variable<ISExpression>;

		// if (variable.equals(nAsVariable)) {
		if (variable.name === nAsVariable.name) {
			return new SKIOp('I');
		}

		return new GraphReductionNode(new SKIOp('K'), n);
	}

	public convertToGraph(): IExpression<ISExpression> {
		if (this.argList.value.length !== 1) {
			throw new Error(
				`SASLLambdaExpression.ConvertToGraph() : There are ${this.argList.value.length} arguments; expected 1.`
			);
		}

		const convertibleBody = this.body as unknown as IConvertibleToGraph;

		if (!isIConvertibleToGraph(convertibleBody)) {
			throw new Error(
				'SASLLambdaExpression.ConvertToGraph() : Body is not IConvertibleToGraph.'
			);
		}

		return this.makeAbstraction(convertibleBody.convertToGraph(), this.argList.value[0]);
	}
}
