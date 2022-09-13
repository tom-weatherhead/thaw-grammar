// tom-weatherhead/thaw-grammar/src/languages/sasl/domain-object-model/lambda-expression.ts

// import { ifDefinedThenElse } from 'thaw-common-utilities.ts';

// import { IEnvironmentFrame } from '../../../common/domain-object-model/environment-frame';
import { IExpression } from '../../../common/domain-object-model/iexpression';
// import { IGlobalInfo } from '../../../common/domain-object-model/iglobal-info';
import { IVariable } from '../../../common/domain-object-model/variable';

import { ISExpression } from '../../lisp/domain-object-model/isexpression';

// import { Closure } from '../../scheme/domain-object-model/closure';
import { LambdaExpression } from '../../scheme/domain-object-model/lambda-expression';

import { GraphReductionNode, isGraphReductionNode, SKIOp } from '../graph-reduction';

// import { SASLClosure } from './closure';
import { IConvertibleToGraph, isIConvertibleToGraph } from './iconvertible-to-graph';

export class SASLLambdaExpression extends LambdaExpression implements IConvertibleToGraph {
	constructor(
		argList: IVariable<ISExpression>[],
		body: IExpression<ISExpression>,
		line = 0,
		column = 0
	) {
		super(argList, body, line, column);
	}

	// public override evaluate(
	// 	globalInfo: IGlobalInfo<ISExpression>,
	// 	localEnvironment?: IEnvironmentFrame<ISExpression>,
	// 	// eslint-disable-next-line @typescript-eslint/no-unused-vars
	// 	options?: unknown
	// ): ISExpression {
	// 	options;
	//
	// 	return new Closure(
	// 		this.argList,
	// 		this.body,
	// 		ifDefinedThenElse(localEnvironment, globalInfo.globalEnvironment),
	// 		this.line,
	// 		this.column
	// 	);
	// }

	private makeAbstraction(
		n: IExpression<ISExpression>,
		variable: IVariable<ISExpression>
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

		const nAsVariable = n as IVariable<ISExpression>;

		// if (variable.equals(nAsVariable)) {
		if (variable.name === nAsVariable.name) {
			return new SKIOp('I');
		}

		return new GraphReductionNode(new SKIOp('K'), n);
	}

	public convertToGraph(): IExpression<ISExpression> {
		if (this.argList.length !== 1) {
			throw new Error(
				`SASLLambdaExpression.ConvertToGraph() : There are ${this.argList.length} arguments; expected 1.`
			);
		}

		const convertibleBody = this.body as unknown as IConvertibleToGraph;

		if (!isIConvertibleToGraph(convertibleBody)) {
			throw new Error(
				'SASLLambdaExpression.ConvertToGraph() : Body is not IConvertibleToGraph.'
			);
		}

		return this.makeAbstraction(convertibleBody.convertToGraph(), this.argList[0]);
	}
}
