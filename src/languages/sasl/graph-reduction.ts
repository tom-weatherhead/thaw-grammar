// graph-reduction.ts

import { IExpression } from '../../common/domain-object-model/iexpression';
import { ISExpression } from '../../languages/lisp/domain-object-model/isexpression';

import { SExpressionBase } from '../lisp/domain-object-model/sexpression-base';

// #region SKIOp

const typenameSKIOp = 'SKIOp';

export function isSKIOp(obj: unknown): obj is SKIOp {
	return (obj as SKIOp).typename === typenameSKIOp;
}

export class SKIOp extends SExpressionBase {
	public readonly expectedNumArgs: number;
	public readonly typename = typenameSKIOp;

	constructor(public readonly name: string) {
		super();

		switch (name) {
			case 'S':
				this.expectedNumArgs = 3;
				break;

			case 'K':
				this.expectedNumArgs = 2;
				break;

			case 'I':
				this.expectedNumArgs = 1;
				break;

			default:
				throw new Error(`SKIOp constructor: Invalid name '${name}'`);
		}
	}

	public toString(): string {
		return this.name;
	}

	// public evaluate(): IExpression<ISExpression> {
	// 	throw new Error('SKIOps cannot be evaluated; they must be reduced');
	// }
	//
	// public asValue(): IExpression<ISExpression> | undefined {
	// 	return undefined;
	// }
}

// #endregion

// An interior (non-leaf) node of the graph.

const typenameGraphReductionNode = 'GraphReductionNode';

export function isGraphReductionNode(obj: unknown): obj is GraphReductionNode {
	const otherNode = obj as GraphReductionNode;

	return (
		typeof otherNode !== 'undefined' &&
		typeof otherNode.typename !== 'undefined' &&
		otherNode.typename === typenameGraphReductionNode
	);
}

export class GraphReductionNode extends SExpressionBase {
	public readonly typename = typenameGraphReductionNode;

	constructor(
		public leftChild: IExpression<ISExpression>,
		public rightChild: IExpression<ISExpression>
	) {
		super();

		if (typeof leftChild === 'undefined') {
			throw new Error('GraphReductionNode constructor: leftChild is undefined');
		} else if (typeof rightChild === 'undefined') {
			throw new Error('GraphReductionNode constructor: rightChild is undefined');
		}
	}

	public toString(): string {
		return `(${this.leftChild} ${this.rightChild})`;
	}

	// public evaluate(): IExpression<ISExpression> {
	// 	// NotImplementedException
	// 	throw new Error('GraphReductionNode.evaluate()');
	// }

	// public asValue(): IExpression<ISExpression> | undefined {
	// 	return undefined;
	// }
}
