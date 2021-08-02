// iconvertible-to-graph.ts

import { IExpression } from '../../../common/domain-object-model/iexpression';

import { ISExpression } from '../../lisp/domain-object-model/isexpression';

export interface IConvertibleToGraph {
	convertToGraph(): IExpression<ISExpression>;
}

export function isIConvertibleToGraph(obj: unknown): obj is IConvertibleToGraph {
	return typeof (obj as IConvertibleToGraph).convertToGraph === 'function';
}
