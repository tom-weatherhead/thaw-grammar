// tom-weatherhead/thaw-grammar/src/languages/scheme/domain-object-model/icallable-sexpression.ts

import { IEnvironmentFrame } from '../../../common/domain-object-model/environment-frame';
import { IExpression } from '../../../common/domain-object-model/iexpression';
import { IGlobalInfo } from '../../../common/domain-object-model/iglobal-info';

import { ISExpression } from '../../lisp/domain-object-model/isexpression';

export interface ICallableSExpression extends ISExpression {
	// expectedNumArgs: number;
	readonly line: number;
	readonly column: number;

	call(
		expressionList: IExpression<ISExpression>[],
		localEnvironment: IEnvironmentFrame<ISExpression>,
		globalInfo: IGlobalInfo<ISExpression>
	): ISExpression;
}
