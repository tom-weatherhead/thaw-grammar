// tom-weatherhead/thaw-grammar/src/languages/scheme/domain-object-model/icallable-sexpression.ts

'use strict';

import { EnvironmentFrame } from '../../../common/domain-object-model/environment-frame';
import { ExpressionList } from '../../../common/domain-object-model/expression-list';
import { IGlobalInfo } from '../../../common/domain-object-model/iglobal-info';

import { ISExpression } from '../../lisp/domain-object-model/isexpression';

export interface ICallableSExpression extends ISExpression {
	// expectedNumArgs: number;
	line: number;
	column: number;
	call(
		expressionList: ExpressionList<ISExpression>,
		localEnvironment: EnvironmentFrame<ISExpression>,
		globalInfo: IGlobalInfo<ISExpression>
	): ISExpression;
}
