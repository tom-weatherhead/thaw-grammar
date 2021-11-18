// tom-weatherhead/thaw-grammar/src/languages/lisp/domain-object-model/sexpression-base.ts

import { IEnvironmentFrame } from '../../../common/domain-object-model/environment-frame';
import { IExpression } from '../../../common/domain-object-model/iexpression';
import { IGlobalInfo } from '../../../common/domain-object-model/iglobal-info';
import { ISExpression } from './isexpression';
import { SExpressionBareBase } from './sexpression-bare-base';

export abstract class SExpressionBase
	extends SExpressionBareBase
	implements IExpression<ISExpression>
{
	/* eslint-disable @typescript-eslint/no-unused-vars */
	public evaluate(
		globalInfo: IGlobalInfo<ISExpression>,
		localEnvironment?: IEnvironmentFrame<ISExpression>,
		options?: unknown
	): ISExpression {
		return this;
	}
	/* eslint-enable @typescript-eslint/no-unused-vars */
}
