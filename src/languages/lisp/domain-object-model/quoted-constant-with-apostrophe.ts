// tom-weatherhead/thaw-grammar/src/languages/lisp/domain-object-model/quoted-constant-with-apostrophe.ts

'use strict';

import { EnvironmentFrame } from '../../../common/domain-object-model/environment-frame';
import { IExpression } from '../../../common/domain-object-model/iexpression';
import { IGlobalInfo } from '../../../common/domain-object-model/iglobal-info';

import { ISExpression } from './isexpression';

export class QuotedConstantWithApostrophe implements IExpression<ISExpression> {
	public readonly sexpression: ISExpression;

	constructor(sexpression: ISExpression) {
		this.sexpression = sexpression;
	}

	public toString(): string {
		// return "'" + this.sexpression.toString();

		return `'${this.sexpression}`;
	}

	/* eslint-disable @typescript-eslint/no-unused-vars */
	public evaluate(
		localEnvironment: EnvironmentFrame<ISExpression>,
		globalInfo: IGlobalInfo<ISExpression>
	): ISExpression {
		return this.sexpression;
	}
	/* eslint-enable @typescript-eslint/no-unused-vars */
}
