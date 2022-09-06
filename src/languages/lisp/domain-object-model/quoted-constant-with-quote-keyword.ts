// tom-weatherhead/thaw-grammar/src/languages/lisp/domain-object-model/quoted-constant-with-quote-keyword.ts

import { IEnvironmentFrame } from '../../../common/domain-object-model/environment-frame';
import { IExpression } from '../../../common/domain-object-model/iexpression';
import { IGlobalInfo } from '../../../common/domain-object-model/iglobal-info';

import { ISExpression } from './isexpression';

const typenameQuotedConstantWithQuoteKeyword = 'QuotedConstantWithQuoteKeyword';

export function isQuotedConstantWithQuoteKeyword(
	obj: unknown
): obj is QuotedConstantWithQuoteKeyword {
	const qcqk = obj as QuotedConstantWithQuoteKeyword;

	return typeof qcqk !== 'undefined' && qcqk.typename === typenameQuotedConstantWithQuoteKeyword;
}

export class QuotedConstantWithQuoteKeyword implements IExpression<ISExpression> {
	public readonly typename: string = typenameQuotedConstantWithQuoteKeyword;
	public readonly sexpression: ISExpression;

	constructor(sexpression: ISExpression) {
		this.sexpression = sexpression;
	}

	public toString(): string {
		// return "'" + this.sexpression.toString();

		return `(quote ${this.sexpression})`;
	}

	/* eslint-disable @typescript-eslint/no-unused-vars */
	// public evaluate(
	// 	localEnvironment: EnvironmentFrame<ISExpression>,
	// 	globalInfo: IGlobalInfo<ISExpression>
	// ): ISExpression {
	public evaluate(
		globalInfo: IGlobalInfo<ISExpression>,
		localEnvironment?: IEnvironmentFrame<ISExpression>,
		options?: unknown
	): ISExpression {
		globalInfo;
		localEnvironment;
		options;

		return this.sexpression;
	}
	/* eslint-enable @typescript-eslint/no-unused-vars */
}
