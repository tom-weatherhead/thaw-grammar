// tom-weatherhead/thaw-grammar/src/languages/lisp/domain-object-model/quoted-constant-with-apostrophe.ts

import { IEnvironmentFrame } from '../../../common/domain-object-model/environment-frame';
import { IExpression } from '../../../common/domain-object-model/iexpression';
import { IGlobalInfo } from '../../../common/domain-object-model/iglobal-info';

import { ISExpression } from './isexpression';

const typenameQuotedConstantWithApostrophe = 'QuotedConstantWithApostrophe';

export function isQuotedConstantWithApostrophe(obj: unknown): obj is QuotedConstantWithApostrophe {
	const qca = obj as QuotedConstantWithApostrophe;

	return typeof qca !== 'undefined' && qca.typename === typenameQuotedConstantWithApostrophe;
}

export class QuotedConstantWithApostrophe implements IExpression<ISExpression> {
	public readonly typename: string = typenameQuotedConstantWithApostrophe;
	public readonly sexpression: ISExpression;

	constructor(sexpression: ISExpression) {
		this.sexpression = sexpression;
	}

	public toString(): string {
		// return "'" + this.sexpression.toString();

		return `'${this.sexpression}`;
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
		return this.sexpression;
	}
	/* eslint-enable @typescript-eslint/no-unused-vars */
}
