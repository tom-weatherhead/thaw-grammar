// tom-weatherhead/thaw-grammar/src/languages/scheme/domain-object-model/closure.ts

import { EvaluationException } from 'thaw-interpreter-core';

import {
	EnvironmentFrame,
	IEnvironmentFrame
} from '../../../common/domain-object-model/environment-frame';
import { IExpression } from '../../../common/domain-object-model/iexpression';
import { IGlobalInfo } from '../../../common/domain-object-model/iglobal-info';
import { IVariable } from '../../../common/domain-object-model/variable';

import { ISExpression } from '../../lisp/domain-object-model/isexpression';
import { SExpressionBase } from '../../lisp/domain-object-model/sexpression-base';

import { ICallableSExpression } from './icallable-sexpression';

export class Closure extends SExpressionBase implements ICallableSExpression {
	constructor(
		public readonly argList: IVariable<ISExpression>[],
		public readonly body: IExpression<ISExpression>,
		public readonly closureEnvironment: IEnvironmentFrame<ISExpression>,
		public readonly line = 0,
		public readonly column = 0
	) {
		super();
	}

	public call(
		expressionList: IExpression<ISExpression>[],
		localEnvironment: IEnvironmentFrame<ISExpression>,
		globalInfo: IGlobalInfo<ISExpression>
	): ISExpression {
		if (expressionList.length !== this.argList.length) {
			throw new EvaluationException(
				`Closure.call() : Expected ${this.argList.length} argument(s), instead of the actual ${expressionList.length} argument(s); body = ${this.body}`
			);
		}

		const evaluatedArguments = expressionList.map((expr: IExpression<ISExpression>) =>
			expr.evaluate(globalInfo, localEnvironment)
		);

		const newEnvironment = new EnvironmentFrame<ISExpression>(this.closureEnvironment);

		// if (globalInfo.Debug) {
		// 	LISPGlobalInfo.CreateStackTraceInNewEnvironmentFrame(localEnvironment, newEnvironment, Line, Column);
		// }

		newEnvironment.compose(this.argList, evaluatedArguments);

		return this.body.evaluate(globalInfo, newEnvironment);
	}

	public override isClosure(): boolean {
		return true;
	}

	public toString(): string {
		return '<closure>';
	}

	/* eslint-disable @typescript-eslint/no-unused-vars */
	public override evaluate(
		globalInfo: IGlobalInfo<ISExpression>,
		localEnvironment?: IEnvironmentFrame<ISExpression>,
		options?: unknown
	): ISExpression {
		return this;
	}
	/* eslint-enable @typescript-eslint/no-unused-vars */
}
