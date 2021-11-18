// tom-weatherhead/thaw-grammar/src/languages/scheme/domain-object-model/closure.ts

import { EvaluationException } from 'thaw-interpreter-core';

import {
	EnvironmentFrame,
	IEnvironmentFrame
} from '../../../common/domain-object-model/environment-frame';
import { ExpressionList } from '../../../common/domain-object-model/expression-list';
import { IExpression } from '../../../common/domain-object-model/iexpression';
import { IGlobalInfo } from '../../../common/domain-object-model/iglobal-info';
import { VariableList } from '../../../common/domain-object-model/variable-list';

// import { ArgumentException } from '../../../common/exceptions/argument-exception';

import { ISExpression } from '../../lisp/domain-object-model/isexpression';
import { SExpressionBase } from '../../lisp/domain-object-model/sexpression-base';

import { ICallableSExpression } from './icallable-sexpression';

export class Closure extends SExpressionBase implements ICallableSExpression {
	// public readonly argList: VariableList<ISExpression>;
	// public readonly body: IExpression<ISExpression>;
	// public readonly closureEnvironment: IEnvironmentFrame<ISExpression>;
	// public readonly line: number;
	// public readonly column: number;

	constructor(
		public readonly argList: VariableList<ISExpression>,
		public readonly body: IExpression<ISExpression>,
		public readonly closureEnvironment: IEnvironmentFrame<ISExpression>,
		public readonly line = 0,
		public readonly column = 0
	) {
		super();

		// console.log('Creating an instance of Closure...');

		// this.argList = argList;
		// this.body = body;
		// this.closureEnvironment = closureEnvironment;
		// this.line = line;
		// this.column = column;
	}

	public call(
		expressionList: ExpressionList<ISExpression>,
		localEnvironment: IEnvironmentFrame<ISExpression>,
		globalInfo: IGlobalInfo<ISExpression>
	): ISExpression {
		if (expressionList.value.length !== this.argList.value.length) {
			throw new EvaluationException(
				`Closure.call() : Expected ${this.argList.value.length} argument(s), instead of the actual ${expressionList.value.length} argument(s); body = ${this.body}`
			);
		}

		const evaluatedArguments = expressionList.value.map((expr: IExpression<ISExpression>) =>
			expr.evaluate(globalInfo, localEnvironment)
		);

		const newEnvironment = new EnvironmentFrame<ISExpression>(this.closureEnvironment);

		// if (globalInfo.Debug) {
		// 	LISPGlobalInfo.CreateStackTraceInNewEnvironmentFrame(localEnvironment, newEnvironment, Line, Column);
		// }

		newEnvironment.compose(this.argList.value, evaluatedArguments);

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
		// console.log('Evaluating an instance of Closure...');

		return this;
	}
	/* eslint-enable @typescript-eslint/no-unused-vars */
}
