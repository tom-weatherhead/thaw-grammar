// tom-weatherhead/thaw-grammar/src/languages/scheme/domain-object-model/lambda-expression.ts

import { ifDefinedThenElse } from 'thaw-common-utilities.ts';

import { IEnvironmentFrame } from '../../../common/domain-object-model/environment-frame';
import { IExpression } from '../../../common/domain-object-model/iexpression';
import { IGlobalInfo } from '../../../common/domain-object-model/iglobal-info';
import { IVariable } from '../../../common/domain-object-model/variable';
// import { VariableList } from '../../../common/domain-object-model/variable-list';
// import { ArgumentException } from '../../../common/exceptions/argument-exception';
// import { EvaluationException } from '../../../common/exceptions/evaluation-exception';
import { ISExpression } from '../../lisp/domain-object-model/isexpression';

import { Closure } from './closure';

export class LambdaExpression implements IExpression<ISExpression> {
	// public readonly argList: IVariable<ISExpression>[];
	// public readonly body: IExpression<ISExpression>;
	// public readonly line: number;
	// public readonly column: number;

	constructor(
		public readonly argList: IVariable<ISExpression>[],
		public readonly body: IExpression<ISExpression>,
		public readonly line = 0,
		public readonly column = 0
	) {
		// console.log('Creating an instance of LambdaExpression...');
		// console.log(`-> argList is ${typeof argList} ${argList}`);
		// console.log(`-> body is ${typeof body} ${body}`);
		// this.argList = argList;
		// this.body = body;
		// this.line = line;
		// this.column = column;
	}

	public toString(): string {
		return `(lambda ${this.argList} ${this.body})`;
	}

	// public evaluate(
	// 	localEnvironment: EnvironmentFrame<ISExpression>,
	// 	// eslint-disable-next-line @typescript-eslint/no-unused-vars
	// 	globalInfo: IGlobalInfo<ISExpression>
	// ): ISExpression {
	public evaluate(
		globalInfo: IGlobalInfo<ISExpression>,
		localEnvironment?: IEnvironmentFrame<ISExpression>,
		// eslint-disable-next-line @typescript-eslint/no-unused-vars
		options?: unknown
	): ISExpression {
		// console.log('Evaluating an instance of LambdaExpression...');

		return new Closure(
			this.argList,
			this.body,
			ifDefinedThenElse(localEnvironment, globalInfo.globalEnvironment),
			this.line,
			this.column
		);
	}
}

// public class LambdaExpression : IExpression<ISExpression>
// {
//     public readonly VariableList<ISExpression> ArgList;
//     public readonly IExpression<ISExpression> Body;
//     public readonly int Line;
//     public readonly int Column;

//     public LambdaExpression(VariableList<ISExpression> argList, IExpression<ISExpression> body, int line, int column)
//     {
//         ArgList = argList;
//         Body = body;
//         Line = line;
//         Column = column;
//     }

//     public override string ToString()
//     {
//         return string.Format("(lambda {0} {1})", ArgList, Body);
//     }

//     public virtual ISExpression Evaluate(EnvironmentFrame<ISExpression> localEnvironment, IGlobalInfo<ISExpression> globalInfo)
//     {
//         return new Closure(ArgList, Body, localEnvironment, Line, Column);
//     }
// }
