// closure.ts

import { IEnvironmentFrame } from '../../../common/domain-object-model/environment-frame';
import { ExpressionList } from '../../../common/domain-object-model/expression-list';
import { IExpression } from '../../../common/domain-object-model/iexpression';
// import { IGlobalInfo } from '../../../common/domain-object-model/iglobal-info';
import { VariableList } from '../../../common/domain-object-model/variable-list';

import { ISExpression } from '../../lisp/domain-object-model/isexpression';

import { Closure } from '../../scheme/domain-object-model/closure';

import { SASLGlobalInfo } from './global-info';
import { isThunk, Thunk } from './thunk';

export class SASLClosure extends Closure {
	constructor(
		argList: VariableList<ISExpression>,
		body: IExpression<ISExpression>,
		closureEnvironment: IEnvironmentFrame<ISExpression>,
		line = 0,
		column = 0
	) {
		super(argList, body, closureEnvironment, line, column);
	}

	// #if !DEAD_CODE
	protected callHelper_EvaluateArguments(
		args: ExpressionList<ISExpression>,
		localEnvironment: IEnvironmentFrame<ISExpression>,
		// eslint-disable-next-line @typescript-eslint/no-unused-vars
		globalInfo: SASLGlobalInfo
	): ISExpression[] {
		return args.value.map((expr) => (isThunk(expr) ? expr : new Thunk(expr, localEnvironment)));
	}

	protected callHelper_PrepareReturnValue(
		result: ISExpression,
		globalInfo: SASLGlobalInfo
	): ISExpression {
		return globalInfo.dethunk(result);
	}
	// #else
	// public override ISExpression Call(ExpressionList<ISExpression> arguments, EnvironmentFrame<ISExpression> localEnvironment, IGlobalInfo<ISExpression> globalInfo)
	// {
	//     var actualNumArgs = arguments.Value.Count;
	//     var expectedNumArgs = ArgList.Value.Count;
	//
	//     if (actualNumArgs != expectedNumArgs)
	//     {
	//         throw new EvaluationException(
	//             string.Format("SASLClosure.Call : Expected {0} argument(s), instead of the actual {1} argument(s); body = {2}",
	//                 expectedNumArgs, actualNumArgs, Body),
	//             Line, Column);
	//     }
	//
	//     var evaluatedArguments = arguments.Value.Select(expr => (ISExpression)((expr is Thunk) ? expr : new Thunk(expr, localEnvironment))).ToList();
	//     var newEnvironment = new EnvironmentFrame<ISExpression>(ClosureEnvironment);
	//
	//     newEnvironment.Compose(ArgList.Value, evaluatedArguments);
	//     ISExpression result = Body.Evaluate(newEnvironment, globalInfo);
	//
	//     return SASLGlobalInfo.DeThunk(ref result, globalInfo);
	// }
	// #endif
}
