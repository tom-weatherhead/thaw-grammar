// tom-weatherhead/thaw-grammar/src/languages/scheme/domain-object-model/evaluable-expression.ts

import { EnvironmentFrame } from '../../../common/domain-object-model/environment-frame';
import { ExpressionList } from '../../../common/domain-object-model/expression-list';
import { IExpression } from '../../../common/domain-object-model/iexpression';
import { IGlobalInfo } from '../../../common/domain-object-model/iglobal-info';
import { Variable } from '../../../common/domain-object-model/variable';

// import { ArgumentException } from '../../../common/exceptions/argument-exception';
import { EvaluationException } from '../../../common/exceptions/evaluation-exception';

import { ISExpression } from '../../lisp/domain-object-model/isexpression';

import { ICallableSExpression } from './icallable-sexpression';

export class EvaluableExpression implements IExpression<ISExpression> {
	public readonly firstExpression: IExpression<ISExpression>;
	public readonly expressionList: ExpressionList<ISExpression>;

	constructor(
		firstExpression: IExpression<ISExpression>,
		expressionList: ExpressionList<ISExpression>
	) {
		// console.log('Creating an instance of EvaluableExpression...');
		this.firstExpression = firstExpression;
		this.expressionList = expressionList;
	}

	public evaluate(
		localEnvironment: EnvironmentFrame<ISExpression>,
		globalInfo: IGlobalInfo<ISExpression>
	): ISExpression {
		// console.log('Evaluating an instance of EvaluableExpression...');
		// const firstExprAsVariable = this.firstExpression as Variable<ISExpression>;
		// console.log('this.firstExpression =', this.firstExpression);
		// console.log('this.firstExpression as Variable<ISExpression> =', this.firstExpression as Variable<ISExpression>);
		// console.log('this.firstExpression instanceof Variable<ISExpression> =', this.firstExpression instanceof Variable);

		// if (firstExprAsVariable === undefined || localEnvironment.isDefined(firstExprAsVariable)) {
		// if ((this.firstExpression instanceof Variable<ISExpression>) || localEnvironment.isDefined(firstExprAsVariable)) {
		if (
			!(this.firstExpression instanceof Variable) ||
			localEnvironment.isDefined(this.firstExpression as Variable<ISExpression>)
		) {
			const firstExprValue = this.firstExpression.evaluate(localEnvironment, globalInfo);

			// firstExprValue = DeThunkSExpression(firstExprValue, globalInfo);

			// console.log('firstExprValue as ICallableSExpression =', firstExprValue as ICallableSExpression);
			// console.log('firstExprValue instanceof ICallableSExpression =', (firstExprValue instanceof ICallableSExpression));
			// console.log('firstExprValue.isPrimOp() =', firstExprValue.isPrimOp());
			// console.log('firstExprValue.isClosure() =', firstExprValue.isClosure());

			const callableSExpr = firstExprValue as ICallableSExpression;

			if (callableSExpr === undefined) {
				throw new EvaluationException(
					'EvaluableExpression.evaluate() : FirstExpression is not a callable S-Expression'
				);
			}

			// console.log('firstExprValue is callable. Calling it...');

			return callableSExpr.call(this.expressionList, localEnvironment, globalInfo);
		}

		// throw new EvaluationException('EvaluableExpression.evaluate() : This expression is either bad or not yet supported');

		const firstExprValueX = this.firstExpression.evaluate(localEnvironment, globalInfo);
		const firstExprValueIsCallable = (firstExprValueX as ICallableSExpression) !== undefined;

		throw new EvaluationException(
			`EvaluableExpression.evaluate() : Foo. firstExprValueIsCallable: ${firstExprValueIsCallable}`
		);
	}
}

// public class EvaluableExpression : IExpression<ISExpression>
// {
//     public readonly IExpression<ISExpression> FirstExpression;
//     public readonly ExpressionList<ISExpression> ExpressionList;

//     public EvaluableExpression(IExpression<ISExpression> firstExpression, ExpressionList<ISExpression> expressionList)
//     {
//         FirstExpression = firstExpression;
//         ExpressionList = expressionList;
//     }

//     public override string ToString()
//     {

//         if (ExpressionList.Value.Count == 0)
//         {
//             return string.Format("({0})", FirstExpression);
//         }

//         return string.Format("({0} {1})", FirstExpression, ExpressionList);
//     }

//     protected virtual ISExpression DeThunkSExpression(ISExpression sexpression, IGlobalInfo<ISExpression> globalInfo)
//     {
//         return sexpression;
//     }

//     public ISExpression Evaluate(EnvironmentFrame<ISExpression> localEnvironment, IGlobalInfo<ISExpression> globalInfo)
//     {
//         var firstExprAsVariable = FirstExpression as Variable<ISExpression>;

//         if (firstExprAsVariable == null || localEnvironment.IsDefined(firstExprAsVariable))
//         {
//             var firstExprValue = FirstExpression.Evaluate(localEnvironment, globalInfo);

//             firstExprValue = DeThunkSExpression(firstExprValue, globalInfo);

//             var callableSExpr = firstExprValue as ICallableSExpression;

//             if (callableSExpr == null)
//             {
//                 throw new Exception(string.Format("EvaluableExpression.Evaluate : FirstExpression is not a callable S-Expression; it is a {0}: {1}",
//                     firstExprValue.GetType().FullName, firstExprValue));
//             }

//             return callableSExpr.Call(ExpressionList, localEnvironment, globalInfo);
//         }
//         else
//         {
//             // Is FirstExpression the name of a macro?
//             var name = new Name(firstExprAsVariable.Name, firstExprAsVariable.Line, firstExprAsVariable.Column);

//             if (!globalInfo.MacroDefinitions.ContainsKey(name))
//             {
//                 throw new EvaluationException(
//                     string.Format("Could not find '{0}' in the MacroDefinitions", name.Value),
//                     name.Line, name.Column);
//             }

//             var macro = globalInfo.MacroDefinitions[name];

//             if (ExpressionList.Value.Count != macro.ArgumentCount)
//             {
//                 throw new EvaluationException(
//                     string.Format("The macro '{0}' expects {1} argument(s); {2} were passed in",
//                         name.Value, macro.ArgumentCount, ExpressionList.Value.Count),
//                     name.Line, name.Column);
//             }

//             return macro.InvokeMacro(ExpressionList.Value, localEnvironment, globalInfo);
//         }
//     }
// }
