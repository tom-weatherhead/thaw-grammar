// prim-op.ts

import { Name } from 'thaw-interpreter-core';

import { IEnvironmentFrame } from '../../../common/domain-object-model/environment-frame';
import { IExpression } from '../../../common/domain-object-model/iexpression';
import { IGlobalInfo } from '../../../common/domain-object-model/iglobal-info';

import { ISExpression } from '../../lisp/domain-object-model/isexpression';
import { SExpressionList } from '../../lisp/domain-object-model/sexpression-list';
import { NullSExpression } from '../../lisp/domain-object-model/null-sexpression';

import { PrimOp } from '../../scheme/domain-object-model/primitive-operator';

import { isSASLEvaluableExpression, SASLEvaluableExpression } from './evaluable-expression';
import { SASLGlobalInfo } from './global-info';
import { IConvertibleToGraph } from './iconvertible-to-graph';
import { isThunk, Thunk } from './thunk';

function isISExpression(obj: unknown): obj is ISExpression {
	const otherISExpression = obj as ISExpression;

	return (
		typeof otherISExpression !== 'undefined' &&
		typeof otherISExpression.isClosure === 'function'
	);
}

function isSExpressionList(obj: unknown): obj is SExpressionList {
	const otherSExpressionList = obj as SExpressionList;

	return (
		typeof otherSExpressionList !== 'undefined' &&
		typeof otherSExpressionList.head !== 'undefined' &&
		typeof otherSExpressionList.tail !== 'undefined'
	);
}

export class SASLPrimOp extends PrimOp implements IConvertibleToGraph {
	constructor(operatorName: Name) {
		super(operatorName);
	}

	// protected override bool TryGetExpectedNumArgs(IGlobalInfo<ISExpression> globalInfo, out int result)
	// {
	//
	// 	switch (OperatorName.Value) {
	// 		case "cond":
	// 			result = -1;
	// 			return true;
	//
	// 		case "if":
	// 			result = 3;
	// 			return true;
	//
	// 		default:
	// 			return super.TryGetExpectedNumArgs(globalInfo, out result);
	// 	}
	// }

	private executeIf(
		argumentsAsSExpressions: ISExpression[],
		globalInfo: SASLGlobalInfo
	): ISExpression {
		let result: ISExpression;
		let conditionValue = argumentsAsSExpressions[0];

		conditionValue = globalInfo.dethunk(conditionValue);

		if (!conditionValue.isNull()) {
			result = argumentsAsSExpressions[1];
		} else {
			result = argumentsAsSExpressions[2];
		}

		return globalInfo.dethunk(result);
	}

	private executeCond(
		argumentsAsSExpressions: ISExpression[],
		globalInfo: IGlobalInfo<ISExpression>
	): ISExpression {
		for (const argumentAsSExpression of argumentsAsSExpressions) {
			if (!isThunk(argumentAsSExpression)) {
				throw new Error('ExecuteCond : argumentAsSExpression is not a Thunk.');
			}

			let thunk = argumentAsSExpression as Thunk;

			while (isThunk(thunk.body)) {
				thunk = thunk.body as Thunk;
			}

			if (!isSASLEvaluableExpression(thunk.body)) {
				// The thunk body is not really an evaluable expression, but that's how the parser sees it.
				throw new Error('ExecuteCond : thunk.Body is not an SASLEvaluableExpression.');
			}

			const evExp = thunk.body as SASLEvaluableExpression;
			const conditionExpression = evExp.firstExpression;
			const conditionValue = conditionExpression.evaluate(globalInfo, thunk.thunkEnvironment);

			if (conditionValue.isNull()) {
				continue;
			}

			if (evExp.expressionList.length !== 1) {
				throw new Error(
					`ExecuteCond : evExp.ExpressionList length is ${evExp.expressionList.length} rather than 1.`
				);
			}

			return evExp.expressionList[0].evaluate(globalInfo, thunk.thunkEnvironment);
		}

		return new NullSExpression();
	}

	public override call(
		args: IExpression<ISExpression>[],
		localEnvironment: IEnvironmentFrame<ISExpression>,
		globalInfo: IGlobalInfo<ISExpression>
	): ISExpression {
		// const actualNumArgs = args.value.length;
		// let expectedNumArgs: number;

		// if (!this.tryGetExpectedNumArgs(globalInfo, out expectedNumArgs)) {
		// 	throw new Error(string.Format("SASLPrimOp : Unknown operator name '{0}'.", this.name.value));
		// } else if (expectedNumArgs >= 0 && actualNumArgs != expectedNumArgs) {
		// 	throw new Error(string.Format("SASLPrimOp : Expected {0} arguments for operator '{1}', instead of the actual {2} arguments.",
		// 	expectedNumArgs, this.name.value, actualNumArgs));
		// }

		// ThAW 2012/12/07 : Do not create thunks out of arguments that are already S-expressions (i.e. values) : See page 202, exercise 6.
		const argumentsAsSExpressions = args.map((expr) =>
			isISExpression(expr) ? (expr as ISExpression) : new Thunk(expr, localEnvironment)
		);

		switch (this.name.value) {
			case 'cons':
				return new SExpressionList(argumentsAsSExpressions[0], argumentsAsSExpressions[1]);

			case 'list':
				return SExpressionList.makeFromList(argumentsAsSExpressions);

			case 'if':
				return this.executeIf(argumentsAsSExpressions, globalInfo as SASLGlobalInfo);

			case 'cond':
				return this.executeCond(argumentsAsSExpressions, globalInfo);

			default:
				break;
		}

		const evaluatedArguments = argumentsAsSExpressions.map((sexpr) =>
			isThunk(sexpr) ? (sexpr as Thunk).dethunk(globalInfo) : sexpr
		);
		let list: SExpressionList;

		switch (this.name.value) {
			case 'car':
				list = evaluatedArguments[0] as SExpressionList;

				if (!isSExpressionList(list)) {
					throw new Error(
						`car: first arg is not an SExpressionList; it is an ? with value '${evaluatedArguments[0]}'.`
					);
				}

				return (globalInfo as SASLGlobalInfo).dethunk(list.head);

			case 'cdr':
				list = evaluatedArguments[0] as SExpressionList;

				if (!isSExpressionList(list)) {
					// throw new Error(string.Format(
					// "cdr: first arg is not an SExpressionList; it is an '{0}' with value '{1}'.",
					// evaluatedArguments[0].GetType().FullName, evaluatedArguments[0]));
					throw new Error(
						`cdr: first arg is not an SExpressionList; it is an ? with value '${evaluatedArguments[0]}'.`
					);
				}

				return (globalInfo as SASLGlobalInfo).dethunk(list.tail);

			default:
				// It is safe to pass a null localEnvironment to EvaluateAux(), since it only uses localEnvironment to evaluate user-defined LISP functions.
				// const result = this.evaluateAux(evaluatedArguments, null, globalInfo);
				const result = super.call(args, localEnvironment, globalInfo);

				if (isThunk(result)) {
					throw new Error('SASLPrimOp.Call was about to return a Thunk.');
				}

				return result;
		}
	}

	public convertToGraph(): IExpression<ISExpression> {
		return this;
	}
}
