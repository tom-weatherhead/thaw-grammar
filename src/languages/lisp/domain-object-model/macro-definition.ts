// tom-weatherhead/thaw-grammar/src/languages/lisp/domain-object-model/macro-definition.ts

import { Name } from 'thaw-interpreter-core';

import { isBeginUsage } from '../../../common/domain-object-model/begin-usage';
import { isCondUsage } from '../../../common/domain-object-model/cond-usage';
import {
	EnvironmentFrame,
	IEnvironmentFrame
} from '../../../common/domain-object-model/environment-frame';
import { isFunctionDefinition } from '../../../common/domain-object-model/function-definition';
import { isIfUsage } from '../../../common/domain-object-model/if-usage';
import { IExpression } from '../../../common/domain-object-model/iexpression';
import { IGlobalInfo } from '../../../common/domain-object-model/iglobal-info';
import { IMacroDefinition } from '../../../common/domain-object-model/imacro-definition';
import { isLetUsage } from '../../../common/domain-object-model/let-usage';
import { isLetStarUsage } from '../../../common/domain-object-model/let-star-usage';
import { isOperatorUsage } from '../../../common/domain-object-model/operator-usage';
import { isSetUsage } from '../../../common/domain-object-model/set-usage';
import { IVariable } from '../../../common/domain-object-model/variable';
import { isWhileUsage } from '../../../common/domain-object-model/while-usage';

import { ISExpression } from './isexpression';
import { isNullSExpression } from './null-sexpression';
import { isQuotedConstantWithApostrophe } from './quoted-constant-with-apostrophe';
import { isQuotedConstantWithQuoteKeyword } from './quoted-constant-with-quote-keyword';
import { isSExpressionList, SExpressionList } from './sexpression-list';

import { isCallCCUsage } from '../../scheme/domain-object-model/call-cc-usage';
import { isEvaluableExpression } from '../../scheme/domain-object-model/evaluable-expression';
import { isLambdaExpression } from '../../scheme/domain-object-model/lambda-expression';
import { isLetRecUsage } from '../../scheme/domain-object-model/let-rec-usage';

const typenameMacroDefinition = 'MacroDefinition';

export function isMacroDefinition(obj: unknown): obj is MacroDefinition {
	const md = obj as MacroDefinition;

	return typeof md !== 'undefined' && md.typename === typenameMacroDefinition;
}

export class MacroDefinition implements IExpression<ISExpression>, IMacroDefinition<ISExpression> {
	public readonly typename: string = typenameMacroDefinition;
	public readonly argumentCount: number; // This is a 'get' accessor.

	constructor(
		public readonly macroName: Name,
		public readonly argList: IVariable<ISExpression>[],
		public readonly body: IExpression<ISExpression>
	) {
		this.argumentCount = this.argList.length;
	}

	// public override string ToString() {
	// 	return string.Format("(define-macro {0} {1} {2})", MacroName, ArgList, Body);
	// }
	//
	// public int ArgumentCount {
	// 	get
	// 	{
	// 		return ArgList.Value.Count;
	// 	}
	// }

	/* eslint-disable @typescript-eslint/no-unused-vars */
	public evaluate(
		globalInfo: IGlobalInfo<ISExpression>,
		localEnvironment?: IEnvironmentFrame<ISExpression>,
		options?: unknown
	): ISExpression {
		localEnvironment;
		options;

		globalInfo.macroDefinitions.set(this.macroName.value, this);

		return globalInfo.trueValue;
	}
	/* eslint-enable @typescript-eslint/no-unused-vars */

	// private sExpressionListToString_ApostrophesToQuoteKeywords(l: SExpressionList): string {
	// 	const headAsString = this.objectToString_ApostrophesToQuoteKeywords(l.head);
	//
	// 	if (isNullSExpression(l.tail)) {
	// 		return headAsString;
	// 		// } else if (isThunk(l.tail)) {
	// 		// 	return `${headAsString} ${l.tail}`;
	// 	} else if (isSExpressionList(l.tail)) {
	// 		return `${headAsString} ${this.sExpressionListToString_ApostrophesToQuoteKeywords(
	// 			l.tail
	// 		)}`;
	// 	} else {
	// 		// Tail is a symbol, an integer literal, a string, a closure, etc.
	// 		return `${headAsString} . ${this.objectToString_ApostrophesToQuoteKeywords(l.tail)}`;
	// 	}
	// }

	public objectToString_ApostrophesToQuoteKeywords(expr: unknown): string {
		if (isFunctionDefinition<ISExpression>(expr)) {
			return `(define ${expr.functionName} (${expr.argList
				.map((a) => a.name)
				.join(' ')}) ${this.objectToString_ApostrophesToQuoteKeywords(expr.body)})`;
		} else if (isIfUsage<ISExpression>(expr)) {
			return `(if ${this.objectToString_ApostrophesToQuoteKeywords(
				expr.condition
			)} ${this.objectToString_ApostrophesToQuoteKeywords(
				expr.ifBody
			)} ${this.objectToString_ApostrophesToQuoteKeywords(expr.elseBody)})`;
		} else if (isWhileUsage<ISExpression>(expr)) {
			return `(while ${this.objectToString_ApostrophesToQuoteKeywords(
				expr.condition
			)} ${this.objectToString_ApostrophesToQuoteKeywords(expr.body)})`;
		} else if (isSetUsage<ISExpression>(expr)) {
			return `(set ${expr.variableName} ${this.objectToString_ApostrophesToQuoteKeywords(
				expr.expression
			)})`;
		} else if (isBeginUsage<ISExpression>(expr)) {
			return `(begin ${this.objectToString_ApostrophesToQuoteKeywords(
				expr.firstExpression
			)} ${expr.expressionList
				.map((e) => this.objectToString_ApostrophesToQuoteKeywords(e))
				.join(' ')})`;
		} else if (isCondUsage<ISExpression>(expr)) {
			const exprPairListString = expr.exprPairList
				.map(
					([expr1, expr2]: [IExpression<ISExpression>, IExpression<ISExpression>]) =>
						`(${this.objectToString_ApostrophesToQuoteKeywords(
							expr1
						)} ${this.objectToString_ApostrophesToQuoteKeywords(expr2)})`
				)
				.join(' ');

			return `(cond ${exprPairListString})`;
		} else if (isLetUsage<ISExpression>(expr)) {
			const bindingsString = expr.bindings
				.map(
					([v, e]: [IVariable<ISExpression>, IExpression<ISExpression>]) =>
						`(${v} ${this.objectToString_ApostrophesToQuoteKeywords(e)})`
				)
				.join(' ');

			return `(let (${bindingsString}) ${this.objectToString_ApostrophesToQuoteKeywords(
				expr.expression
			)})`;
		} else if (isLetStarUsage<ISExpression>(expr)) {
			const bindingsString = expr.bindings
				.map(
					([v, e]: [IVariable<ISExpression>, IExpression<ISExpression>]) =>
						`(${v} ${this.objectToString_ApostrophesToQuoteKeywords(e)})`
				)
				.join(' ');

			return `(let* (${bindingsString}) ${this.objectToString_ApostrophesToQuoteKeywords(
				expr.expression
			)})`;
		} else if (isOperatorUsage<ISExpression>(expr) /* && !(expr is Scheme.PrimOp) */) {
			if (expr.expressionList.length === 0) {
				return `(${expr.operatorName})`;
			}

			const exprListString = expr.expressionList
				.map((e) => this.objectToString_ApostrophesToQuoteKeywords(e))
				.join(' ');

			return `(${expr.operatorName} ${exprListString})`;
		} else if (isQuotedConstantWithApostrophe(expr)) {
			return `(quote ${expr.sexpression})`;
		}
		// 	/*
		// else if (expr is SExpressionList) // Not an IExpression<ISExpression>
		// {
		// 	return string.Format("({0})", SExpressionListToString_ApostrophesToQuoteKeywords((SExpressionList)expr));
		// }
		// 	 */
		else if (isMacroDefinition(expr)) {
			return `(define-macro ${expr.macroName} ${
				expr.argList
			} ${this.objectToString_ApostrophesToQuoteKeywords(expr.body)})`;
		} else if (isLambdaExpression(expr)) {
			return `(lambda ${expr.argList} ${this.objectToString_ApostrophesToQuoteKeywords(
				expr.body
			)})`;
		} else if (isEvaluableExpression(expr)) {
			const feAsString = this.objectToString_ApostrophesToQuoteKeywords(expr.firstExpression);

			if (expr.expressionList.length == 0) {
				return `(${feAsString})`;
			}

			return `(${feAsString} ${expr.expressionList
				.map((x) => this.objectToString_ApostrophesToQuoteKeywords(x))
				.join(' ')})`;
		} else if (isLetRecUsage<ISExpression>(expr)) {
			const fnBindingAsString = ([v, expr2]: [
				IVariable<ISExpression>,
				IExpression<ISExpression>
			]) => `(${v} ${this.objectToString_ApostrophesToQuoteKeywords(expr2)})`;
			const bindingsAsString = expr.bindings.map(fnBindingAsString).join(' ');

			return `(letrec (${bindingsAsString}) ${this.objectToString_ApostrophesToQuoteKeywords(
				expr.expression
			)})`;
		} else if (isCallCCUsage(expr)) {
			return `(call/cc ${this.objectToString_ApostrophesToQuoteKeywords(expr.body)})`;
		} else {
			return `${expr}`;
		}
	}

	private expressionToSExpression(
		expr: IExpression<ISExpression>,
		globalInfo: IGlobalInfo<ISExpression>
	): ISExpression {
		if (typeof globalInfo.tokenizer === 'undefined') {
			throw new Error('expressionToSExpression() : this.tokenizer is undefined.');
		} else if (typeof globalInfo.parser === 'undefined') {
			throw new Error('expressionToSExpression() : this.parser is undefined.');
		}

		let quotedConstStr: string;

		if (isQuotedConstantWithApostrophe(expr)) {
			quotedConstStr = expr.toString();
		} else {
			quotedConstStr = "'" + this.objectToString_ApostrophesToQuoteKeywords(expr);
		}

		let parserResult: unknown;

		try {
			parserResult = globalInfo.parser.parse(globalInfo.tokenizer.tokenize(quotedConstStr));
		} catch (ex) {
			throw new Error(`Error while parsing ${quotedConstStr} : ${ex}`);
		}

		if (!isQuotedConstantWithApostrophe(parserResult)) {
			throw new Error(
				`MacroDefinition.ExpressionToSExpression() : The following did not parse to a quoted constant with apostrophe: ${quotedConstStr}`
			);
		}

		// var quotedConst = (QuotedConstantWithApostrophe)parserResult;

		return parserResult.evaluate(globalInfo);
	}

	private sExpressionListToStringWithoutBracketsForReparse(l: SExpressionList): string {
		const headAsString = this.sExpressionToStringForReparse(l.head);

		if (isNullSExpression(l.tail)) {
			return headAsString;
		}
		// else if (l.Tail is Thunk)
		// {
		// 	return string.Format("{0} {1}", headAsString, this.sExpressionToStringForReparse(l.Tail));
		// }
		else if (isSExpressionList(l.tail)) {
			return `${headAsString} ${this.sExpressionListToStringWithoutBracketsForReparse(
				l.tail
			)}`;
		} else {
			// Tail is a symbol, an integer literal, a string, a closure, etc.
			return `${headAsString} . ${this.sExpressionToStringForReparse(l.tail)}`;
		}
	}

	private sExpressionToStringForReparse(sexpression: ISExpression): string {
		// Convert the first level of quote keywords to apostrophes; e.g.:
		// (quote foo) -> "'foo"
		// (quote (quote foo)) -> "'(quote foo)"
		// ((quote foo) (quote bar)) -> "('foo 'bar)"

		// var qc = sexpression as QuotedConstantWithQuoteKeyword;

		// if (qc != null)
		if (isQuotedConstantWithQuoteKeyword(sexpression)) {
			return "'" + sexpression.sexpression.toString();
		}

		// var l = sexpression as SExpressionList;

		if (!isSExpressionList(sexpression)) {
			return sexpression.toString();
		} else {
			/*
		else if (l.Head.ToString() == "quote")
		{
			var l2 = l.Tail as SExpressionList;

			if (l2 != null && l2.Length == 1)
			{
				return "'" + l2.Head.ToString();
			}
			else
			{
				return "'" + l.Tail.ToString();
			}
		}
			 */
			return `(${this.sExpressionListToStringWithoutBracketsForReparse(sexpression)})`;
		}
	}

	public invokeMacro(
		unevaluatedArguments: IExpression<ISExpression>[],
		localEnvironment: IEnvironmentFrame<ISExpression>,
		globalInfo: IGlobalInfo<ISExpression>
	): ISExpression {
		if (typeof globalInfo.tokenizer === 'undefined') {
			throw new Error('invokeMacro() : this.tokenizer is undefined.');
		} else if (typeof globalInfo.parser === 'undefined') {
			throw new Error('invokeMacro() : this.parser is undefined.');
		}

		const rhoPrime = new EnvironmentFrame<ISExpression>(localEnvironment);

		rhoPrime.compose(
			this.argList,
			unevaluatedArguments.map((expr) => this.expressionToSExpression(expr, globalInfo))
		);

		const substitutedBody = this.body.evaluate(globalInfo, rhoPrime);
		const substitutedBodyAsString = this.sExpressionToStringForReparse(substitutedBody);
		let parserResult: unknown;

		try {
			parserResult = globalInfo.parser.parse(
				globalInfo.tokenizer.tokenize(substitutedBodyAsString)
			);
		} catch (ex) {
			throw new Error(`Error while parsing ${substitutedBodyAsString} : ${ex}`);
		}

		// if (!(parserResult is IExpression<ISExpression>)) {
		// 	throw new Error(
		// 		`MacroDefinition.InvokeMacro() : The following did not parse to an IExpression<ISExpression>: ${substitutedBodyAsString}`;
		// }

		const exprParsed = parserResult as IExpression<ISExpression>;

		return exprParsed.evaluate(globalInfo, localEnvironment);
	}
}
