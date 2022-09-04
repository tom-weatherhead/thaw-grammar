// tom-weatherhead/thaw-grammar/src/languages/lisp/domain-object-model/macro-definition.ts

import { Name } from 'thaw-interpreter-core';

import { isBeginUsage } from '../../../common/domain-object-model/begin-usage';
import {
	EnvironmentFrame,
	IEnvironmentFrame
} from '../../../common/domain-object-model/environment-frame';
import { isFunctionDefinition } from '../../../common/domain-object-model/function-definition';
import { isIfUsage } from '../../../common/domain-object-model/if-usage';
import { IExpression } from '../../../common/domain-object-model/iexpression';
import { IGlobalInfo } from '../../../common/domain-object-model/iglobal-info';
import { IMacroDefinition } from '../../../common/domain-object-model/imacro-definition';
import { isSetUsage } from '../../../common/domain-object-model/set-usage';
import { IVariable } from '../../../common/domain-object-model/variable';
import { isWhileUsage } from '../../../common/domain-object-model/while-usage';
import { ISExpression } from './isexpression';
import { isNullSExpression } from './null-sexpression';
import { isQuotedConstantWithApostrophe } from './quoted-constant-with-apostrophe';
import { isQuotedConstantWithQuoteKeyword } from './quoted-constant-with-quote-keyword';
import { isSExpressionList, SExpressionList } from './sexpression-list';

export class MacroDefinition implements IExpression<ISExpression>, IMacroDefinition<ISExpression> {
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
			// var fd = (FunctionDefinition<ISExpression>)expr;

			return `(define ${expr.functionName} (${expr.argList
				.map((a) => a.name)
				.join(' ')}) ${this.objectToString_ApostrophesToQuoteKeywords(expr.body)})`;
		} else if (isIfUsage(expr)) {
			// var iu = (IfUsage<ISExpression>)expr;

			return `(if ${this.objectToString_ApostrophesToQuoteKeywords(
				expr.condition
			)} ${this.objectToString_ApostrophesToQuoteKeywords(
				expr.ifBody
			)} ${this.objectToString_ApostrophesToQuoteKeywords(expr.elseBody)})`;
		} else if (isWhileUsage(expr)) {
			// var wu = (WhileUsage<ISExpression>)expr;

			return `(while ${this.objectToString_ApostrophesToQuoteKeywords(
				expr.condition
			)} ${this.objectToString_ApostrophesToQuoteKeywords(expr.body)})`;
		} else if (isSetUsage(expr)) {
			// var su = (SetUsage<ISExpression>)expr;

			return `(set ${expr.variableName} ${this.objectToString_ApostrophesToQuoteKeywords(
				expr.expression
			)})`;
		} else if (isBeginUsage(expr)) {
			// var bu = (BeginUsage<ISExpression>)expr;

			return `(begin ${this.objectToString_ApostrophesToQuoteKeywords(
				expr.firstExpression
			)} ${expr.expressionList
				.map((e) => this.objectToString_ApostrophesToQuoteKeywords(e))
				.join(' ')})`;
		}
		// else if (expr is CondUsage<ISExpression>) {
		// 	var cu = (CondUsage<ISExpression>)expr;
		//
		// 	return string.Format("(cond {0})", string.Join(" ", cu.ExprPairList.Select(ep => string.Format("({0} {1})",
		// 		ObjectToString_ApostrophesToQuoteKeywords(ep.Key),
		// 		ObjectToString_ApostrophesToQuoteKeywords(ep.Value)))));
		// } else if (expr is LetUsage<ISExpression>) {
		// 	var lu = (LetUsage<ISExpression>)expr;
		//
		// 	return string.Format("(let ({0}) {1})",
		// 		string.Join(" ", lu.Bindings.Select(b => string.Format("({0} {1})", b.Key, ObjectToString_ApostrophesToQuoteKeywords(b.Value)))),
		// 		ObjectToString_ApostrophesToQuoteKeywords(lu.Expression)); }
		// else if (expr is LetStarUsage<ISExpression>) {
		// 	var lsu = (LetStarUsage<ISExpression>)expr;
		//
		// 	return string.Format("(let* ({0}) {1})",
		// 		string.Join(" ", lsu.Bindings.Select(b => string.Format("({0} {1})", b.Key, ObjectToString_ApostrophesToQuoteKeywords(b.Value)))),
		// 		ObjectToString_ApostrophesToQuoteKeywords(lsu.Expression));
		// } else if (expr is OperatorUsage<ISExpression> && !(expr is Scheme.PrimOp)) {
		// 	var ou = (OperatorUsage<ISExpression>)expr;
		//
		// 	if (ou.ExpressionList.Value.Count == 0) {
		// 		return string.Format("({0})", ou.OperatorName);
		// 	}
		//
		// 	return string.Format("({0} {1})", ou.OperatorName,
		// 		string.Join(" ", ou.ExpressionList.Value.Select(x => ObjectToString_ApostrophesToQuoteKeywords(x))));
		// } else if (expr is QuotedConstantWithApostrophe) {
		// 	var qc = (QuotedConstantWithApostrophe)expr;
		//
		// 	return string.Format("(quote {0})", qc.sexpression);
		// }
		// 	/*
		// else if (expr is SExpressionList) // Not an IExpression<ISExpression>
		// {
		// 	return string.Format("({0})", SExpressionListToString_ApostrophesToQuoteKeywords((SExpressionList)expr));
		// }
		// 	 */
		// else if (expr is MacroDefinition) {
		// 	var md = (MacroDefinition)expr;
		//
		// 	return string.Format("(define-macro {0} {1} {2})", md.MacroName, md.ArgList, ObjectToString_ApostrophesToQuoteKeywords(md.Body));
		// }

		// else if (expr is Scheme.LambdaExpression)
		// {
		// 	var le = (Scheme.LambdaExpression)expr;
		//
		// 	return string.Format("(lambda {0} {1})", le.ArgList, ObjectToString_ApostrophesToQuoteKeywords(le.Body));
		// }
		// else if (expr is Scheme.EvaluableExpression)
		// {
		// 	var ee = (Scheme.EvaluableExpression)expr;
		// 	var feAsString = ObjectToString_ApostrophesToQuoteKeywords(ee.FirstExpression);
		//
		// 	if (ee.ExpressionList.Value.Count == 0)
		// 	{
		// 		return string.Format("({0})", feAsString);
		// 	}
		//
		// 	return string.Format("({0} {1})", feAsString,
		// 		string.Join(" ", ee.ExpressionList.Value.Select(x => ObjectToString_ApostrophesToQuoteKeywords(x))));
		// }
		// else if (expr is Scheme.LetRecUsage)
		// {
		// 	var lru = (Scheme.LetRecUsage)expr;
		//
		// 	return string.Format("(letrec ({0}) {1})",
		// 		string.Join(" ", lru.Bindings.Select(b => string.Format("({0} {1})", b.Key, ObjectToString_ApostrophesToQuoteKeywords(b.Value)))),
		// 		ObjectToString_ApostrophesToQuoteKeywords(lru.Expression));
		// }
		// else if (expr is Scheme.CallCCUsage)
		// {
		// 	var cccu = (Scheme.CallCCUsage)expr;
		//
		// 	return string.Format("(call/cc {0})", ObjectToString_ApostrophesToQuoteKeywords(cccu.Body));
		// }
		else {
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
