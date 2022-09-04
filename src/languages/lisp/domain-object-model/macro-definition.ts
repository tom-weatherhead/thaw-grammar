// tom-weatherhead/thaw-grammar/src/languages/lisp/domain-object-model/macro-definition.ts

import { EnvironmentFrame, IEnvironmentFrame } from '../../../common/domain-object-model/environment-frame';
import { isFunctionDefinition } from '../../../common/domain-object-model/function-definition';
import { isIfUsage } from '../../../common/domain-object-model/if-usage';
import { IExpression } from '../../../common/domain-object-model/iexpression';
import { IGlobalInfo } from '../../../common/domain-object-model/iglobal-info';
import { IMacroDefinition } from '../../../common/domain-object-model/imacro-definition';
import { IVariable } from '../../../common/domain-object-model/variable';
import { isWhileUsage } from '../../../common/domain-object-model/while-usage';
import { ISExpression } from './isexpression';
import { isNullSExpression } from './null-sexpression';
import { isSExpressionList, SExpressionList } from './sexpression-list';

export class MacroDefinition implements IExpression<ISExpression>, IMacroDefinition<ISExpression> {
	// public readonly macroName: string; // Name
	// public readonly argList: IVariable<ISExpression>[];
	// public readonly IExpression<ISExpression> Body;
	public readonly argumentCount: number; // This is a 'get' accessor.

	constructor(public readonly macroName: string, public readonly argList: IVariable<ISExpression>[], public readonly body: IExpression<ISExpression>) {
		// MacroName = macroName;
		// ArgList = argList;
		// Body = body;
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

	// public ISExpression Evaluate(EnvironmentFrame<ISExpression> localEnvironment, IGlobalInfo<ISExpression> globalInfo)
	// {
	// 	globalInfo.MacroDefinitions[MacroName] = this;
	// 	return globalInfo.TrueValue;
	// }

	/* eslint-disable @typescript-eslint/no-unused-vars */
	public evaluate(
		globalInfo: IGlobalInfo<ISExpression>,
		localEnvironment?: IEnvironmentFrame<ISExpression>,
		options?: unknown
	): ISExpression {
		globalInfo.macroDefinitions[this.macroName] = this;
		return globalInfo.trueValue;
	}
	/* eslint-enable @typescript-eslint/no-unused-vars */

	private sExpressionListToString_ApostrophesToQuoteKeywords(l: SExpressionList): string {
		let headAsString = this.objectToString_ApostrophesToQuoteKeywords(l.head);

		if (isNullSExpression(l.tail)) {
			return headAsString;
		// } else if (isThunk(l.tail)) {
		// 	return `${headAsString} ${l.tail}`;
		}
		else if (isSExpressionList(l.tail)) {
			return `${headAsString} ${this.sExpressionListToString_ApostrophesToQuoteKeywords(l.tail)}`;
		} else { // Tail is a symbol, an integer literal, a string, a closure, etc.
			return `${headAsString} . ${this.objectToString_ApostrophesToQuoteKeywords(l.tail)}`;
		}
	}

	public objectToString_ApostrophesToQuoteKeywords(expr: unknown): string {

		if (isFunctionDefinition<ISExpression>(expr)) {
			// var fd = (FunctionDefinition<ISExpression>)expr;

			return `(define ${expr.functionName} (${expr.argList.map(a => a.name).join(' ')}) ${this.objectToString_ApostrophesToQuoteKeywords(expr.body)})`;
		} else if (isIfUsage(expr)) {
			// var iu = (IfUsage<ISExpression>)expr;

			return `(if ${this.objectToString_ApostrophesToQuoteKeywords(expr.condition)} ${this.objectToString_ApostrophesToQuoteKeywords(expr.ifBody)} ${this.objectToString_ApostrophesToQuoteKeywords(expr.elseBody)})`;
		} else if (isWhileUsage(expr)) {
			// var wu = (WhileUsage<ISExpression>)expr;

			return `(while ${this.objectToString_ApostrophesToQuoteKeywords(expr.condition)} ${this.objectToString_ApostrophesToQuoteKeywords(expr.body)})`;
		}

		// else if (expr is SetUsage<ISExpression>) {
		// 	var su = (SetUsage<ISExpression>)expr;
		//
		// 	return string.Format("(set {0} {1})", su.VariableName,
		// 		ObjectToString_ApostrophesToQuoteKeywords(su.Expression));
		// } else if (expr is BeginUsage<ISExpression>) {
		// 	var bu = (BeginUsage<ISExpression>)expr;
		//
		// 	return string.Format("(begin {0} {1})",
		// 		ObjectToString_ApostrophesToQuoteKeywords(bu.FirstExpression),
		// 		string.Join(" ", bu.ExpressionList.Value.Select(x => ObjectToString_ApostrophesToQuoteKeywords(x))));
		// } else if (expr is CondUsage<ISExpression>) {
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
			return expr.toString();
		}
	}

	private expressionToSExpression(expr: IExpression<ISExpression>, globalInfo: IGlobalInfo<ISExpression>): ISExpression {
		let quotedConstStr: string;

		if (expr is QuotedConstantWithApostrophe)
		{
			quotedConstStr = expr.ToString();
		}
		else
		{
			quotedConstStr = "'" + ObjectToString_ApostrophesToQuoteKeywords(expr);
		}

		object parserResult;

		try
		{
			parserResult = globalInfo.Parser.Parse(globalInfo.Tokenizer.Tokenize(quotedConstStr));
		}
		catch (Exception ex)
		{
			throw new Exception(string.Format("Error while parsing {0} : {1}", quotedConstStr, ex.Message));
		}

		if (!(parserResult is QuotedConstantWithApostrophe))
		{
			throw new Exception(string.Format(
				"MacroDefinition.ExpressionToSExpression() : The following did not parse to a quoted constant with apostrophe: {0}",
				quotedConstStr));
		}

		var quotedConst = (QuotedConstantWithApostrophe)parserResult;

		return quotedConst.evaluate(null, globalInfo);
	}

	private sExpressionListToStringWithoutBracketsForReparse(l: SExpressionList): string
	{
		var headAsString = this.sExpressionToStringForReparse(l.Head);

		if (l.Tail is NullSExpression)
		{
			return headAsString;
		}
		else if (l.Tail is Thunk)
		{
			return string.Format("{0} {1}", headAsString, this.sExpressionToStringForReparse(l.Tail));
		}
		else if (l.Tail is SExpressionList)
		{
			var tail = (SExpressionList)l.Tail;

			return string.Format("{0} {1}", headAsString, this.sExpressionListToStringWithoutBracketsForReparse(tail));
		}
		else // Tail is a symbol, an integer literal, a string, a closure, etc.
		{
			return string.Format("{0} . {1}", headAsString, this.sExpressionToStringForReparse(l.Tail));
		}
	}

	private sExpressionToStringForReparse(sexpression: ISExpression): string {
		// Convert the first level of quote keywords to apostrophes; e.g.:
		// (quote foo) -> "'foo"
		// (quote (quote foo)) -> "'(quote foo)"
		// ((quote foo) (quote bar)) -> "('foo 'bar)"

		var qc = sexpression as QuotedConstantWithQuoteKeyword;

		if (qc != null)
		{
			return "'" + qc.sexpression.ToString();
		}

		var l = sexpression as SExpressionList;

		if (l == null)
		{
			return sexpression.ToString();
		}
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
		else
		{
			return `(${this.sExpressionListToStringWithoutBracketsForReparse(l)})`;
		}
	}

	public invokeMacro(
		unevaluatedArguments: IExpression<ISExpression>[],
		localEnvironment: IEnvironmentFrame<ISExpression>,
		globalInfo: IGlobalInfo<ISExpression>
	): ISExpression {
		let rhoPrime = new EnvironmentFrame<ISExpression>(localEnvironment);

		rhoPrime.compose(this.argList.value, unevaluatedArguments.map(expr => this.expressionToSExpression(expr, globalInfo)));

		let substitutedBody = this.body.evaluate(rhoPrime, globalInfo);
		let substitutedBodyAsString = this.sExpressionToStringForReparse(substitutedBody);
		let parserResult: unknown;

		try {
			parserResult = globalInfo.Parser.Parse(globalInfo.tokenizer.tokenize(substitutedBodyAsString));
		} catch (ex: Error) {
			throw new Error(`Error while parsing ${substitutedBodyAsString} : ${ex.Message}`);
		}

		if (!(parserResult is IExpression<ISExpression>)) {
			throw new Error(
				`MacroDefinition.InvokeMacro() : The following did not parse to an IExpression<ISExpression>: ${substitutedBodyAsString}`;
		}

		let exprParsed = (IExpression<ISExpression>)parserResult;

		return exprParsed.evaluate(localEnvironment, globalInfo);
	}
}
