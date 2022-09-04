<<<<<<< HEAD
public class MacroDefinition : IExpression<ISExpression>, IMacroDefinition<ISExpression>
{
	public readonly Name MacroName;
	public readonly VariableList<ISExpression> ArgList;
	public readonly IExpression<ISExpression> Body;

	public MacroDefinition(Name macroName, VariableList<ISExpression> argList, IExpression<ISExpression> body)
	{
		MacroName = macroName;
		ArgList = argList;
		Body = body;
	}

	public override string ToString()
	{
		return string.Format("(define-macro {0} {1} {2})", MacroName, ArgList, Body);
	}

	public int ArgumentCount
	{
		get
		{
			return ArgList.Value.Count;
		}
	}

	public ISExpression Evaluate(EnvironmentFrame<ISExpression> localEnvironment, IGlobalInfo<ISExpression> globalInfo)
	{
		globalInfo.MacroDefinitions[MacroName] = this;
		return globalInfo.TrueValue;
	}

	private static string SExpressionListToString_ApostrophesToQuoteKeywords(SExpressionList l)
	{
		var headAsString = ObjectToString_ApostrophesToQuoteKeywords(l.Head);

		if (l.Tail is NullSExpression)
		{
			return headAsString;
		}
		else if (l.Tail is Thunk)
		{
			return string.Format("{0} {1}", headAsString, l.Tail);
		}
		else if (l.Tail is SExpressionList)
		{
			var tail = l.Tail as SExpressionList;

			return string.Format("{0} {1}", headAsString, SExpressionListToString_ApostrophesToQuoteKeywords(tail));
		}
		else // Tail is a symbol, an integer literal, a string, a closure, etc.
		{
			return string.Format("{0} . {1}", headAsString, ObjectToString_ApostrophesToQuoteKeywords(l.Tail));
		}
	}

	public static string ObjectToString_ApostrophesToQuoteKeywords(object expr)
	{

		if (expr is FunctionDefinition<ISExpression>)
		{
			var fd = (FunctionDefinition<ISExpression>)expr;

			return string.Format("(define {0} {1} {2})", fd.FunctionName, fd.ArgList, ObjectToString_ApostrophesToQuoteKeywords(fd.Body));
		}
		else if (expr is IfUsage<ISExpression>)
		{
			var iu = (IfUsage<ISExpression>)expr;

			return string.Format("(if {0} {1} {2})",
				ObjectToString_ApostrophesToQuoteKeywords(iu.Condition),
				ObjectToString_ApostrophesToQuoteKeywords(iu.IfBody),
				ObjectToString_ApostrophesToQuoteKeywords(iu.ElseBody));
		}
		else if (expr is WhileUsage<ISExpression>)
		{
			var wu = (WhileUsage<ISExpression>)expr;

			return string.Format("(while {0} {1})",
				ObjectToString_ApostrophesToQuoteKeywords(wu.Condition),
				ObjectToString_ApostrophesToQuoteKeywords(wu.Body));
		}
		else if (expr is SetUsage<ISExpression>)
		{
			var su = (SetUsage<ISExpression>)expr;

			return string.Format("(set {0} {1})", su.VariableName,
				ObjectToString_ApostrophesToQuoteKeywords(su.Expression));
		}
		else if (expr is BeginUsage<ISExpression>)
		{
			var bu = (BeginUsage<ISExpression>)expr;

			return string.Format("(begin {0} {1})",
				ObjectToString_ApostrophesToQuoteKeywords(bu.FirstExpression),
				string.Join(" ", bu.ExpressionList.Value.Select(x => ObjectToString_ApostrophesToQuoteKeywords(x))));
		}
		else if (expr is CondUsage<ISExpression>)
		{
			var cu = (CondUsage<ISExpression>)expr;

			return string.Format("(cond {0})", string.Join(" ", cu.ExprPairList.Select(ep => string.Format("({0} {1})",
				ObjectToString_ApostrophesToQuoteKeywords(ep.Key),
				ObjectToString_ApostrophesToQuoteKeywords(ep.Value)))));
		}
		else if (expr is LetUsage<ISExpression>)
		{
			var lu = (LetUsage<ISExpression>)expr;

			return string.Format("(let ({0}) {1})",
				string.Join(" ", lu.Bindings.Select(b => string.Format("({0} {1})", b.Key, ObjectToString_ApostrophesToQuoteKeywords(b.Value)))),
				ObjectToString_ApostrophesToQuoteKeywords(lu.Expression));
		}
		else if (expr is LetStarUsage<ISExpression>)
		{
			var lsu = (LetStarUsage<ISExpression>)expr;

			return string.Format("(let* ({0}) {1})",
				string.Join(" ", lsu.Bindings.Select(b => string.Format("({0} {1})", b.Key, ObjectToString_ApostrophesToQuoteKeywords(b.Value)))),
				ObjectToString_ApostrophesToQuoteKeywords(lsu.Expression));
		}
		else if (expr is OperatorUsage<ISExpression> && !(expr is Scheme.PrimOp))
		{
			var ou = (OperatorUsage<ISExpression>)expr;

			if (ou.ExpressionList.Value.Count == 0)
			{
				return string.Format("({0})", ou.OperatorName);
			}

			return string.Format("({0} {1})", ou.OperatorName,
				string.Join(" ", ou.ExpressionList.Value.Select(x => ObjectToString_ApostrophesToQuoteKeywords(x))));
		}
		else if (expr is QuotedConstantWithApostrophe)
		{
			var qc = (QuotedConstantWithApostrophe)expr;

			return string.Format("(quote {0})", qc.sexpression);
		}
			/*
		else if (expr is SExpressionList) // Not an IExpression<ISExpression>
		{
			return string.Format("({0})", SExpressionListToString_ApostrophesToQuoteKeywords((SExpressionList)expr));
		}
			 */
		else if (expr is MacroDefinition)
		{
			var md = (MacroDefinition)expr;

			return string.Format("(define-macro {0} {1} {2})", md.MacroName, md.ArgList, ObjectToString_ApostrophesToQuoteKeywords(md.Body));
		}
		else if (expr is Scheme.LambdaExpression)
		{
			var le = (Scheme.LambdaExpression)expr;

			return string.Format("(lambda {0} {1})", le.ArgList, ObjectToString_ApostrophesToQuoteKeywords(le.Body));
		}
		else if (expr is Scheme.EvaluableExpression)
		{
			var ee = (Scheme.EvaluableExpression)expr;
			var feAsString = ObjectToString_ApostrophesToQuoteKeywords(ee.FirstExpression);

			if (ee.ExpressionList.Value.Count == 0)
			{
				return string.Format("({0})", feAsString);
			}

			return string.Format("({0} {1})", feAsString,
				string.Join(" ", ee.ExpressionList.Value.Select(x => ObjectToString_ApostrophesToQuoteKeywords(x))));
		}
		else if (expr is Scheme.LetRecUsage)
		{
			var lru = (Scheme.LetRecUsage)expr;

			return string.Format("(letrec ({0}) {1})",
				string.Join(" ", lru.Bindings.Select(b => string.Format("({0} {1})", b.Key, ObjectToString_ApostrophesToQuoteKeywords(b.Value)))),
				ObjectToString_ApostrophesToQuoteKeywords(lru.Expression));
		}
		else if (expr is Scheme.CallCCUsage)
		{
			var cccu = (Scheme.CallCCUsage)expr;

			return string.Format("(call/cc {0})", ObjectToString_ApostrophesToQuoteKeywords(cccu.Body));
		}
		else
		{
			return expr.ToString();
		}
	}

	private static ISExpression ExpressionToSExpression(IExpression<ISExpression> expr, IGlobalInfo<ISExpression> globalInfo)
	{
		string quotedConstStr;

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

		return quotedConst.Evaluate(null, globalInfo);
	}

	private static string SExpressionListToStringWithoutBracketsForReparse(SExpressionList l)
	{
		var headAsString = SExpressionToStringForReparse(l.Head);

		if (l.Tail is NullSExpression)
		{
			return headAsString;
		}
		else if (l.Tail is Thunk)
		{
			return string.Format("{0} {1}", headAsString, SExpressionToStringForReparse(l.Tail));
		}
		else if (l.Tail is SExpressionList)
		{
			var tail = (SExpressionList)l.Tail;

			return string.Format("{0} {1}", headAsString, SExpressionListToStringWithoutBracketsForReparse(tail));
		}
		else // Tail is a symbol, an integer literal, a string, a closure, etc.
		{
			return string.Format("{0} . {1}", headAsString, SExpressionToStringForReparse(l.Tail));
		}
	}

	public static string SExpressionToStringForReparse(ISExpression sexpression)
	{
=======
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
		} else if (isIfUsage<ISExpression>(expr)) {
			// var iu = (IfUsage<ISExpression>)expr;

			return `(if ${this.objectToString_ApostrophesToQuoteKeywords(
				expr.condition
			)} ${this.objectToString_ApostrophesToQuoteKeywords(
				expr.ifBody
			)} ${this.objectToString_ApostrophesToQuoteKeywords(expr.elseBody)})`;
		} else if (isWhileUsage<ISExpression>(expr)) {
			// var wu = (WhileUsage<ISExpression>)expr;

			return `(while ${this.objectToString_ApostrophesToQuoteKeywords(
				expr.condition
			)} ${this.objectToString_ApostrophesToQuoteKeywords(expr.body)})`;
		} else if (isSetUsage<ISExpression>(expr)) {
			// var su = (SetUsage<ISExpression>)expr;

			return `(set ${expr.variableName} ${this.objectToString_ApostrophesToQuoteKeywords(
				expr.expression
			)})`;
		} else if (isBeginUsage<ISExpression>(expr)) {
			// var bu = (BeginUsage<ISExpression>)expr;

			return `(begin ${this.objectToString_ApostrophesToQuoteKeywords(
				expr.firstExpression
			)} ${expr.expressionList
				.map((e) => this.objectToString_ApostrophesToQuoteKeywords(e))
				.join(' ')})`;
		} else if (isCondUsage<ISExpression>(expr)) {
			// var cu = (CondUsage<ISExpression>)expr;
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
			// var lu = (LetUsage<ISExpression>)expr;
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
			// var lsu = (LetStarUsage<ISExpression>)expr;

			const bindingsString = expr.bindings
				.map(
					([v, e]: [IVariable<ISExpression>, IExpression<ISExpression>]) =>
						`(${v} ${this.objectToString_ApostrophesToQuoteKeywords(e)})`
				)
				.join(' ');

			return `(let* (${bindingsString}) ${this.objectToString_ApostrophesToQuoteKeywords(
				expr.expression
			)})`;
		}
		// else if (expr is OperatorUsage<ISExpression> && !(expr is Scheme.PrimOp)) {
		else if (isOperatorUsage<ISExpression>(expr) /* && !(expr is Scheme.PrimOp) */) {
			// var ou = (OperatorUsage<ISExpression>)expr;

			if (expr.expressionList.length === 0) {
				return `(${expr.operatorName})`;
			}

			const exprListString = expr.expressionList
				.map((e) => this.objectToString_ApostrophesToQuoteKeywords(e))
				.join(' ');

			return `(${expr.operatorName} ${exprListString})`;
		}

		// else if (expr is QuotedConstantWithApostrophe) {
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
>>>>>>> master
		// Convert the first level of quote keywords to apostrophes; e.g.:
		// (quote foo) -> "'foo"
		// (quote (quote foo)) -> "'(quote foo)"
		// ((quote foo) (quote bar)) -> "('foo 'bar)"

<<<<<<< HEAD
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
=======
		// var qc = sexpression as QuotedConstantWithQuoteKeyword;

		// if (qc != null)
		if (isQuotedConstantWithQuoteKeyword(sexpression)) {
			return "'" + sexpression.sexpression.toString();
		}

		// var l = sexpression as SExpressionList;

		if (!isSExpressionList(sexpression)) {
			return sexpression.toString();
		} else {
>>>>>>> master
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
<<<<<<< HEAD
		else
		{
			return string.Format("({0})", SExpressionListToStringWithoutBracketsForReparse(l));
		}
	}

	public ISExpression InvokeMacro(
		List<IExpression<ISExpression>> unevaluatedArguments,
		EnvironmentFrame<ISExpression> localEnvironment,
		IGlobalInfo<ISExpression> globalInfo)
	{
		var rhoPrime = new EnvironmentFrame<ISExpression>(localEnvironment);

		rhoPrime.Compose(ArgList.Value, unevaluatedArguments.Select(expr => ExpressionToSExpression(expr, globalInfo)).ToList());

		var substitutedBody = Body.Evaluate(rhoPrime, globalInfo);
		var substitutedBodyAsString = SExpressionToStringForReparse(substitutedBody);
		object parserResult;

		try
		{
			parserResult = globalInfo.Parser.Parse(globalInfo.Tokenizer.Tokenize(substitutedBodyAsString));
		}
		catch (Exception ex)
		{
			throw new Exception(string.Format("Error while parsing {0} : {1}", substitutedBodyAsString, ex.Message));
		}

		if (!(parserResult is IExpression<ISExpression>))
		{
			throw new Exception(string.Format(
				"MacroDefinition.InvokeMacro() : The following did not parse to an IExpression<ISExpression>: {0}",
				substitutedBodyAsString));
		}

		var exprParsed = (IExpression<ISExpression>)parserResult;

		return exprParsed.Evaluate(localEnvironment, globalInfo);
=======
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
>>>>>>> master
	}
}
