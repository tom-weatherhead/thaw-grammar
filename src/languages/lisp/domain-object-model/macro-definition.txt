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
	}
}
