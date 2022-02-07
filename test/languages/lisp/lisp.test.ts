// tom-weatherhead/thaw-grammar/test/lisp.test.ts

'use strict';

import { LanguageSelector } from 'thaw-interpreter-types';

import { createFnRecognizer, createInfrastructure } from '../../create-infrastructure';

import { createGrammar, IExpression, ISExpression, LISPGlobalInfo } from '../../..';

import { createParser, SyntaxException } from 'thaw-parser';

const ls = LanguageSelector.LISP;

test('LL(1) LISP parser instance creation test', () => {
	// Arrange
	const grammar = createGrammar(ls);

	// Act
	const parser = createParser(grammar.defaultParser, grammar);

	// Assert
	expect(parser).toBeTruthy();
});

function evaluateToISExpression(input: string): ISExpression {
	const { tokenizer, parser } = createInfrastructure(ls);
	const globalInfo = new LISPGlobalInfo({ tokenizer, parser });

	return globalInfo.evaluate(input);
}

function lispTest(
	data: Array<[input: string, expectedResult: string | string[]]>,
	options: {
		presets?: string[];
	} = {}
): void {
	// Arrange
	const { tokenizer, parser } = createInfrastructure(ls);

	const globalInfo = new LISPGlobalInfo({ tokenizer, parser });

	globalInfo.loadPresets();

	if (typeof options.presets !== 'undefined') {
		for (const preset of options.presets) {
			globalInfo.loadPreset(preset);
		}
	}

	for (const [input, expectedResult] of data) {
		// Act
		const parseResult = parser.parse(tokenizer.tokenize(input));
		const expr = parseResult as IExpression<ISExpression>;
		const actualResult = expr.evaluate(globalInfo, globalInfo.globalEnvironment).toString();

		// console.log(`input: ${input}\nactualResult:\n${actualResult}\n\n`);

		// Assert
		if (typeof expectedResult === 'string') {
			expect(actualResult).toBe(expectedResult);
		} else {
			for (const str of expectedResult) {
				expect(actualResult.includes(str)).toBe(true);
			}
		}
	}
}

// [SetUp]
// public void SetUp()
// {
// 	globalInfo.DynamicScoping = false;
// 	globalInfo.Clear();
// 	globalInfo.LoadPresets();
// }

test('LL(1) LISP recognize test', () => {
	// Arrange
	const f = createFnRecognizer(ls);

	f('(* 7 13)');

	expect(() => f('(* 7 13')).toThrow(SyntaxException);

	f('(define double (x) (+ x x))');
	f('(double 5)');

	f('(define +1 (n) (+ n 1))');
	f('(+1 5)');

	f('(define not (boolval) (if boolval 0 1))');
	f('(define <> (x y) (not (= x y)))');

	f('(define mod (m n) (- m (* n (/ m n))))');
	f('(define gcd (m n) (if (= n 0) m (gcd n (mod m n))))');

	f('13');
	f("'13");
	f("'T");
	f("'()");
	f("'(1 2 3)");

	f('"ABC"');
});

// [Test]
// public void ParseIntegerLiteralTest()
// {
// 	var value = 13;
// 	var expr = parser.Parse(tokenizer.Tokenize(value.ToString())) as IntegerLiteral;
//
// 	Assert.IsNotNull(expr);
// 	Assert.AreEqual(value, expr.Value);
// }

// [Test]
// public void ParseStringLiteralTest()
// {
// 	const string expectedValue = "ABC";
// 	var inputString = string.Format("\"{0}\"", expectedValue);
// 	var expr = parser.Parse(tokenizer.Tokenize(inputString)) as LISPString;
//
// 	Assert.IsNotNull(expr);
// 	Assert.AreEqual(expectedValue, expr.Value);
// }

// [Test]
// public void ParseQuotedIntegerLiteralTest()
// {
// 	var value = 13;
// 	var parseResult = parser.Parse(tokenizer.Tokenize("'" + value.ToString()));
//
// 	Assert.IsNotNull(parseResult);
// 	Assert.AreEqual("'" + value.ToString(), parseResult.ToString());
//
// 	var expr = parseResult as IExpression<ISExpression>;
//
// 	Assert.IsNotNull(expr);
//
// 	var sexpr = expr.Evaluate(globalInfo.GlobalEnvironment, globalInfo);
//
// 	Assert.IsNotNull(sexpr);
// 	Assert.IsTrue(sexpr.IsNumber());
// 	Assert.IsTrue(sexpr is IntegerLiteral);
//
// 	var intlit = sexpr as IntegerLiteral;
//
// 	Assert.AreEqual(value, intlit.Value);
// }

// [Test]
// public void ParseQuotedSymbolTest()
// {
// 	var value = "T";
// 	var parseResult = parser.Parse(tokenizer.Tokenize("'" + value));
//
// 	Assert.IsNotNull(parseResult);
// 	Assert.AreEqual("'" + value, parseResult.ToString());
//
// 	var expr = parseResult as IExpression<ISExpression>;
//
// 	Assert.IsNotNull(expr);
//
// 	var sexpr = expr.Evaluate(globalInfo.GlobalEnvironment, globalInfo);
//
// 	Assert.IsNotNull(sexpr);
// 	Assert.IsTrue(sexpr.IsSymbol());
// 	Assert.IsTrue(sexpr is LISPSymbol);
//
// 	var symbol = sexpr as LISPSymbol;
//
// 	Assert.AreEqual(value, symbol.Value);
// }

// [Test]
// public void ParseQuotedEmptyListTest()
// {
// 	var value = "'()";
// 	var parseResult = parser.Parse(tokenizer.Tokenize(value));
//
// 	Assert.IsNotNull(parseResult);
// 	Assert.AreEqual(value, parseResult.ToString());
//
// 	var expr = parseResult as IExpression<ISExpression>;
//
// 	Assert.IsNotNull(expr);
//
// 	var sexpr = expr.Evaluate(globalInfo.GlobalEnvironment, globalInfo);
//
// 	Assert.IsNotNull(sexpr);
// 	Assert.IsTrue(sexpr.IsNull());
// }

// [Test]
// public void ParseQuotedNonEmptyListTest1()
// {
// 	var value = "'(1 2 3)";
// 	var parseResult = parser.Parse(tokenizer.Tokenize(value));
//
// 	Assert.IsNotNull(parseResult);
// 	Assert.AreEqual(value, parseResult.ToString());
//
// 	var expr = parseResult as IExpression<ISExpression>;
//
// 	Assert.IsNotNull(expr);
//
// 	var sexpr = expr.Evaluate(globalInfo.GlobalEnvironment, globalInfo);
//
// 	Assert.IsNotNull(sexpr);
// 	Assert.IsTrue(sexpr.IsList());
// 	Assert.IsFalse(sexpr.IsNull());
// }

// [Test]
// public void ParseQuotedNonEmptyListTest2()
// {
// 	var value = "'((1 2) 3 ((4 5 6) 7 8))";
// 	var parseResult = parser.Parse(tokenizer.Tokenize(value));
//
// 	Assert.IsNotNull(parseResult);
// 	Assert.AreEqual(value, parseResult.ToString());
//
// 	var expr = parseResult as IExpression<ISExpression>;
//
// 	Assert.IsNotNull(expr);
//
// 	var sexpr = expr.Evaluate(globalInfo.GlobalEnvironment, globalInfo);
//
// 	Assert.IsNotNull(sexpr);
// 	Assert.IsTrue(sexpr.IsList());
// 	Assert.IsFalse(sexpr.IsNull());
// }

// [Test]
// public void AdditionTest()
// {
// 	var value = "(+ 2 3)";
// 	var tokenList = tokenizer.Tokenize(value);
//
// 	Assert.AreEqual(6, tokenList.Count);
// 	Assert.AreEqual(TokenType.T_LeftBracket, tokenList[0].TokenType);
// 	Assert.AreEqual(TokenType.T_Ident, tokenList[1].TokenType);
// 	Assert.IsTrue(tokenList[1].TokenValue is string);
// 	Assert.AreEqual("+", tokenList[1].TokenValue as string);
// 	Assert.AreEqual(TokenType.T_IntLit, tokenList[2].TokenType);
// 	Assert.IsTrue(tokenList[2].TokenValue is int);
// 	Assert.AreEqual(2, (int)tokenList[2].TokenValue);
// 	Assert.AreEqual(TokenType.T_IntLit, tokenList[3].TokenType);
// 	Assert.IsTrue(tokenList[3].TokenValue is int);
// 	Assert.AreEqual(3, (int)tokenList[3].TokenValue);
// 	Assert.AreEqual(TokenType.T_RightBracket, tokenList[4].TokenType);
// 	Assert.AreEqual(TokenType.T_EOF, tokenList[5].TokenType);
//
// 	var parseResult = parser.Parse(tokenizer.Tokenize(value));
//
// 	Assert.IsNotNull(parseResult);
// 	Assert.IsTrue(parseResult is OperatorUsage<ISExpression>);
//
// 	var ou = parseResult as OperatorUsage<ISExpression>;
//
// 	Assert.IsNotNull(ou.OperatorName);
// 	Assert.IsNotNull(ou.OperatorName.Value);
// 	Assert.AreEqual("+", ou.OperatorName.Value);
// 	Assert.AreEqual(2, ou.ExpressionList.Value.Count);
// 	Assert.AreEqual(value, parseResult.ToString());
//
// 	var expr = parseResult as IExpression<ISExpression>;
//
// 	Assert.IsNotNull(expr);
//
// 	var sexpr = expr.Evaluate(globalInfo.GlobalEnvironment, globalInfo);
//
// 	Assert.IsNotNull(sexpr);
// 	Assert.IsTrue(sexpr.IsNumber());
// 	Assert.IsTrue(sexpr is IntegerLiteral);
//
// 	var intlit = sexpr as IntegerLiteral;
//
// 	Assert.AreEqual(5, intlit.Value);
// 	Assert.AreEqual("5", sexpr.ToString());
// }

test('LL(1) LISP addition test 2', () => {
	lispTest([['(+ 2 3)', '5']]);
});

test('LISP multi-operator test', () => {
	lispTest([['(- (* 8 (+ 12 1)) (/ (+ 16 4) 5))', '100']]);
});

test('LISP inequality test', () => {
	lispTest([
		['(< 1 2)', 'T'],
		['(< 2 2)', '()'],
		['(< 3 2)', '()'],

		['(> 1 2)', '()'],
		['(> 2 2)', '()'],
		['(> 3 2)', 'T']
	]);
});

// [Test]
// public void VariableTest1()
// {
// 	var input = "n";
// 	var parseResult = GetParseResult(input);
//
// 	Assert.IsTrue(parseResult is Variable<ISExpression>);
//
// 	var variable = parseResult as Variable<ISExpression>;
//
// 	Assert.AreEqual(input, variable.Name);
// }

// [Test]
// public void VariableTest2()
// {
// 	var input = "(+ n 1)";
// 	var parseResult = GetParseResult(input);
//
// 	Assert.IsTrue(parseResult is OperatorUsage<ISExpression>);
//
// 	var opUsage = parseResult as OperatorUsage<ISExpression>;
//
// 	Assert.AreEqual("+", opUsage.OperatorName.Value);
// 	Assert.AreEqual(2, opUsage.ExpressionList.Value.Count);
//
// 	var expr1 = opUsage.ExpressionList.Value[0];
// 	var expr2 = opUsage.ExpressionList.Value[1];
//
// 	Assert.IsTrue(expr1 is Variable<ISExpression>);
// 	Assert.IsTrue(expr2 is IntegerLiteral);
//
// 	var variable = expr1 as Variable<ISExpression>;
// 	var intLit = expr2 as IntegerLiteral;
//
// 	Assert.AreEqual("n", variable.Name);
// 	Assert.AreEqual(1, intLit.Value);
// }

test('LL(1) LISP if test', () => {
	lispTest([
		["(if (> 2 1) 'First 'Second)", 'First'],
		["(if (< 2 1) 'First 'Second)", 'Second']
	]);
});

// [Test]
// public void EqualsTest()
// {
// 	Assert.AreEqual("T", Evaluate("(= 0 0)"));
// 	Assert.AreEqual("T", Evaluate("(= 1 1)"));
// 	Assert.AreEqual("()", Evaluate("(= 0 1)"));
// 	Assert.AreEqual("()", Evaluate("(= 1 0)"));
// 	Assert.AreEqual("T", Evaluate("(= (+ 2 3) 5)"));
//
// 	Assert.AreEqual("()", Evaluate("(= 0 '())"));
// 	Assert.AreEqual("()", Evaluate("(= 0 'T)"));
// 	Assert.AreEqual("()", Evaluate("(= 0 '(1 2 3))"));
//
// 	Assert.AreEqual("()", Evaluate("(= 'T 0)"));
// 	Assert.AreEqual("T", Evaluate("(= 'T 'T)"));
// 	Assert.AreEqual("()", Evaluate("(= 'T 'F)"));
// 	Assert.AreEqual("()", Evaluate("(= 'T '(1 2 3))"));
// 	Assert.AreEqual("()", Evaluate("(= 'T '())"));
//
// 	Assert.AreEqual("()", Evaluate("(= '(1 2 3) 0)"));
// 	Assert.AreEqual("()", Evaluate("(= '(1 2 3) 'T)"));
// 	Assert.AreEqual("()", Evaluate("(= '(1 2 3) '(1 2))"));
// 	Assert.AreEqual("T", Evaluate("(= '(1 2 3) '(1 2 3))"));
// 	Assert.AreEqual("()", Evaluate("(= '(1 2 3) '(1 2 3 4))"));
// 	Assert.AreEqual("()", Evaluate("(= '(1 2 3) '(5 8 13))"));
// 	Assert.AreEqual("()", Evaluate("(= '(1 2 3) '())"));
//
// 	Assert.AreEqual("()", Evaluate("(= '() 0)"));
// 	Assert.AreEqual("()", Evaluate("(= '() 'T)"));
// 	Assert.AreEqual("()", Evaluate("(= '() '(1 2 3))"));
// 	Assert.AreEqual("T", Evaluate("(= '() '())"));
// }

// [Test]
// public void WhileTest()
// {
// 	const string whileTest = @"
// (begin
// (set x 1)
// (set acc 0)
// (while (< x 5) (begin
// (set acc (+ acc x))
// (set x (+ x 1))
// ))
// acc
// )";
//
// 	Assert.AreEqual("10", Evaluate(whileTest));
// }

// [Test]
// public void SetTest()
// {
// 	Assert.AreEqual("2", Evaluate("(set globalN 2)"));
// 	Assert.AreEqual("2", Evaluate("globalN"));
// 	Assert.AreEqual("3", Evaluate("(+ globalN 1)"));
// }

// [Test]
// public void BeginTest()
// {
// 	Assert.AreEqual("13", Evaluate("(begin 1 1 2 3 5 8 13)"));
// }

// [Test]
// public void FuncDefTest()
// {
// 	Assert.AreEqual("T", Evaluate("(define increment (n) (+ n 1))"));
// 	Assert.AreEqual("14", Evaluate("(increment 13)"));
// }

// [Test]
// public void GCDFunctionTest()
// {
// 	Assert.AreEqual("3", Evaluate("(mod 101 7)"));
//
// 	Assert.AreEqual("7", Evaluate("(gcd 343 91)"));
// 	Assert.AreEqual("1", Evaluate("(gcd 31 29)"));
// }

// [Test]
// public void NumberPredicateTest()
// {
// 	Assert.AreEqual("T", Evaluate("(number? 7)"));          // Number
// 	Assert.AreEqual("T", Evaluate("(number? '7)"));         // Quoted number
// 	Assert.AreEqual("()", Evaluate("(number? 'T)"));        // Symbol
// 	Assert.AreEqual("()", Evaluate("(number? '(1 2 3))"));  // List 1
// 	Assert.AreEqual("()", Evaluate("(number? '(()))"));     // List 2
// 	Assert.AreEqual("()", Evaluate("(number? '())"));       // Null
// 	Assert.AreEqual("()", Evaluate("(number? \"ABC\")"));   // String
// 	Assert.AreEqual("()", Evaluate("(number? '\"ABC\")"));  // Quoted string
// }

// [Test]
// public void SymbolPredicateTest()
// {
// 	Assert.AreEqual("()", Evaluate("(symbol? 7)"));         // Number
// 	Assert.AreEqual("()", Evaluate("(symbol? '7)"));        // Quoted number
// 	Assert.AreEqual("T", Evaluate("(symbol? 'T)"));         // Symbol
// 	Assert.AreEqual("()", Evaluate("(symbol? '(1 2 3))"));  // List 1
// 	Assert.AreEqual("()", Evaluate("(symbol? '(()))"));     // List 2
// 	Assert.AreEqual("()", Evaluate("(symbol? '())"));       // Null
// 	Assert.AreEqual("()", Evaluate("(symbol? \"ABC\")"));   // String
// 	Assert.AreEqual("()", Evaluate("(symbol? '\"ABC\")"));  // Quoted string
// }

// [Test]
// public void ListPredicateTest()
// {
// 	Assert.AreEqual("()", Evaluate("(list? 7)"));           // Number
// 	Assert.AreEqual("()", Evaluate("(list? '7)"));          // Quoted number
// 	Assert.AreEqual("()", Evaluate("(list? 'T)"));          // Symbol
// 	Assert.AreEqual("T", Evaluate("(list? '(1 2 3))"));     // List 1
// 	Assert.AreEqual("T", Evaluate("(list? '(()))"));        // List 2
// 	Assert.AreEqual("()", Evaluate("(list? '())"));         // Null
// 	Assert.AreEqual("()", Evaluate("(list? \"ABC\")"));     // String
// 	Assert.AreEqual("()", Evaluate("(list? '\"ABC\")"));    // Quoted string
// }

// [Test]
// public void NullPredicateTest()
// {
// 	Assert.AreEqual("()", Evaluate("(null? 7)"));           // Number
// 	Assert.AreEqual("()", Evaluate("(null? '7)"));          // Quoted number
// 	Assert.AreEqual("()", Evaluate("(null? 'T)"));          // Symbol
// 	Assert.AreEqual("()", Evaluate("(null? '(1 2 3))"));    // List 1
// 	Assert.AreEqual("()", Evaluate("(null? '(()))"));       // List 2
// 	Assert.AreEqual("T", Evaluate("(null? '())"));          // Null
// 	Assert.AreEqual("()", Evaluate("(null? \"ABC\")"));     // String
// 	Assert.AreEqual("()", Evaluate("(null? '\"ABC\")"));    // Quoted string
// }

// [Test]
// public void StringPredicateTest()
// {
// 	Assert.AreEqual("()", Evaluate("(string? 7)"));         // Number
// 	Assert.AreEqual("()", Evaluate("(string? '7)"));        // Quoted number
// 	Assert.AreEqual("()", Evaluate("(string? 'T)"));        // Symbol
// 	Assert.AreEqual("()", Evaluate("(string? '(1 2 3))"));  // List 1
// 	Assert.AreEqual("()", Evaluate("(string? '(()))"));     // List 2
// 	Assert.AreEqual("()", Evaluate("(string? '())"));       // Null
// 	Assert.AreEqual("T", Evaluate("(string? \"ABC\")"));    // String
// 	Assert.AreEqual("T", Evaluate("(string? '\"ABC\")"));   // Quoted string
// }

// [Test]
// public void ConsTest()
// {
// 	Assert.AreEqual("(1)", Evaluate("(cons 1 '())"));
// 	Assert.AreEqual("(1 2 3)", Evaluate("(cons 1 '(2 3))"));
// }

// [Test]
// public void CarTest()
// {
// 	Assert.AreEqual("1", Evaluate("(car '(1))"));
// 	Assert.AreEqual("1", Evaluate("(car '(1 2 3))"));
// 	Assert.AreEqual("(1 2)", Evaluate("(car '((1 2) (3 4)))"));
// }

// [Test]
// public void CdrTest()
// {
// 	Assert.AreEqual("()", Evaluate("(cdr '(1))"));
// 	Assert.AreEqual("(2 3)", Evaluate("(cdr '(1 2 3))"));
// 	Assert.AreEqual("((3 4))", Evaluate("(cdr '((1 2) (3 4)))"));
// }

// [Test]
// public void LengthFunctionTest()
// {
// 	Assert.AreEqual("0", Evaluate("(length '())"));
// 	Assert.AreEqual("6", Evaluate("(length '(1 1 2 3 5 8))"));
// }

// [Test]
// public void DotTest()
// {
// 	Assert.AreEqual("(1 2 . 3)", Evaluate("(cons 1 (cons 2 3))"));
// 	Assert.AreEqual("(1 2 . 3)", Evaluate("(set x '(1 2 . 3))"));
// 	Assert.AreEqual("(1 2 3 4)", Evaluate("(set x '(1 2 . (3 4)))"));
// }

// [Test]
// public void GlobalVsLocalVariableTest()
// {
// 	Evaluate("(set a 1)");
// 	Evaluate("(define afunc () a)");
// 	Evaluate("(define func2 (a) (afunc))");
//
// 	Assert.AreEqual("1", Evaluate("(func2 0)"));
// }

test('LL(1) LISP cond test', () => {
	lispTest([
		[
			"(define condtest (n) (cond ((= n 1) 'First) ((= n 2) 'Second) ((= n 3) 'Third) ('T 'Other)))",
			'T'
		],
		['(condtest 0)', 'Other'],
		['(condtest 1)', 'First'],
		['(condtest 2)', 'Second'],
		['(condtest 3)', 'Third'],
		['(condtest 4)', 'Other']
	]);
});

// [Test]
// public void LetTest()
// {
// 	Assert.AreEqual("5", Evaluate("(let ((n (+ 2 3))) n)"));
// }
test('LL(1) LISP let test', () => {
	lispTest([['(let ((n (+ 2 3))) n)', '5']]);
});

// [Test]
// public void LetStarTest()
// {
// 	Assert.AreEqual("25", Evaluate("(let* ((x (+ 2 3)) (y (* x x))) y)"));
// }

// [Test]
// public void ListTest()
// {
// 	Assert.AreEqual("()", Evaluate("(list)"));
// 	Assert.AreEqual("(1)", Evaluate("(list 1)"));
// 	Assert.AreEqual("(1 2 3)", Evaluate("(list 1 2 3)"));
// }

// [Test]
// public void SieveOfEratosthenesTest()   // See page 31
// {
// 	Evaluate("(define divides (m n) (= (mod n m) 0))");
// 	Evaluate("(define interval-list (m n) (if (> m n) '() (cons m (interval-list (+1 m) n))))");
//
// 	Assert.AreEqual("(3 4 5 6 7)", Evaluate("(interval-list 3 7)"));
//
// 	Evaluate(@"
// (define remove-multiples (n l)
// (if (null? l) '()
// (if (divides n (car l))
// 	(remove-multiples n (cdr l))
// 	(cons (car l) (remove-multiples n (cdr l))))))");
//
// 	Assert.AreEqual("(3 5 7)", Evaluate("(remove-multiples 2 '(2 3 4 5 6 7))"));
//
// 	Evaluate(@"
// (define sieve (l)
// (if (null? l) '()
// (cons (car l)
// 	(sieve (remove-multiples (car l) (cdr l))))))");
// 	Evaluate("(define primes<= (n) (sieve (interval-list 2 n)))");
//
// 	Assert.AreEqual("(2 3 5 7)", Evaluate("(primes<= 10)"));
// 	Assert.AreEqual("(2 3 5 7 11 13 17 19 23 29)", Evaluate("(primes<= 30)"));
// }

// [Test]
// public void PropertyListTest()   // See page 33
// {
// 	globalInfo.LoadPreset("assoc");
//
// 	Evaluate(@"
// (set fruits '((apple ((texture crunchy)))
// (banana ((colour yellow)))))");
//
// 	Evaluate(@"
// (define getprop (x p plist)
// ; find property p of individual x in plist
// (assoc p (assoc x plist)))");
//
// 	Assert.AreEqual("crunchy", Evaluate("(getprop 'apple 'texture fruits)"));
//
// 	Evaluate(@"
// (define putprop (x p y plist)
// ; give individual x value y for property p
// (mkassoc x (mkassoc p y (assoc x plist)) plist))");
//
// 	Evaluate("(set fruits (putprop 'apple 'colour 'red fruits))");
//
// 	Assert.AreEqual("red", Evaluate("(getprop 'apple 'colour fruits)"));
//
// 	Evaluate("(define hasprop? (p y alist) (= (assoc p alist) y))");
// 	Evaluate(@"
// (define gatherprop (p y plist)
// ; get all individuals having value y for property p
// (if (null? plist) '()
// (if (hasprop? p y (cadar plist))
// 	(cons (caar plist) (gatherprop p y (cdr plist)))
// 	(gatherprop p y (cdr plist)))))");
//
// 	Evaluate("(set fruits (putprop 'lemon 'colour 'yellow fruits))");
//
// 	Assert.AreEqual("(banana lemon)", Evaluate("(gatherprop 'colour 'yellow fruits)"));
// }

// [Test]
// public void RplacaRplacdTest()  // See page 55
// {
// 	Evaluate("(set x '(a b c))");
// 	Evaluate("(set y x)");
// 	Evaluate("(rplaca y 'd)");
// 	Assert.AreEqual("(d b c)", Evaluate("y"));
// 	Assert.AreEqual("(d b c)", Evaluate("x"));
//
// 	Evaluate("(rplacd y 'e)");
// 	Assert.AreEqual("(d . e)", Evaluate("y"));
// 	Assert.AreEqual("(d . e)", Evaluate("x"));
//
// 	// See exercise 8 on page 61
// 	globalInfo.LoadPreset("assoc");
//
// 	Evaluate(@"
// (define rplac-assoc (x y alist)
// (if (null? alist) '()
// (if (= x (caar alist))
// 	(rplacd (car alist) (list y))
// 	(if (null? (cdr alist))
// 		(rplacd alist (list (list x y)))
// 		(rplac-assoc x y (cdr alist))))))");
// 	Evaluate("(set test-alist (mkassoc 'b 2 (mkassoc 'a 1 '())))");
// 	Evaluate("(rplac-assoc 'a 7 test-alist)");
// 	Assert.AreEqual("7", Evaluate("(assoc 'a test-alist)"));
//
// 	Evaluate(@"
// (define snip-off-last-cdr (l)
// (if (null? l) '()
// (if (null? (cdr l)) l
// 	(if (null? (cdr (cdr l)))
// 		(let ((result (cdr l)))
// 			(begin
// 				(rplacd l '())
// 				result))
// 		(snip-off-last-cdr (cdr l))))))");
// 	Evaluate(@"
// (define set-last-cdr (l x)
// (if (null? l) '()
// (if (null? (cdr l))
// 	(rplacd l x)
// 	(set-last-cdr (cdr l) x))))");
// 	Evaluate(@"
// (define nreverse (l)
// (if (null? l) '()
// (if (null? (cdr l)) l
// 	(let ((old-first l)
// 		  (old-last (snip-off-last-cdr l))
// 		  (rest-of-list (cdr l)))
// 		(begin
// 			(rplacd old-first '())
// 			(rplacd old-last (nreverse rest-of-list))
// 			(set-last-cdr old-last old-first)
// 			old-last)))))");
//
// 	Assert.AreEqual("(7 6 5 4 3 2 1)", Evaluate("(nreverse '(1 2 3 4 5 6 7))"));
// }

// [Test]
// public void CountTest()     // Exercise 1a), from page 59
// {
// 	Evaluate(@"
// (define count (x l)
// (if (null? l) 0
// (if (= x (car l))
// 	(+1 (count x (cdr l)))
// 	(count x (cdr l)))))");
// 	Evaluate(@"
// (define countall (x l)
// (if (null? l) 0
// (if (= x (car l))
// 	(+1 (countall x (cdr l)))
// 	(if (atom? (car l))
// 		(countall x (cdr l))
// 		(+ (countall x (car l)) (countall x (cdr l)))))))");
//
// 	Assert.AreEqual("1", Evaluate("(count 'a '(1 b a (c a)))"));
// 	Assert.AreEqual("2", Evaluate("(countall 'a '(1 b a (c a)))"));
// }

// [Test]
// public void ReverseTest()     // Exercise 1b), from page 60
// {
// 	Evaluate(@"
// (define reverse (l)
// (if (null? l) '()
// (append (reverse (cdr l)) (list (car l)))))");
//
// 	Assert.AreEqual("(e (c d) b a)", Evaluate("(reverse '(a b (c d) e))"));
// }

// [Test]
// public void TwistTest()     // Exercise 1c), from page 60
// {
// 	Evaluate(@"
// (define twist (l)
// (if (atom? l) l
// (append (twist (cdr l)) (list (twist (car l))))
// ))");
//
// 	Assert.AreEqual("(e (d c) ((5 b) a))", Evaluate("(twist '((a (b 5)) (c d) e))"));
// }

// [Test]
// public void FlattenTest()     // Exercise 1d), from page 60
// {
// 	Evaluate(@"
// (define flatten (tree)
// (if (null? tree) '()
// (if (atom? tree) (list tree)
// 	(append (flatten (car tree)) (flatten (cdr tree))))))");
//
// 	Assert.AreEqual("(1 2 3 4)", Evaluate("(append '(1 2) '(3 4))"));
// 	Assert.AreEqual("()", Evaluate("(flatten '())"));
// 	Assert.AreEqual("(1 2 3 4)", Evaluate("(flatten '(1 2 3 4))"));
// 	Assert.AreEqual("(1 2 3 4)", Evaluate("(flatten '((1 2) (3 4)))"));
// 	Assert.AreEqual("(5 1 2 6 7 3 4)", Evaluate("(flatten '(5 (1 2 6) () ((7 3) 4)))"));
//
// 	Assert.AreEqual("(a b c d e)", Evaluate("(flatten '((a b) ((c d) e)))"));
// }

// [Test]
// public void SublistTest()     // Exercise 1e), from page 60
// {
// 	Evaluate(@"
// (define sublist (l1 l2)
// (if (null? l1) 'T
// (if (null? l2) '()
// 	(if (= (car l1) (car l2))
// 		(sublist (cdr l1) (cdr l2))
// 		(sublist l1 (cdr l2))))))");
// 	Evaluate(@"
// (define starts-with (l1 l2) ; True iff l1 starts with l2
// (if (null? l2) 'T
// (if (null? l1) '()
// 	(if (not (= (car l1) (car l2))) '()
// 		(starts-with (cdr l1) (cdr l2))))))");
// 	Evaluate(@"
// (define contig-sublist (l1 l2)
// (if (null? l2) '()
// (if (starts-with l2 l1) 'T
// 	(contig-sublist l1 (cdr l2)))))");
//
// 	Assert.AreEqual("()", Evaluate("(sublist '(a d c) '(x a y b z c))"));
// 	Assert.AreEqual("()", Evaluate("(sublist '(c b a) '(x a y b z c))"));
// 	Assert.AreEqual("T", Evaluate("(sublist '(a b c) '(x a y b z c))"));
//
// 	Assert.AreEqual("T", Evaluate("(starts-with '(a b c) '(a b))"));
// 	Assert.AreEqual("T", Evaluate("(starts-with '(a b c) '(a b c))"));
// 	Assert.AreEqual("()", Evaluate("(starts-with '(a b c) '(a b c d))"));
// 	Assert.AreEqual("()", Evaluate("(starts-with '(a b c) '(b c))"));
// 	Assert.AreEqual("()", Evaluate("(starts-with '(a b c) '(a d))"));
//
// 	Assert.AreEqual("()", Evaluate("(contig-sublist '(a b c) '(x a y b z c))"));
// 	Assert.AreEqual("T", Evaluate("(contig-sublist '(a y) '(x a y b z c))"));
// }

// [Test]
// public void SetsTest()     // Exercise 2, from page 60
// {
// 	// Basic set functions from page 34
// 	Evaluate("(set nullset '())");
// 	Evaluate(@"
// (define member? (x s)
// (if (null? s) '()
// (if (equal x (car s)) 'T (member? x (cdr s)))))");
// 	Evaluate("(define addelt (x s) (if (member? x s) s (cons x s)))");
// 	Evaluate("(define size (s) (length s))");
// 	Evaluate(@"
// (define union (s1 s2)
// (if (null? s1) s2
// (if (member? (car s1) s2)
// 	(union (cdr s1) s2)
// 	(cons (car s1) (union (cdr s1) s2)))))");
//
// 	Assert.AreEqual("(3 a)", Evaluate("(set s (addelt 3 (addelt 'a nullset)))"));
// 	Assert.AreEqual("T", Evaluate("(member? 'a s)"));
// 	Assert.AreEqual("(a 2 3)", Evaluate("(union s (addelt 2 (addelt 3 nullset)))"));
//
// 	// Exercise 2a)
// 	Evaluate(@"
// (define remove (x s)
// (if (null? s) '()
// (if (equal x (car s))
// 	(cdr s)
// 	(addelt (car s) (remove x (cdr s))))))");
//
// 	Assert.AreEqual("(1 2 3)", Evaluate("(set s (addelt 1 (addelt 2 (addelt 3 nullset))))"));
// 	Assert.AreEqual("(2 3)", Evaluate("(remove 1 s)"));
// 	Assert.AreEqual("(1 3)", Evaluate("(remove 2 s)"));
// 	Assert.AreEqual("(1 2)", Evaluate("(remove 3 s)"));
// 	Assert.AreEqual("(1 2 3)", Evaluate("(remove 4 s)"));
//
// 	// Exercise 2b)
// 	Evaluate(@"
// (define subset (s1 s2)
// (if (null? s1) 'T
// (and (member? (car s1) s2) (subset (cdr s1) s2))))");
//
// 	Assert.AreEqual("T", Evaluate("(subset (addelt 3 (addelt 1 nullset)) s)"));
// 	Assert.AreEqual("()", Evaluate("(subset (addelt 3 (addelt 4 nullset)) s)"));
//
// 	// Exercise 2c)
// 	Evaluate("(define =set (s1 s2) (and (subset s1 s2) (subset s2 s1)))");
//
// 	Assert.AreEqual("()", Evaluate("(=set (addelt 3 (addelt 1 nullset)) s)"));
// 	Assert.AreEqual("T", Evaluate("(=set (addelt 3 (addelt 1 (addelt 2 nullset))) s)"));
// 	Assert.AreEqual("()", Evaluate("(=set (addelt 3 (addelt 1 (addelt 2 (addelt 4 nullset)))) s)"));
// }

// [Test]
// public void TreeTraversalTest()     // From pages 35-37, and Exercise 4, from pages 60-61
// {
// 	Evaluate("(set sample-tree '(A (B C D) (E (F G H) I)))");
//
// 	// Pre-order traversal
// 	Evaluate(@"
// (define pre-ord-children (children)
// (if (null? children) '()
// (append (pre-ord (car children)) (pre-ord-children (cdr children)))))");
// 	Evaluate(@"
// (define pre-ord (tree)
// (if (atom? tree) (list tree)
// (append (list (car tree)) (pre-ord-children (cdr tree)))))");
//
// 	Assert.AreEqual("(A B C D E F G H I)", Evaluate("(pre-ord sample-tree)"));
//
// 	// Level-order traversal
// 	globalInfo.LoadPreset("queue");
//
// 	Evaluate(@"
// (define level-ord-enqueue-children (children node-q)
// (if (null? children) node-q
// (level-ord-enqueue-children (cdr children) (enqueue (car children) node-q))))");
// 	Evaluate(@"
// (define level-ord* (node-q)
// (if (empty? node-q) '()
// (begin
// 	(set this-node (front node-q))
// 	(if (atom? this-node)
// 		(append (list this-node) (level-ord* (rm-front node-q)))
// 		(append (list (car this-node)) (level-ord* (level-ord-enqueue-children (cdr this-node) (rm-front node-q))))))))");
// 	Evaluate("(define level-ord (tree) (level-ord* (enqueue tree empty-queue)))");
//
// 	Assert.AreEqual("(A B E C D F I G H)", Evaluate("(level-ord sample-tree)"));
//
// 	// Post-order traversal
// 	Evaluate(@"
// (define post-ord-children (children)
// (if (null? children) '()
// (append (post-ord (car children)) (post-ord-children (cdr children)))))");
// 	Evaluate(@"
// (define post-ord (tree)
// (if (atom? tree) (list tree)
// (append (post-ord-children (cdr tree)) (list (car tree)))))");
//
// 	Assert.AreEqual("(C D B G H F I E A)", Evaluate("(post-ord sample-tree)"));
// }

// [Test]
// public void MacroTest()     // From pages 56-57, and Exercise 12, from pages 62-63
// {
// 	Evaluate("(define <= (x y) (or (< x y) (= x y)))");
// 	Evaluate(@"
// (define-macro for (indexvar lower upper body)
// (list 'begin
// (list 'set indexvar lower)
// (list 'while
// 	(list '<= indexvar upper)
// 	(list 'begin body
// 		(list 'set indexvar (list '+ indexvar 1))))))");
// 	Evaluate("(set sum 0)");
// 	Evaluate("(for x 1 10 (set sum (+ sum x)))");
// 	Assert.AreEqual("55", Evaluate("sum"));
// }

// [Test]
// public void RandomTest()
// {
// 	const int maxValue = 100;
// 	var x = int.Parse(Evaluate(string.Format("(random {0})", maxValue)));
//
// 	Assert.IsTrue(x >= 0);
// 	Assert.IsTrue(x < maxValue);
// }

// [Test]
// public void RelationalDatabaseTest()    // From Section 2.3 (pages 39-45) in Kamin
// {
// 	globalInfo.LoadPreset("set");
//
// 	// Functions from Figure 2.3
// 	Evaluate(@"
// (define UNION (r s)
// (if (not (equal (car r) (car s)))
// (print 'UNION-error)
// (cons (car r) (union (cdr r) (cdr s)))))");
// 	Evaluate(@"
// (define INTER (r s)
// (if (not (equal (car r) (car s)))
// (print 'INTER-error)
// (cons (car r) (inter (cdr r) (cdr s)))))");
// 	Evaluate(@"
// (define DIFF (r s)
// (if (not (equal (car r) (car s)))
// (print 'DIFF-error)
// (cons (car r) (diff (cdr r) (cdr s)))))");
//
// 	// Functions from Figure 2.4
// 	Evaluate(@"
// (define SELECT (A v r)
// (cons (car r) (include-rows v (col-num A (car r)) (cdr r))))");
// 	Evaluate(@"
// (define col-num (A A-list)
// (if (= A (car A-list)) 0
// (+1 (col-num A (cdr A-list)))))");
// 	Evaluate(@"
// (define include-rows (v n rows)
// (if (null? rows) '()
// (if (= v (nth n (car rows)))
// 	(cons (car rows) (include-rows v n (cdr rows)))
// 	(include-rows v n (cdr rows)))))");
//
// 	// Functions from Figure 2.5
// 	Evaluate(@"
// (define PROJECT (X r)
// (cons X (include-cols* (col-num* X (car r)) (cdr r))))");
// 	Evaluate(@"
// (define col-num* (X A-list)
// (if (null? X) '()
// (cons (col-num (car X) A-list) (col-num* (cdr X) A-list))))");
// 	Evaluate(@"
// (define include-cols* (col-nums rows)
// (if (null? rows) nullset
// (addelt (include-cols col-nums (car rows))
// 	(include-cols* col-nums (cdr rows)))))");
// 	Evaluate(@"
// (define include-cols (col-nums row)
// (if (null? col-nums) '()
// (cons (nth (car col-nums) row)
// 	(include-cols (cdr col-nums) row))))");
//
// 	// Functions from Figure 2.6
// 	Evaluate(@"
// (define JOIN (r s)
// (begin
// (set intersection (inter (car r) (car s)))
// (set r-intersection (col-num* intersection (car r)))
// (set s-intersection (col-num* intersection (car s)))
// (set r-diff-s (diff (car r) intersection))
// (set r-diff-s-cols (col-num* r-diff-s (car r)))
// (set s-diff-r (diff (car s) intersection))
// (set s-diff-r-cols (col-num* s-diff-r (car s)))
// (cons (append intersection (append r-diff-s s-diff-r))
// (join-cols* r-intersection r-diff-s-cols s-intersection s-diff-r-cols (cdr r) (cdr s)))))");
// 	Evaluate(@"
// (define join-cols* (X-r r-cols X-s s-cols r-rows s-rows)
// (begin
// (set new-rows '())
// (while (not (null? r-rows))
// 	(begin
// 		(set s-tmp s-rows)
// 		(while (not (null? s-tmp))
// 			(begin
// 				(if (equal (include-cols X-r (car r-rows)) (include-cols X-s (car s-tmp)))
// 					(set new-rows (cons (join-cols X-r r-cols s-cols (car r-rows) (car s-tmp)) new-rows))
// 					'())
// 				(set s-tmp (cdr s-tmp))))
// 		(set r-rows (cdr r-rows))))
// new-rows))");
// 	Evaluate(@"
// (define join-cols (X-r r-cols s-cols r-row s-row)
// (append (include-cols X-r r-row)
// (append (include-cols r-cols r-row)
// 	(include-cols s-cols s-row))))");
//
// 	// The sample session from pages 44-45 :
// 	Evaluate(@"
// (set CRIMES
// '((Victim Crime Criminal Location)
// (Phelps robbery Harrison London)
// (Drebber murder Hope London)
// (Sir-Charles murder Stapleton Devonshire)
// (Lady-Eva blackmail Milverton London)
// (Brunton murder Howells West-Sussex)))");
// 	Evaluate(@"
// (set MURDERS
// '((Victim Weapon Motive)
// (Drebber poison revenge)
// (Sir-Charles hound greed)
// (Brunton burial-alive passion)))");
// 	Evaluate(@"(set LONDON-MURDERS
// (JOIN MURDERS
// (PROJECT '(Victim Criminal)
// (SELECT 'Location 'London
// 	(SELECT 'Crime 'murder CRIMES)))))");
//
// 	Assert.AreEqual("((Victim Weapon Motive Criminal) (Drebber poison revenge Hope))", Evaluate("LONDON-MURDERS"));
//
// 	// Exercise 5a) on page 61
// 	Evaluate(@"
// (define AND-SELECT (A-list v-list r)
// (if (or (null? A-list) (null? v-list)) r
// (INTER (SELECT (car A-list) (car v-list) r) (AND-SELECT (cdr A-list) (cdr v-list) r))))");
//
// 	Assert.AreEqual("((Victim Crime Criminal Location) (Drebber murder Hope London))",
// 		Evaluate("(AND-SELECT '(Crime Location) '(murder London) CRIMES)"));
//
// 	// Exercise 5b) on page 61
// 	Evaluate(@"
// (define OR-SELECT (A v-list r)
// (if (null? v-list) (list (car r))
// (UNION (SELECT A (car v-list) r) (OR-SELECT A (cdr v-list) r))))");
//
// 	Assert.AreEqual("((Victim Weapon Motive) (Drebber poison revenge) (Sir-Charles hound greed))",
// 		Evaluate("(OR-SELECT 'Weapon '(poison hound) MURDERS)"));
//
// 	// Exercise 5c) on page 61
// 	Evaluate(@"
// (define is-subset-of (s1 s2)
// (if (null? s1) 'T
// (if (not (member? (car s1) s2)) '()
// 	(is-subset-of (cdr s1) s2))))");
// 	Evaluate("(define =set (s1 s2) (and (is-subset-of s1 s2) (is-subset-of s2 s1)))");
// 	Evaluate(@"
// (define UNION2 (r s)
// (if (not (=set (car r) (car s)))
// (print 'UNION2-error)
// (cons (car r) (union (cdr r) (cdr (PROJECT (car r) s))))))");
// 	Evaluate(@"
// (define INTER2 (r s)
// (if (not (=set (car r) (car s)))
// (print 'INTER2-error)
// (cons (car r) (inter (cdr r) (cdr (PROJECT (car r) s))))))");
// 	Evaluate(@"
// (define DIFF2 (r s)
// (if (not (=set (car r) (car s)))
// (print 'DIFF2-error)
// (cons (car r) (diff (cdr r) (cdr (PROJECT (car r) s))))))");
//
// 	Evaluate(@"(set LONDON-MURDERS2
// (PROJECT '(Victim Criminal)
// (SELECT 'Location 'London
// (SELECT 'Crime 'murder CRIMES))))");
// 	Evaluate(@"(set DEVONSHIRE-MURDERS
// (PROJECT '(Criminal Victim)
// (SELECT 'Location 'Devonshire
// (SELECT 'Crime 'murder CRIMES))))");
//
// 	Assert.AreEqual("((Victim Criminal) (Drebber Hope) (Sir-Charles Stapleton))",
// 		Evaluate("(UNION2 LONDON-MURDERS2 DEVONSHIRE-MURDERS)"));
//
// 	Evaluate(@"(set MURDER-CRIMES
// (PROJECT '(Victim Criminal)
// (SELECT 'Crime 'murder CRIMES)))");
// 	Evaluate(@"(set LONDON-CRIMES
// (PROJECT '(Criminal Victim)
// (SELECT 'Location 'London CRIMES)))");
//
// 	Assert.AreEqual("((Victim Criminal) (Drebber Hope))",
// 		Evaluate("(INTER2 MURDER-CRIMES LONDON-CRIMES)"));
// 	Assert.AreEqual("((Victim Criminal) (Sir-Charles Stapleton) (Brunton Howells))",
// 		Evaluate("(DIFF2 MURDER-CRIMES LONDON-CRIMES)"));
//
// 	// Exercise 5d) on page 61
// 	Evaluate("(define REMOVE (X r) (PROJECT (diff (car r) X) r))");
//
// 	Assert.AreEqual("((Victim Weapon) (Drebber poison))",
// 		Evaluate("(REMOVE '(Criminal Motive) LONDON-MURDERS)"));
// 	Assert.AreEqual("((Weapon) (poison))",
// 		Evaluate("(REMOVE '(Criminal Victim Motive) LONDON-MURDERS)"));
// }

// [Test]
// public void EvalInLISPTest()    // From Section 2.4 (pages 46-51) in Kamin
// {
// 	globalInfo.LoadPreset("assoc");
//
// 	Evaluate("(define cadr (x) (car (cdr x)))");
// 	Evaluate("(define cddr (x) (cdr (cdr x)))");
// 	Evaluate("(define caddr (x) (car (cddr x)))");
// 	Evaluate("(define cadddr (x) (caddr (cdr x)))");
//
// 	// Functions from page 48
// 	Evaluate(@"
// (define apply-binary-op (f x y)
// (cond
// ((= f 'cons) (cons x y))
// ((= f '+) (+ x y))
// ((= f '-) (- x y))
// ((= f '*) (* x y))
// ((= f '/) (/ x y))
// ((= f '<) (< x y))
// ((= f '>) (> x y))
// ((= f '=) (= x y))
// ('T 'binary-op-error!)))");
// 	Evaluate(@"
// (define apply-unary-op (f x)
// (cond
// ((= f 'car) (car x))
// ((= f 'cdr) (cdr x))
// ((= f 'number?) (number? x))
// ((= f 'list?) (list? x))
// ((= f 'symbol?) (symbol? x))
// ((= f 'null?) (null? x))
// ('T 'unary-op-error!)
// ; ('T f)
// ))");
//
// 	Evaluate(@"
// (define do-begin (expr-list rho fundefs)
// (if (null? (cdr expr-list))
// (eval (car expr-list) rho fundefs)
// (begin
// 	(eval (car expr-list) rho fundefs)
// 	(do-begin (cdr expr-list) rho fundefs))))");
// 	Evaluate(@"
// (define do-let (var-expr-list expr rho-in rho-in-progress fundefs)
// (if (null? var-expr-list)
// (eval expr rho-in-progress fundefs)
// (do-let (cdr var-expr-list) expr rho-in
// 	(mkassoc
// 		(caar var-expr-list)
// 		(eval (cadar var-expr-list) rho-in fundefs)
// 		rho-in-progress)
// 	fundefs)))");
// 	Evaluate(@"
// (define do-let* (var-expr-list expr rho fundefs)
// (if (null? var-expr-list)
// (eval expr rho fundefs)
// (do-let* (cdr var-expr-list) expr
// 	(mkassoc
// 		(caar var-expr-list)
// 		(eval (cadar var-expr-list) rho fundefs)
// 		rho)
// 	fundefs)))");
// 	Evaluate(@"
// (define do-cond (expr-pair-list rho fundefs)
// (if (null? expr-pair-list)
// '()
// (if (eval (caar expr-pair-list) rho fundefs)
// 	(eval (cadar expr-pair-list) rho fundefs)
// 	(do-cond (cdr expr-pair-list) rho fundefs))))");
//
// 	// Functions from Figure 2.7
// 	Evaluate(@"
// (define eval (expr rho fundefs)
// (cond
// ((number? expr) expr)
// ((symbol? expr)
// 	(if (assoc-contains-key expr rho)
// 		(assoc expr rho)
// 		(assoc expr global-environment)))
// ((= (car expr) 'quote) (cadr expr))
// ((= (car expr) 'if)
// 	(if (null? (eval (cadr expr) rho fundefs))
// 		(eval (cadddr expr) rho fundefs)
// 		(eval (caddr expr) rho fundefs)))
// ((= (car expr) 'begin) (do-begin (cdr expr) rho fundefs)) ; Exercise 6a) on page 61
// ((= (car expr) 'print) ; Exercise 6a) on page 61
// 	(print (eval (cadr expr) rho fundefs)))
// ((= (car expr) 'set)
// 	(let ((evaluated-expression (eval (caddr expr) rho fundefs)))
// 		(if (assoc-contains-key (cadr expr) rho)
// 			(begin
// 				(rplac-assoc (cadr expr) evaluated-expression rho)
// 				evaluated-expression)
// 			(begin
// 				(set global-environment (mkassoc (cadr expr) evaluated-expression global-environment))
// 				evaluated-expression))))
// ((= (car expr) 'let) (do-let (cadr expr) (caddr expr) rho rho fundefs))
// ((= (car expr) 'let*) (do-let* (cadr expr) (caddr expr) rho fundefs))
// ((= (car expr) 'cond) (do-cond (cdr expr) rho fundefs))
// ((= (car expr) 'list) (evallist (cdr expr) rho fundefs))
// ((userfun? (car expr) fundefs)
// 	(apply-userfun
// 		(assoc (car expr) fundefs)
// 		(evallist (cdr expr) rho fundefs)
// 		fundefs))
// ((= (length expr) 2)
// 	(apply-unary-op (car expr) (eval (cadr expr) rho fundefs)))
// ('T (apply-binary-op (car expr)
// 		(eval (cadr expr) rho fundefs)
// 		(eval (caddr expr) rho fundefs)))))");
// 	Evaluate("(define userfun? (f fundefs) (assoc-contains-key f fundefs))");
// 	Evaluate(@"
// (define apply-userfun (fundef args fundefs)
// (eval (cadr fundef) ; body of function
// (mkassoc* (car fundef) args '()) ; local env
// fundefs))");
// 	Evaluate(@"
// (define evallist (el rho fundefs)
// (if (null? el) '()
// (cons (eval (car el) rho fundefs)
// 	(evallist (cdr el) rho fundefs))))");
// 	Evaluate(@"
// (define mkassoc* (keys values al)
// (if (null? keys) al
// (mkassoc* (cdr keys) (cdr values)
// 	(mkassoc (car keys) (car values) al))))");
//
// 	// Functions from Figure 2.8
// 	Evaluate(@"
// (define r-e-p-loop (inputs)
// (begin
// (set global-environment '())
// (r-e-p-loop* inputs '())))");
// 	Evaluate(@"
// (define r-e-p-loop* (inputs fundefs)
// (cond
// ((null? inputs) '()) ; session done
// ((atom? (car inputs)) ; input is variable or number
// 	(process-exp (car inputs) (cdr inputs) fundefs))
// ((= (caar inputs) 'define) ; input is function definition
// 	(process-def (car inputs) (cdr inputs) fundefs))
// ('T (process-exp (car inputs) (cdr inputs) fundefs))))");
// 	Evaluate(@"
// (define process-def (e inputs fundefs)
// (cons (cadr e) ; echo function name
// (r-e-p-loop* inputs
// 	(mkassoc (cadr e) (cddr e) fundefs))))");
// 	Evaluate(@"
// (define process-exp (e inputs fundefs)
// (cons (eval e '() fundefs) ; print value of expression
// (r-e-p-loop* inputs fundefs)))");
//
// 	Assert.AreEqual("23", Evaluate("(eval '(+ 3 (* 4 5)) '() '())"));   // Adapted from page 46
// 	Assert.AreEqual("6", Evaluate("(eval '(+ i (/ 9 i)) (mkassoc 'i 3 '()) '())")); // Adapted from page 47
// 	//Assert.AreEqual("b", Evaluate("(eval '(car (cdr (quote (a b c)))) '() '())"));  // Adapted from page 47
// 	Assert.AreEqual("b", Evaluate("(eval (list 'car (list 'cdr (list 'quote '(a b c)))) '() '())"));  // Adapted from page 47
//
// 	// From page 48 :
// 	Evaluate("(set E (mkassoc 'double '((a) (+ a a)) '()))");
// 	Assert.AreEqual("8", Evaluate("(eval '(double 4) '() E)")); // My simplified test
// 	//Assert.AreEqual("8", Evaluate("(eval '(double (car (quote (4 5)))) '() E)"));
// 	Assert.AreEqual("8", Evaluate("(eval (list 'double (list 'car (list 'quote '(4 5)))) '() E)"));
// 	Evaluate(@"
// (set E (mkassoc 'expo
// '((m n) (if (= n 0) 1 (* m (expo m (- n 1)))))
// '()))");
// 	Assert.AreEqual("64", Evaluate("(eval '(expo 4 3) '() E)"));
//
// 	// The first example session, from page 50
// 	/*
// 	Assert.AreEqual("(double 8 expo 64)", Evaluate(@"
// (r-e-p-loop '(
// (define double (a) (+ a a))
// (double (car (quote (4 5))))
// (define expo (m n) (if (= n 0) 1 (* m (expo m (- n 1)))))
// (expo 4 3)
// ))"));
// 	 */
// 	Assert.AreEqual("(double 8 expo 64)", Evaluate(@"
// (r-e-p-loop (list
// '(define double (a) (+ a a))
// (list 'double (list 'car (list 'quote '(4 5))))
// '(define expo (m n) (if (= n 0) 1 (* m (expo m (- n 1)))))
// '(expo 4 3)
// ))"));
//
// 	// Part of exercise 6a) on page 61 : print
// 	/*
// 	Assert.AreEqual("(print-test (1 2 3 4 5 6 7 8 9 10))", Evaluate(@"
// (r-e-p-loop '(
// (define print-test (m n) (if (> m n) (quote ()) (cons (print m) (print-test (+ m 1) n))))
// (print-test 1 10)
// ))"));
// 	 */
// 	Assert.AreEqual("(print-test (1 2 3 4 5 6 7 8 9 10))", Evaluate(@"
// (r-e-p-loop (list
// (list 'define 'print-test '(m n) (list 'if '(> m n) (list 'quote '()) '(cons (print m) (print-test (+ m 1) n))))
// '(print-test 1 10)
// ))"));
//
// 	// Part of exercise 6a) on page 61 : begin... and all of exercise 6b) : set and global variables
// 	Assert.AreEqual("(0 begin-test 13 14)", Evaluate(@"
// (r-e-p-loop '(
// (set global-variable 0)
// (define begin-test (n) (begin (set global-variable (+ n 1)) n))
// (begin-test 13)
// global-variable
// ))"));
//
// 	// Setting a local variable (a function parameter)
// 	Assert.AreEqual("(add1 14)", Evaluate(@"
// (r-e-p-loop '(
// (define add1 (n) (begin (set n (+ n 1)) n))
// (add1 13)
// ))"));
//
// 	// Exercise 6d) on page 61 : Local variables, which I will implement in the form of "let" and "let*"
// 	Assert.AreEqual("(let-test 169)", Evaluate(@"
// (r-e-p-loop '(
// (define let-test (x) (let ((y (* x x))) y))
// (let-test 13)
// ))"));
// 	Assert.AreEqual("(let*-test 343)", Evaluate(@"
// (r-e-p-loop '(
// (define let*-test (x) (let* ((y (* x x)) (z (* x y))) z))
// (let*-test 7)
// ))"));
//
// 	// cond test
// 	/*
// 	Assert.AreEqual("(condtest Other First Second Third Other)", Evaluate(@"
// (r-e-p-loop '(
// (define condtest (n) (cond ((= n 1) (quote First)) ((= n 2) (quote Second)) ((= n 3) (quote Third)) ((quote T) (quote Other))))
// (condtest 0)
// (condtest 1)
// (condtest 2)
// (condtest 3)
// (condtest 4)
// ))"));
// 	 */
// 	Assert.AreEqual("(condtest Other First Second Third Other)", Evaluate(@"
// (r-e-p-loop (list
// (list 'define 'condtest '(n) (list 'cond (list '(= n 1) (list 'quote 'First)) (list '(= n 2) (list 'quote 'Second)) (list '(= n 3) (list 'quote 'Third)) (list (list 'quote 'T) (list 'quote 'Other))))
// '(condtest 0)
// '(condtest 1)
// '(condtest 2)
// '(condtest 3)
// '(condtest 4)
// ))"));
//
// 	// "list" tests.
// 	Assert.AreEqual("((2 3 5 7) (1 2 3 5))", Evaluate(@"
// (r-e-p-loop '(
// (list 2 3 5 7)
// (list (+ 0 1) (+ 1 1) (+ 1 2) (+ 2 3))
// ))"));
//
// 	// Test of "let".
// 	Assert.AreEqual("(60)", Evaluate(@"
// (r-e-p-loop '(
// (let ((a 2) (b 3) (c 5) (d (+ 3 4)))
// (* (+ a b) (+ c d)))
// ))"));
//
// 	// Test of "let*".
// 	Assert.AreEqual("(24)", Evaluate(@"
// (r-e-p-loop '(
// (let* ((a 1) (b (* a 2)) (c (* b 3)) (d (* c 4)))
// d)
// ))"));
// }

// [Test]
// public void QuoteKeywordTest()
// {
// 	const string expectedResult = "T";
// 	var sexpr = EvaluateToSExpression(string.Format("'(quote {0})", expectedResult));
//
// 	Assert.IsTrue(sexpr is QuotedConstantWithQuoteKeyword);
//
// 	var qc = (QuotedConstantWithQuoteKeyword)sexpr;
// 	Assert.AreEqual(expectedResult, qc.sexpression.ToString());
//
// 	//Assert.AreEqual("T", Evaluate("(quote T)"));
// 	//Assert.AreEqual("()", Evaluate("(quote ())"));
// 	Assert.AreEqual("(quote T)", Evaluate("'(quote T)"));
// 	Assert.AreEqual("(quote ())", Evaluate("'(quote ())"));
// 	//Assert.AreEqual("(quote T)", Evaluate("(quote (quote T))"));
// }

// [Test]
// public void Cond4AsMacroTest()  // See the end of exercise 12 on page 63
// {
// 	Evaluate("(define cadr (l) (car (cdr l)))");
// 	Evaluate(@"
// (define-macro cond4 (x1 x2 x3 x4)
// (list 'if (car x1) (cadr x1)
// (list 'if (car x2) (cadr x2)
// 	(list 'if (car x3) (cadr x3)
// 		(list 'if (car x4) (cadr x4) '(quote ()))))))");
// 	Evaluate("(define cond4test (n) (cond4 '((= n 1) (quote First)) '((= n 2) (quote Second)) '((= n 3) (quote Third)) '((quote T) (quote Other))))");
//
// 	Assert.AreEqual("Other", Evaluate("(cond4test 0)"));
// 	Assert.AreEqual("First", Evaluate("(cond4test 1)"));
// 	Assert.AreEqual("Second", Evaluate("(cond4test 2)"));
// 	Assert.AreEqual("Third", Evaluate("(cond4test 3)"));
// 	Assert.AreEqual("Other", Evaluate("(cond4test 4)"));
// }

// [Test]
// public void DynamicScopingTest()
// {
// 	globalInfo.DynamicScoping = true;
//
// 	Evaluate("(define innerFunc () (set x 13))");
// 	Evaluate(@"
// (define outerFunc ()
// (let ((x 7))
// (begin
// 	(innerFunc)
// 	x)))");
//
// 	Assert.AreEqual("13", Evaluate("(outerFunc)"));
// }

// [Test]
// public void PrintTest()
// {
// 	Assert.AreEqual("7", Evaluate("(print 7)"));                // Number
// 	Assert.AreEqual("7", Evaluate("(print '7)"));               // Quoted number
// 	Assert.AreEqual("T", Evaluate("(print 'T)"));               // Symbol
// 	Assert.AreEqual("(1 2 3)", Evaluate("(print '(1 2 3))"));   // List 1
// 	Assert.AreEqual("(())", Evaluate("(print '(()))"));         // List 2
// 	Assert.AreEqual("()", Evaluate("(print '())"));             // Null
// 	Assert.AreEqual("ABC", Evaluate("(print \"ABC\")"));        // String
// 	Assert.AreEqual("ABC", Evaluate("(print '\"ABC\")"));       // Quoted string
// }
//
// [Test]
// public void ToStringTest()
// {
// 	Assert.AreEqual("7", Evaluate("(tostring 7)"));                 // Number
// 	Assert.AreEqual("7", Evaluate("(tostring '7)"));                // Quoted number
// 	Assert.AreEqual("T", Evaluate("(tostring 'T)"));                // Symbol
// 	Assert.AreEqual("(1 2 3)", Evaluate("(tostring '(1 2 3))"));    // List 1
// 	Assert.AreEqual("(())", Evaluate("(tostring '(()))"));          // List 2
// 	Assert.AreEqual("()", Evaluate("(tostring '())"));              // Null
// 	Assert.AreEqual("ABC", Evaluate("(tostring \"ABC\")"));         // String
// 	Assert.AreEqual("ABC", Evaluate("(tostring '\"ABC\")"));        // Quoted string
// }
//
// [Test]
// public void ListToStringTest()
// {
// 	Assert.AreEqual("ABC", Evaluate("(listtostring '(\"A\" \"B\" \"C\"))"));
// }
//
// [Test]
// public void StringToListTest()
// {
// 	Assert.AreEqual("(A B C)", Evaluate("(stringtolist \"ABC\")"));
// }
//
// [Test]
// public void StringToSymbolTest()
// {
// 	Assert.AreEqual("ABC", Evaluate("(stringtosymbol \"ABC\")"));
// 	Assert.AreEqual("T", Evaluate("(symbol? (stringtosymbol \"ABC\"))"));
// }

// TODO: Add floating-point numbers to the LISP grammar
test('LL(1) LISP floating-point numbers test', () => {
	lispTest([
		['(+ 1.25 2)', '3.25'],
		['(+ 1 2.5)', '3.5'],
		['(+ 1.25 2.5)', '3.75'],

		['(> 1.25 2)', '()'],
		['(> 1 2.5)', '()'],
		['(> 1.25 2.5)', '()'],

		['(> 3.25 2)', 'T'],
		['(> 3 2.5)', 'T'],
		['(> 3.25 2.5)', 'T']
	]);
});

// [Test]
// public void MacroApostrophesToQuoteKeywordsTest()
// {
// 	// Note that these expressions are parsed, but not evaluated.
// 	Assert.AreEqual("(quote foo)", MacroDefinition.ObjectToString_ApostrophesToQuoteKeywords(GetParseResult("'foo")));
// 	Assert.AreEqual("(list (quote foo) (quote bar))", MacroDefinition.ObjectToString_ApostrophesToQuoteKeywords(GetParseResult("(list 'foo 'bar)")));
// 	Assert.AreEqual("(func1 (func2 (func3 (quote foo))))", MacroDefinition.ObjectToString_ApostrophesToQuoteKeywords(GetParseResult("(func1 (func2 (func3 'foo)))")));
//
// 	Assert.AreEqual("(define foo () (quote bar))", MacroDefinition.ObjectToString_ApostrophesToQuoteKeywords(GetParseResult("(define foo () 'bar)")));
// 	Assert.AreEqual("(if foo (quote bar) (quote baz))", MacroDefinition.ObjectToString_ApostrophesToQuoteKeywords(GetParseResult("(if foo 'bar 'baz)")));
// 	Assert.AreEqual("(while foo (quote bar))", MacroDefinition.ObjectToString_ApostrophesToQuoteKeywords(GetParseResult("(while foo 'bar)")));
// 	Assert.AreEqual("(set foo (quote bar))", MacroDefinition.ObjectToString_ApostrophesToQuoteKeywords(GetParseResult("(set foo 'bar)")));
// 	Assert.AreEqual("(begin (quote foo) (quote bar) (quote baz))", MacroDefinition.ObjectToString_ApostrophesToQuoteKeywords(GetParseResult("(begin 'foo 'bar 'baz)")));
// 	Assert.AreEqual("(cond ((= foo (quote bar)) (quote baz)) ((quote T) (quote bat)))", MacroDefinition.ObjectToString_ApostrophesToQuoteKeywords(GetParseResult("(cond ((= foo 'bar) 'baz) ('T 'bat))")));
// 	Assert.AreEqual("(let ((foo (quote bar))) (quote baz))", MacroDefinition.ObjectToString_ApostrophesToQuoteKeywords(GetParseResult("(let ((foo 'bar)) 'baz)")));
// 	Assert.AreEqual("(let* ((foo (quote bar))) (quote baz))", MacroDefinition.ObjectToString_ApostrophesToQuoteKeywords(GetParseResult("(let* ((foo 'bar)) 'baz)")));
// 	Assert.AreEqual("(list (quote foo) (quote bar))", MacroDefinition.ObjectToString_ApostrophesToQuoteKeywords(GetParseResult("(list 'foo 'bar)")));
// 	Assert.AreEqual("(foo (quote bar) (quote baz))", MacroDefinition.ObjectToString_ApostrophesToQuoteKeywords(GetParseResult("(foo 'bar 'baz)")));
// 	Assert.AreEqual("(quote (quote foo))", MacroDefinition.ObjectToString_ApostrophesToQuoteKeywords(GetParseResult("'(quote foo)")));
// 	Assert.AreEqual("(define-macro for (indexvar lower upper body) (list (quote begin) (list (quote set) indexvar lower) (list (quote while) (list (quote <=) indexvar upper) (list (quote begin) body (list (quote set) indexvar (list (quote +) indexvar 1))))))",
// 		MacroDefinition.ObjectToString_ApostrophesToQuoteKeywords(GetParseResult(@"
// (define-macro for (indexvar lower upper body)
// (list 'begin
// (list 'set indexvar lower)
// (list 'while
// 	(list '<= indexvar upper)
// 	(list 'begin body
// 		(list 'set indexvar (list '+ indexvar 1))))))")));
// }
//
// [Test]
// public void MacroSExpressionToStringTest()
// {
// 	Assert.AreEqual("'foo", MacroDefinition.SExpressionToStringForReparse(EvaluateToSExpression("'(quote foo)")));
// 	Assert.AreEqual("'(quote foo)", MacroDefinition.SExpressionToStringForReparse(EvaluateToSExpression("'(quote (quote foo))")));
// 	Assert.AreEqual("('foo 'bar)", MacroDefinition.SExpressionToStringForReparse(EvaluateToSExpression("'((quote foo) (quote bar))")));
//
// 	Assert.AreEqual("7", MacroDefinition.SExpressionToStringForReparse(EvaluateToSExpression("7")));
// 	Assert.AreEqual("sym", MacroDefinition.SExpressionToStringForReparse(EvaluateToSExpression("'sym")));
// 	Assert.AreEqual("str", MacroDefinition.SExpressionToStringForReparse(EvaluateToSExpression("\"str\"")));
// 	Assert.AreEqual("(1 2 3)", MacroDefinition.SExpressionToStringForReparse(EvaluateToSExpression("'(1 2 3)")));
// 	Assert.AreEqual("()", MacroDefinition.SExpressionToStringForReparse(EvaluateToSExpression("'()")));
// 	//Assert.AreEqual("", MacroDefinition.SExpressionToStringForReparse(EvaluateToSExpression("")));
// }

test('LL(1) LISP throw test', () => {
	expect(() => evaluateToISExpression('(throw "Hello World!")')).toThrow('LISPException');
});

test('LL(1) LISP string< test', () => {
	// 2013/12/14

	lispTest([
		['(string< "a" "a")', '()'],
		['(string< "a" "b")', 'T'],
		['(string< "b" "a")', '()'],
		['(string< "abac" "abacus")', 'T'],
		['(string< "abacab" "abacus")', 'T']
	]);
});
