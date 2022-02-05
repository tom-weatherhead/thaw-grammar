// tom-weatherhead/thaw-grammar/test/scheme.test.ts

'use strict';

import { LanguageSelector } from 'thaw-interpreter-types';

import { createParser, SyntaxException } from 'thaw-parser';

import { createGrammar, IExpression, ISExpression, PrimOp, SchemeGlobalInfo } from '../../..';

import { createFnRecognizer, createInfrastructure } from '../../create-infrastructure';

const ls = LanguageSelector.Scheme;

test('LL(1) Scheme parser instance creation test', () => {
	// Arrange
	const grammar = createGrammar(ls);

	// Act
	const parser = createParser(grammar.defaultParser, grammar);

	// Assert
	expect(parser).toBeTruthy();
});

test('LL(1) Scheme recognize test', () => {
	// Arrange
	const f = createFnRecognizer(ls);

	f('(* 7 13)');

	f('+');
	f('(lambda (x) (+ x 1))');
	f('(primop? +)');
	f('(closure? (lambda (x) (+ x 1)))');

	expect(() => f('(* 7 13')).toThrow(SyntaxException);
});

function defineTermRewritingSystem(globalInfo: SchemeGlobalInfo): void {
	// For Exercise 5.

	// See section 4.4 on pages 116-122
	globalInfo.loadPreset('set'); // For "member?"
	//globalInfo.LoadPreset("compose");

	// Functions from Figure 4.2 (on page 120)
	globalInfo.evaluateToString('(set fun-mod (lambda (f x y) (lambda (z) (if (= x z) y (f z)))))');
	globalInfo.evaluateToString("(set variable? (lambda (x) (member? x '(X Y))))");
	globalInfo.evaluateToString("(set empty-subst (lambda (x) 'unbound))");
	globalInfo.evaluateToString(
		[
			'(set mk-subst-fun',
			'(lambda (lhs e sigma)',
			'(if (variable? lhs)',
			"    (if (= (sigma lhs) 'unbound)",
			'        (fun-mod sigma lhs e)',
			"        (if (equal (sigma lhs) e) sigma 'nomatch))",
			'    (if (atom? lhs)',
			"        (if (= lhs e) sigma 'nomatch)",
			"        (if (atom? e) 'nomatch",
			'            (if (= (car lhs) (car e))',
			'                (mk-subst-fun* (cdr lhs) (cdr e) sigma)',
			"                'nomatch))))))"
		].join('\n')
	);
	globalInfo.evaluateToString(
		[
			'(set mk-subst-fun*',
			'(lambda (lhs-lis exp-lis sigma)',
			'(if (null? lhs-lis) sigma',
			'    (begin',
			'        (set car-match',
			'            (mk-subst-fun (car lhs-lis) (car exp-lis) sigma))',
			"        (if (= car-match 'nomatch) 'nomatch",
			'            (mk-subst-fun* (cdr lhs-lis) (cdr exp-lis) car-match))))))'
		].join('\n')
	);
	globalInfo.evaluateToString(
		[
			'(set extend-to-pat',
			'(lambda (sigma)',
			'(lambda (p)',
			"    (if (variable? p) (if (= (sigma p) 'unbound) p (sigma p))",
			'        (if (atom? p) p',
			'            (cons (car p)',
			'                (mapcar (extend-to-pat sigma) (cdr p))))))))'
		].join('\n')
	);

	// Function from Figure 4.3 (on page 121)
	globalInfo.evaluateToString(
		[
			'(set mk-toplvl-rw-fn',
			'(lambda (rule)',
			'(lambda (e)',
			'    (begin',
			'        (set induced-subst (mk-subst-fun (car rule) e empty-subst))',
			"        (if (= induced-subst 'nomatch) '()",
			'            ((extend-to-pat induced-subst) (cadr rule)))))))'
		].join('\n')
	);

	// Functions from Figure 4.4 (on page 122)
	globalInfo.evaluateToString(
		[
			'(set apply-inside-exp',
			'(lambda (f)',
			'(lambda (e)',
			'    (begin',
			'        (set newe (f e))',
			'        (if newe newe',
			"            (if (atom? e) '()",
			'                (begin',
			'                    (set newargs ((apply-inside-exp* f) (cdr e)))',
			"                    (if newargs (cons (car e) newargs) '()))))))))"
		].join('\n')
	);
	globalInfo.evaluateToString(
		[
			'(set apply-inside-exp*',
			'(lambda (f)',
			'(lambda (l)',
			"    (if (null? l) '()",
			'        (begin',
			'            (set newfirstarg ((apply-inside-exp f) (car l)))',
			'            (if newfirstarg',
			'                (cons newfirstarg (cdr l))',
			'                (begin',
			'                    (set newrestofargs ((apply-inside-exp* f) (cdr l)))',
			'                    (if newrestofargs',
			"                        (cons (car l) newrestofargs) '()))))))))"
		].join('\n')
	);
	globalInfo.evaluateToString('(set mk-rw-fn (compose mk-toplvl-rw-fn apply-inside-exp))');

	// Functions from Figure 4.4 (on page 122)
	globalInfo.evaluateToString("(set failure (lambda (e) '()))");
	globalInfo.evaluateToString(
		[
			'(set compose-rewrites (lambda (f g)',
			'(lambda (x)',
			'((lambda (fx) (if fx fx (g x))) (f x)))))'
		].join('\n')
	);
	globalInfo.evaluateToString('(set mk-rw-fn* (combine mk-rw-fn compose-rewrites failure))');
	globalInfo.evaluateToString(
		[
			'(set repeat-fn',
			'(lambda (f)',
			'(lambda (e)',
			'    (begin',
			'        (set tmp (f e))',
			'        (if tmp ((repeat-fn f) tmp) e)))))'
		].join('\n')
	);
	globalInfo.evaluateToString('(set compile-trs (compose mk-rw-fn* repeat-fn))');

	// Differentiation: from page 116
	globalInfo.evaluateToString(
		[
			"(set diff-rules '(",
			'((Dx x) 1)',
			'((Dx c) 0)',
			'((Dx (+ X Y)) (+ (Dx X) (Dx Y)))',
			'((Dx (- X Y)) (- (Dx X) (Dx Y)))',
			'((Dx (* X Y)) (+ (* Y (Dx X)) (* X (Dx Y))))',
			'((Dx (/ X Y)) (/ (- (* Y (Dx X)) (* X (Dx Y))) (* Y Y)))))'
		].join('\n')
	);
	globalInfo.evaluateToString('(set differentiate (compile-trs diff-rules))');
}

function evaluateToISExpression(input: string): ISExpression {
	const { tokenizer, parser } = createInfrastructure(ls);
	const globalInfo = new SchemeGlobalInfo({ tokenizer, parser });

	const parseResult = parser.parse(tokenizer.tokenize(input));
	const expr = parseResult as IExpression<ISExpression>;

	return expr.evaluate(globalInfo, globalInfo.globalEnvironment);
}

export function schemeTest(
	data: Array<[input: string, expectedResult: string | string[]]>,
	options: {
		presets?: string[];
		termRewritingSystem?: boolean;
	} = {}
): void {
	// Arrange
	const { tokenizer, parser } = createInfrastructure(ls);

	const schemeGlobalInfo = new SchemeGlobalInfo({ tokenizer, parser });

	schemeGlobalInfo.loadPresets();

	if (typeof options.presets !== 'undefined') {
		for (const preset of options.presets) {
			schemeGlobalInfo.loadPreset(preset);
		}
	}

	if (options.termRewritingSystem) {
		defineTermRewritingSystem(schemeGlobalInfo);
	}

	for (const [input, expectedResult] of data) {
		// Act
		const parseResult = parser.parse(tokenizer.tokenize(input));
		const expr = parseResult as IExpression<ISExpression>;
		const actualResult = expr
			.evaluate(schemeGlobalInfo, schemeGlobalInfo.globalEnvironment)
			.toString();

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

test('LL(1) Scheme PrimOp test 1', () => {
	const input = '+';
	const sexpr = evaluateToISExpression(input);

	expect(sexpr.isPrimOp()).toBe(true);

	const primOp = sexpr as PrimOp;

	expect(primOp.name.value).toBe(input);
});

test('LL(1) Scheme addition test 1', () => {
	schemeTest([['(+ 2 3)', '5']]);
});

test('LL(1) Scheme PrimOpTest2', () => {
	schemeTest([
		['(set add +)', '+'],
		['(add 2 3)', '5']
	]);
});

test('LL(1) Scheme closure test 1', () => {
	const input = '(lambda (x) (+ x 1))';
	const sexpr = evaluateToISExpression(input);

	expect(sexpr.isClosure()).toBe(true);
});

test('LL(1) Scheme closure test 2', () => {
	schemeTest([
		['(set increment (lambda (x) (+ x 1)))', '<closure>'],
		['(increment 13)', '14']
	]);
});

test('LL(1) Scheme ClosureTest3', () => {
	schemeTest([
		['(set add (lambda (x) (lambda (y) (+ x y))))', '<closure>'],
		['((add 8) 13)', '21']
	]);
});

test('LL(1) Scheme let test', () => {
	schemeTest([['(let ((m (* 3 4)) (n (+ 2 3))) (list m n))', '(12 5)']]);
});

test('LL(1) Scheme let* test', () => {
	schemeTest([['(let* ((x (+ 2 3)) (y (* x x))) y)', '25']]);
});

test('LL(1) Scheme let* non-recursive test', () => {
	// 2014/02/17 : Derived from Kamin page 126.

	// Assert that let* is not a clone of letrec.

	expect(() =>
		evaluateToISExpression(
			[
				'(let*',
				'((countones (lambda (l)',
				'(if (null? l) 0',
				'	(if (= (car l) 1) (+ 1 (countones (cdr l)))',
				'	(countones (cdr l)))))))',
				"(countones '(1 2 3 1 0 1 1 5)))"
			].join('\n')
		)
	).toThrow();
});

test('LL(1) Scheme letrec test', () => {
	schemeTest([
		[
			'(letrec ' +
				'((countones (lambda (l) ' +
				'(if (null? l) 0 ' +
				'(if (= (car l) 1) (+ 1 (countones (cdr l))) ' +
				'(countones (cdr l))))))) ' +
				"(countones '(1 2 3 1 0 1 1 5)))",
			'4'
		]
	]);
});

test('LL(1) Scheme cond test', () => {
	schemeTest([
		[
			"(set condtest (lambda (n) (cond ((= n 1) 'First) ((= n 2) 'Second) ((= n 3) 'Third) ('T 'Other))))",
			'<closure>'
		],
		['(condtest 0)', 'Other'],
		['(condtest 1)', 'First'],
		['(condtest 2)', 'Second'],
		['(condtest 3)', 'Third'],
		['(condtest 4)', 'Other']
	]);
});

test('LL(1) Scheme call/cc test', () => {
	// From Kamin page 128.
	schemeTest([
		['(set mod (lambda (m n) (- m (* n (/ m n)))))', '<closure>'],
		['(set gcd (lambda (m n) (if (= n 0) m (gcd n (mod m n)))))', '<closure>'],
		[
			'(set gcd* (lambda (l) ' +
				'(call/cc (lambda (exit) ' +
				'(letrec ((gcd*-aux (lambda (l) ' +
				'    (if (= (car l) 1) (exit 1) ' +
				'        (if (null? (cdr l)) (car l) ' +
				'            (gcd (car l) (gcd*-aux (cdr l)))))))) ' +
				'    (gcd*-aux l))))))',
			'<closure>'
		],
		["(gcd* '(9 27 81 60))", '3'],
		["(gcd* '(101 202 103))", '1'],
		["(gcd* '(9 27 1 81 60))", '1'],
		["(gcd* '(9 27 81 60 1 NotANumber))", '1']
	]);
});

test('LL(1) Scheme list test', () => {
	schemeTest([
		['(list)', '()'],
		['(list 1)', '(1)'],
		['(list 1 2 3)', '(1 2 3)'],
		["(list 1 + 'T)", '(1 + T)']
	]);
});

test('LL(1) Scheme static scope test', () => {
	// See page 135 of Kamin, or pages 128-137 for more context about static vs. dynamic scope.
	schemeTest([
		['(set add (lambda (x) (lambda (y) (+ x y))))', '<closure>'],
		['(set add1 (add 1))', '<closure>'],
		['(set f (lambda (x) (add1 x)))', '<closure>'],
		// Assert that our Scheme uses static scope, as Scheme should.
		['(f 5)', '6']
	]);
});

test('LL(1) Scheme Global vs. Local Variable test', () => {
	schemeTest([
		['(set a 1)', '1'],
		['(set afunc (lambda () a))', '<closure>'],
		['(set func2 (lambda (a) (afunc)))', '<closure>'],
		['(func2 0)', '1']
	]);
});

test('LL(1) Scheme PrimOp and Closure Pred test', () => {
	schemeTest([
		['(set add +)', '+'],
		['(set add1 (lambda (x) (+ x 1)))', '<closure>'],

		['(primop? +)', 'T'],
		['(primop? add)', 'T'],
		['(primop? add1)', '()'],

		['(closure? +)', '()'],
		['(closure? add)', '()'],
		['(closure? add1)', 'T'],

		['(primop? list)', 'T'],

		// Just for fun:
		['(primop? primop?)', 'T'],
		['(primop? closure?)', 'T'],
		['(closure? primop?)', '()'],
		['(closure? closure?)', '()']
	]);
});

test('LL(1) Scheme Streams test', () => {
	// See Kamin pages 176-178 : "SASL vs. Scheme"
	// This Scheme code uses zero-argument closures to mimic SASL thunks.
	// If s is a stream, (car s) is a number, and ((cadr s)) is a stream.
	const line0 = '(set cadr (lambda (x) (car (cdr x))))';
	const line1 =
		'(set add-streams (lambda (s1 s2)' +
		'(list (+ (car s1) (car s2)) (lambda () (add-streams ((cadr s1)) ((cadr s2)))))' +
		'))';
	const line2 =
		'(set stream-first-n (lambda (n s)' +
		"(if (= n 0) '()" +
		'(cons (car s) (stream-first-n (- n 1) ((cadr s)))))' +
		'))';
	const line3 = '(set powers-of-2 (list 1 (lambda () (add-streams powers-of-2 powers-of-2))))';
	const line4 =
		'(set fibonacci (list 0 (lambda () (list 1 (lambda () (add-streams fibonacci ((cadr fibonacci))))))))';

	schemeTest([
		[line0, '<closure>'],
		[line1, '<closure>'],
		[line2, '<closure>'],
		[line3, '(1 <closure>)'],
		[line4, '(0 <closure>)'],
		['(stream-first-n 5 powers-of-2)', '(1 2 4 8 16)'],
		['(stream-first-n 8 fibonacci)', '(0 1 1 2 3 5 8 13)']
	]);
});

test('LL(1) Scheme Rplaca and Rplacd test', () => {
	// See page 55

	schemeTest([
		["(set x '(a b c))", '(a b c)'],
		['(set y x)', '(a b c)'],
		["(rplaca y 'd)", 'd'],
		['y', '(d b c)'],
		['x', '(d b c)'],
		["(rplacd y 'e)", 'e'],
		['y', '(d . e)'],
		['x', '(d . e)']
	]);
});

// test('Scheme Macro test', () => {
// 	// From pages 56-57, and Exercise 12, from pages 62-63 (in the LISP chapter)
//
// 	Evaluate("(set <= (lambda (x y) (or (< x y) (= x y))))");
//
// 	Evaluate([
// '(define-macro for (indexvar lower upper body)',
// '(list \'begin',
// '(list \'set indexvar lower)',
// '(list \'while',
// '	(list \'<= indexvar upper)',
// '	(list \'begin body',
// '		(list \'set indexvar (list \'+ indexvar 1))))))'
// 	].join('\n'));
//
// 	Evaluate("(set sum 0)");
//
// 	Evaluate("(for x 1 10 (set sum (+ sum x)))");
//
// 	Assert.AreEqual("55", Evaluate("sum"));
// });

test('Scheme Random test', () => {
	const maxValue = 100;
	const sexpr = evaluateToISExpression(`(random ${maxValue})`);

	expect(sexpr.isNumber()).toBe(true);

	const n = Number.parseInt(sexpr.toString());

	expect(Number.isNaN(n)).toBe(false);
	expect(n >= 0).toBe(true);
	expect(n < maxValue).toBe(true);
});

test('Scheme Sets test', () => {
	// // See pages 104-105
	schemeTest(
		[
			["(set s1 (addelt 'a (addelt 'b nullset)))", '(a b)'],
			["(member? 'a s1)", 'T'],
			["(member? 'c s1)", '()'],
			["(set s2 (addelt 'b (addelt 'c nullset)))", '(b c)'],
			['(set s3 (union s1 s2))', '(c a b)']
		],
		{ presets: ['set'] }
	);
});

// [Test]
// public void LetMacroTest()  // Part of exercise 15 on page 152.
// {
//     Evaluate("(set list-of-cars (lambda (l) (mapcar car l)))");
//     Evaluate("(set list-of-cadrs (lambda (l) (mapcar cadr l)))");
//     Evaluate(@"
// (define-macro letm (declarations body)
// (cons
// (list 'lambda (list-of-cars declarations) body)
// (list-of-cadrs declarations)))");
//     Assert.AreEqual("(12 5)", Evaluate("(letm ((m (* 3 4)) (n (+ 2 3))) (list m n))"));
// }

// [Test]
// public void LetStarMacroTest()  // Part of exercise 15 on page 152.
// {
//     Evaluate(@"
// (set build-expr
// (lambda (declarations body)
// (if (null? declarations) body
//     (list
//         (list 'lambda
//             (list (car (car declarations)))
//             (build-expr (cdr declarations) body))
//         (cadr (car declarations))))))");
//     Evaluate("(define-macro let*m (declarations body) (build-expr declarations body))");
//     Assert.AreEqual("25", Evaluate("(let*m ((x (+ 2 3)) (y (* x x))) y)"));
// }

// [Test]
// public void LetRecMacroTest()  // Part of exercise 15 on page 152.
// {
//     Evaluate(@"
// (set build-let-declaration
// (lambda (declaration)
// (list (car declaration) 0)))");
//     Evaluate(@"
// (set build-set-statement
// (lambda (declaration)
// (cons 'set declaration)))");
//     Evaluate(@"
// (define-macro letrecm (declarations body)
// (list 'let (mapcar build-let-declaration declarations)
// (cons 'begin
//     (append
//         (mapcar build-set-statement declarations)
//         (list body)))))");
//
//     /*
//     Assert.AreEqual("4", Evaluate(@"
// (letrecm
// ((countones (lambda (l)
// (if (null? l) 0
//     (if (= (car l) 1) (+ 1 (countones (cdr l)))
//         (countones (cdr l)))))))
// (countones (quote (1 2 3 1 0 1 1 5))))"));
//      */
//     Assert.AreEqual("4", Evaluate(@"
// (letrecm
// ((countones (lambda (l)
// (if (null? l) 0
//     (if (= (car l) 1) (+ 1 (countones (cdr l)))
//         (countones (cdr l)))))))
// (countones '(1 2 3 1 0 1 1 5)))"));
// }

// [Test]
// public void MacroApostrophesToQuoteKeywordsTest()
// {
//     // Note that these expressions are parsed, but not evaluated.
//     Assert.AreEqual("(lambda (foo) (quote bar))", MacroDefinition.ObjectToString_ApostrophesToQuoteKeywords(GetParseResult("(lambda (foo) 'bar)")));
//     Assert.AreEqual("(foo (quote bar) (quote baz))", MacroDefinition.ObjectToString_ApostrophesToQuoteKeywords(GetParseResult("(foo 'bar 'baz)")));
//     Assert.AreEqual("((lambda (foo) (quote bar)) (quote baz))", MacroDefinition.ObjectToString_ApostrophesToQuoteKeywords(GetParseResult("((lambda (foo) 'bar) 'baz)")));
//     Assert.AreEqual("(letrec ((foo (quote bar))) (quote baz))", MacroDefinition.ObjectToString_ApostrophesToQuoteKeywords(GetParseResult("(letrec ((foo 'bar)) 'baz)")));
//     Assert.AreEqual("(call/cc (lambda (foo) (foo (quote bar))))", MacroDefinition.ObjectToString_ApostrophesToQuoteKeywords(GetParseResult("(call/cc (lambda (foo) (foo 'bar)))")));
// }

// [Test]
// public void ComposeListTest() // 2013/11/30
// {
//     Evaluate("(set compose-list (combine id compose id))");
//     Evaluate("(set cadaddr (compose-list (list cdr cdr car cdr car)))");
//
//     Assert.AreEqual("10", Evaluate("(cadaddr '((1 2 3 4) (5 6 7 8) (9 10 11 12) (13 14 15 16)))"));
//
//     // 2013/12/02
//     Evaluate("(set compose-list-reverse (combine id (reverse2args compose) id))");
//     Evaluate("(set cadaddr (compose-list-reverse (list car cdr car cdr cdr)))");    // The functions are applied from right to left.
//
//     Assert.AreEqual("10", Evaluate("(cadaddr '((1 2 3 4) (5 6 7 8) (9 10 11 12) (13 14 15 16)))"));
//
//     Evaluate("(set sumplus3 (compose2args + (compose-list (list +1 +1 +1))))");
//
//     Assert.AreEqual("18", Evaluate("(sumplus3 7 8)"));
// }

// [Test]
// public void GeneralFindTest() // 2013/12/03
// {
//     Evaluate(@"
// (set general-find (lambda (pred result zero)
// (letrec
// ((loop
//     (lambda (l)
//         (cond
//             ((null? l) zero)
//             ((pred (car l)) (result (car l)))
//             ('T (loop (cdr l)))
//         )
//     )
// ))
// loop
// )
// ))");
//     Evaluate(@"
// (set original-find (lambda (pred lis)
// (
// (general-find
//     pred
//     (lambda (x) 'T)
//     '()
// )
// lis
// )
// ))");
//     Evaluate("(set original-contains (lambda (x l) (original-find ((curry =) x) l)))");
//
//     Assert.AreEqual("T", Evaluate("(original-contains 5 '(2 3 5 7))"));
//     Assert.AreEqual("()", Evaluate("(original-contains 4 '(2 3 5 7))"));
//
//     Evaluate(@"
// (set alist-alt (lambda (x alist)
// (
// (general-find
//     (compose car ((curry =) x))
//     cadr
//     '()
// )
// alist
// )
// ))");
//     Evaluate("(set sample-alist '((2 11) (3 13) (5 19) (7 19)))");
//
//     Assert.AreEqual("13", Evaluate("(alist-alt 3 sample-alist)"));
//     Assert.AreEqual("()", Evaluate("(alist-alt 4 sample-alist)"));
// }

// [Test]
// public void SyntaxExceptionTest() // 2013/12/12
// {
//     InferenceAssert.ThrowsWithLineAndColumnNumbers<SyntaxException>(() => Evaluate("(if 'T 7 13 'SyntaxError)"), 1, 13);
// }

// [Test]
// public void EvaluationExceptionTest() // 2013/12/12
// {
//     InferenceAssert.ThrowsWithLineAndColumnNumbers<EvaluationException>(() => Evaluate("(car 7)"), 1, 2);
// }

test('Scheme string< test', () => {
	// 2013/12/14

	schemeTest([
		['(primop? string<)', 'T'],
		['(string< "a" "a")', '()'],
		['(string< "a" "b")', 'T'],
		['(string< "b" "a")', '()'],
		['(string< "abac" "abacus")', 'T'],
		['(string< "abacab" "abacus")', 'T']
	]);
});

// [Test]
// public void StringSortTest()    // 2013/12/14
// {
//     globalInfo.LoadPreset("sort");
//
//     // 1) Insertion sort.
//     Assert.AreEqual("(a ab abacab abacus abbot abbreviate abcess baa)",
//         Evaluate("((insertion-sort string<) '(\"abbreviate\" \"abacab\" \"abbot\" \"a\" \"baa\" \"abcess\" \"ab\" \"abacus\"))"));
//
//     // 2) Quicksort.
//     Assert.AreEqual("(a ab abacab abacus abbot abbreviate abcess baa)",
//         Evaluate("((quicksort string<) '(\"abbreviate\" \"abacab\" \"abbot\" \"a\" \"baa\" \"abcess\" \"ab\" \"abacus\"))"));
//
//     // 3) Merge sort.
//     Assert.AreEqual("(a ab abacab abacus abbot abbreviate abcess baa)",
//         Evaluate("((merge-sort string<) '(\"abbreviate\" \"abacab\" \"abbot\" \"a\" \"baa\" \"abcess\" \"ab\" \"abacus\"))"));
// }

// [Test]
// public void RepeatListTest()    // 2014/02/15.  This might be useful in "restruct" in our Scheme APL interpreter.
// {
//     Evaluate(@"
// (set repeat-list (lambda (n master)
// (letrec
// ((loop (lambda (n lm)
//     (if (<= n lm)
//         (take n master) ; Verify the order of take's args
//         (append master (loop (- n lm) lm))
//     )
// )))
// (loop n (length master))
// )
// ))");
//
//     Assert.AreEqual("(2 3 5 7 2 3 5 7 2 3 5)", Evaluate("(repeat-list 11 '(2 3 5 7))"));
// }
