// tom-weatherhead/thaw-parser/test/scheme-exercises.test.ts

'use strict';

// import { LanguageSelector } from 'thaw-interpreter-types';

// import { createTokenizer } from 'thaw-lexical-analyzer';

// import { IGlobalInfoForInterpreter } from '../../..';

// import { createFnRecognizer, createInfrastructure } from '../../create-infrastructure';

import { schemeTest } from './scheme.test';

// const ls = LanguageSelector.Scheme;

test('LL(1) Scheme Exercise 1', () => {
	// Exercise 1 on pages 148-149.

	schemeTest([
		// Exercise 1a) : cdr*
		['(set cdr* (mapc cdr))', '<closure>'],
		["(cdr* '((a b c) (d e) (f)))", '((b c) (e) ())'],

		// Exercise 1b) : max*
		['(set max (lambda (x y) (if (> x y) x y)))', '<closure>'],
		['(set max* (combine id max 0))', '<closure>'],
		["(max* '(1 5 10 3 7 2 8))", '10'],

		// Exercise 1c) : append (although we will call it append2 here)
		['(set append2 (lambda (l1 l2) ((combine id cons l2) l1)))', '<closure>'],
		["(append2 '(a b c) '(d e f g))", '(a b c d e f g)'],

		// Exercise 1d) : addtoend
		['(set addtoend (lambda (x l) ((combine id cons (list x)) l)))', '<closure>'],
		["(addtoend 'a '(b c d))", '(b c d a)'],

		// Exercise 1e) : reverse (although we will call it reverse2 here)
		["(set reverse2 (combine id addtoend '()))", '<closure>'],
		["(reverse2 '(a b c d e f g))", '(g f e d c b a)'],

		// Exercise 1f) : insertion-sort
		[
			[
				'(set insert (lambda (x l)',
				'(cond',
				'((null? l) (list x))',
				'((<= x (car l)) (cons x l))',
				"('T (cons (car l) (insert x (cdr l)))))))"
			].join('\n'),
			'<closure>'
		],

		["(set insertion-sort (combine id insert '()))", '<closure>'],
		["(insertion-sort '(3 7 4 1 2 6 5))", '(1 2 3 4 5 6 7)'],

		// Exercise 1g) : mkpairsfn
		['(set mkpairsfn (lambda (x) (mapc (lambda (l) (cons x l)))))', '<closure>'],
		["((mkpairsfn 'a) '(() (b c) (d) ((e f))))", '((a) (a b c) (a d) (a (e f)))']
	]);
});

test('LL(1) Scheme Exercise 2', () => {
	// Exercise 2 on page 149 : lex-order*

	schemeTest([
		[
			[
				'(set lex-order* (lambda (cmp)',
				'	(lambda (l1 l2)',
				'		(cond',
				'			((null? l1) (not (null? l2)))',
				"			((null? l2) '())",
				"			((cmp (car l1) (car l2)) 'T)",
				"			((cmp (car l2) (car l1)) '())",
				"			('T ((lex-order* cmp) (cdr l1) (cdr l2)))))))"
			].join('\n'),
			'<closure>'
		],
		['(set alpha-order (lex-order* <))', '<closure>'],
		["(alpha-order '(4 15 7) '(4 15 7 5))", 'T'],
		["(alpha-order '(4 15 7) '(4 15 6 6))", '()']
	]);
});

// [Test]
// public void Exercise3Test() // Exercise 3 on page 149 : Sets implemented using characteristic functions
// {
//     Evaluate("(set nullset (lambda (x) '()))");
//     Evaluate("(set member? (lambda (x s) (s x)))");
//     // mk-set-ops : See pages 106-107
//     Evaluate(@"
// (set mk-set-ops (lambda (eqfun)
// (cons (lambda (x s) (if (member? x s) s (lambda (y) (or (eqfun x y) (member? y s))))) ; addelt
// '())))");
//     Evaluate("(set addelt (car (mk-set-ops =)))");
//     Evaluate(@"
// (set union (lambda (s1 s2)
// (lambda (x) (or (member? x s1) (member? x s2)))))");
//     Evaluate(@"
// (set inter (lambda (s1 s2)
// (lambda (x) (and (member? x s1) (member? x s2)))))");
//     Evaluate(@"
// (set diff (lambda (s1 s2)
// (lambda (x) (and (member? x s1) (not (member? x s2))))))");
//
//     Evaluate("(set s1 (addelt 'a (addelt 'b nullset)))");
//     Assert.AreEqual("T", Evaluate("(member? 'a s1)"));
//     Assert.AreEqual("T", Evaluate("(member? 'b s1)"));
//     Assert.AreEqual("()", Evaluate("(member? 'c s1)"));
//
//     Evaluate("(set s2 (addelt 'b (addelt 'c nullset)))");
//     Evaluate("(set s3 (union s1 s2))");
//     Assert.AreEqual("T", Evaluate("(member? 'a s3)"));
//     Assert.AreEqual("T", Evaluate("(member? 'b s3)"));
//     Assert.AreEqual("T", Evaluate("(member? 'c s3)"));
//     Assert.AreEqual("()", Evaluate("(member? 'd s3)"));
//
//     Evaluate("(set s4 (inter s1 s2))");
//     Assert.AreEqual("()", Evaluate("(member? 'a s4)"));
//     Assert.AreEqual("T", Evaluate("(member? 'b s4)"));
//     Assert.AreEqual("()", Evaluate("(member? 'c s4)"));
//
//     Evaluate("(set s5 (diff s1 s2))");
//     Assert.AreEqual("T", Evaluate("(member? 'a s5)"));
//     Assert.AreEqual("()", Evaluate("(member? 'b s5)"));
//     Assert.AreEqual("()", Evaluate("(member? 'c s5)"));
// }

// [Test]
// public void Exercise4Test() // Exercise 4 on page 149 : Optimizing gcd* and gcds (from pages 108-109)
// {
//     // Part 1
//     Evaluate("(set gcd* (lambda (l) (gcd*-aux l id)))");
//     Evaluate(@"
// (set gcd*-aux (lambda (l f)
// (if (= (car l) 1) 1
// (if (null? (cdr l)) (f (car l))
//     (gcd*-aux (cdr l)
//         (lambda (n)
//             (let ((gcd-value (gcd (car l) n)))
//                 (if (= gcd-value 1) 1 (f gcd-value)))))))))");
//
//     Assert.AreEqual("7", Evaluate("(gcd* '(14 49 98))"));
//     Assert.AreEqual("1", Evaluate("(gcd* '(3 5 7 9 11))"));
//     Assert.AreEqual("1", Evaluate("(gcd* '(NotANumber 3 5))"));    // The gcd is calculated from the arguments from right to left.
//
//     // Part 2
//     Evaluate("(set gcds (lambda (s) (gcds-aux s id)))");
//     Evaluate(@"
// (set gcds-aux (lambda (s f)
// (if (number? s) (if (= s 1) 1 (f s))
// (if (null? (cdr s))
//     (gcds-aux (car s) f)
//     (gcds-aux (car s)
//         (lambda (n) (gcds-aux (cdr s)
//             (lambda (p)
//                 (let ((gcd-value (gcd n p)))
//                     (if (= gcd-value 1) 1 (f (gcd n p))))))))))))");
//
//     Assert.AreEqual("7", Evaluate("(gcds '((14 (49 98)) 56 ((84 105 21) 91 77)))"));
//     Assert.AreEqual("1", Evaluate("(gcds '((3 5) 7 (9 11)))"));
//     //Assert.AreEqual("1", Evaluate("(gcds '((NotANumber 3) 5))")); // gcds only accepts S-expressions of numbers and pairs (lists), not symbols.
// }

test('LL(1) Scheme Term Rewriting Systems test', () => {
	// See section 4.4 on pages 116-122

	schemeTest([["(differentiate '(Dx (+ x c)))", '(+ 1 0)']], { termRewritingSystem: true });
});

test('LL(1) Scheme Exercise 5a', () => {
	// Exercise 5a on pages 150-151; TermRewritingSystems

	// defineTermRewritingSystem();

	// Old code; from the text.
	//Evaluate("(set mk-rw-fn* (combine mk-rw-fn compose-rewrites failure))");

	// New code.
	// Evaluate("(set mk-toplvl-rw-fn* (combine mk-toplvl-rw-fn compose-rewrites failure))"); // Apply any of the rules at the top level of an expression.
	// Evaluate("(set mk-rw-fn* (compose mk-toplvl-rw-fn* apply-inside-exp))"); // Extend the above to operate inside expressions.
	//
	// Assert.AreEqual("(+ 1 0)", Evaluate("(differentiate '(Dx (+ x c)))"));

	schemeTest(
		[
			// Apply any of the rules at the top level of an expression.
			[
				'(set mk-toplvl-rw-fn* (combine mk-toplvl-rw-fn compose-rewrites failure))',
				'<closure>'
			],

			// Extend the above to operate inside expressions.
			['(set mk-rw-fn* (compose mk-toplvl-rw-fn* apply-inside-exp))', '<closure>'],

			["(differentiate '(Dx (+ x c)))", '(+ 1 0)']
		],
		{ termRewritingSystem: true }
	);
});

// [Test]
// public void Exercise5bTest()  // Exercise 5b on pages 150-151; TermRewritingSystems; modifications to apply-inside-exp
// {
//     DefineTermRewritingSystem();
//
//     // The next two functions are new or rewritten.
//     Evaluate(@"
// (set apply-func
// (lambda (f)
// (lambda (x)
//     (let ((result ((apply-inside-exp f) x)))
//         (if result result x)))))");
//     Evaluate(@"
// (set apply-inside-exp*
// (lambda (f)
// (lambda (l)
//     (let ((result (mapcar (apply-func f) l)))
//         (if (equal result l) '() result)))))");
//
//     Assert.AreEqual("(+ 1 0)", Evaluate("(differentiate '(Dx (+ x c)))"));
// }
