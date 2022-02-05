// tom-weatherhead/thaw-parser/test/scheme-eval.test.ts

// A Scheme interpreter written in Scheme.

'use strict';

import { LanguageSelector } from 'thaw-interpreter-types';

import { SchemeGlobalInfo } from '../../..';

import { createInfrastructure } from '../../create-infrastructure';

test('LL(1) Scheme Eval test', () => {
	// From section 4.5, on pages 123-124.  Also part of exercise 17 on page 152.

	// Arrange
	const { tokenizer, parser } = createInfrastructure(LanguageSelector.Scheme);
	const globalInfo = new SchemeGlobalInfo({ tokenizer, parser });

	globalInfo.initialize(); // Calls loadSASLSafePresets(), which defines compose, etc.

	globalInfo.loadPreset('assoc');
	globalInfo.loadPreset('select');

	globalInfo.evaluate('(set caddr (lambda (l) (cadr (cdr l))))');
	globalInfo.evaluate('(set cadddr (lambda (l) (caddr (cdr l))))');

	expect(0).toBe(0); // A placeholder test

	// Functions adapted from page 48

	globalInfo.evaluate(
		[
			'(set apply-binary-op (lambda (f x y)',
			'	(cond',
			"		((= f 'cons) (cons x y))",
			"		((= f '+) (+ x y))",
			"		((= f '-) (- x y))",
			"		((= f '*) (* x y))",
			"		((= f '/) (/ x y))",
			"		((= f '<) (< x y))",
			"		((= f '>) (> x y))",
			"		((= f '=) (= x y))",
			"		('T 'binary-op-error!))))"
		].join('\n')
	);

	globalInfo.evaluate(
		[
			'(set apply-unary-op (lambda (f x)',
			'	(cond',
			"		((= f 'car) (car x))",
			"		((= f 'cdr) (cdr x))",
			"		((= f 'number?) (number? x))",
			"		((= f 'list?) (list? x))",
			"		((= f 'symbol?) (symbol? x))",
			"		((= f 'null?) (null? x))",
			"		((= f 'closure?) (is-closure? x))",
			"		((= f 'primop?) (is-primop? x))",
			"		('T 'unary-op-error!)",
			')))'
		].join('\n')
	);

	// From page 123
	globalInfo.evaluate('(set formals (lambda (lamexp) (cadr lamexp)))');
	globalInfo.evaluate('(set body (lambda (lamexp) (caddr lamexp)))');
	globalInfo.evaluate('(set funpart (lambda (clo) (cadr clo)))');
	globalInfo.evaluate('(set envpart (lambda (clo) (caddr clo)))');

	// begin
	globalInfo.evaluate(
		[
			'(set do-begin (lambda (expr-list rho)',
			'	(if (null? (cdr expr-list))',
			'		(eval (car expr-list) rho)',
			'		(begin',
			'			(eval (car expr-list) rho)',
			'			(do-begin (cdr expr-list) rho)))))'
		].join('\n')
	);

	// let
	globalInfo.evaluate('(set construct-let-var-list (mapc car))');
	globalInfo.evaluate('(set construct-let-expr-list (mapc cadr))');
	globalInfo.evaluate(
		[
			'(set do-let (lambda (var-expr-list expr rho)',
			'	(eval',
			'		(cons',
			"			(list 'lambda (construct-let-var-list var-expr-list) expr)",
			'			(construct-let-expr-list var-expr-list)',
			'		)',
			'		rho',
			')))'
		].join('\n')
	);

	// let*
	globalInfo.evaluate(
		[
			'(set construct-let* (lambda (var-expr-list expr)',
			'	(if (null? var-expr-list) expr',
			'		(list',
			'			(list',
			"				'lambda",
			'				(list (caar var-expr-list))',
			'				(construct-let* (cdr var-expr-list) expr)',
			'			)',
			'			(cadar var-expr-list)',
			'))))'
		].join('\n')
	);

	globalInfo.evaluate(
		[
			'(set do-let* (lambda (var-expr-list expr rho)',
			'	(eval (construct-let* var-expr-list expr) rho)))'
		].join('\n')
	);

	// letrec
	globalInfo.evaluate(
		[
			'(set construct-letrec-let-body (lambda (var-expr-list)',
			"	(if (null? var-expr-list) '()",
			'		(cons',
			'			(list (caar var-expr-list) 0)',
			'			(construct-letrec-let-body (cdr var-expr-list))))))'
		].join('\n')
	);

	globalInfo.evaluate(
		[
			'(set construct-letrec-begin-body (lambda (var-expr-list expr)',
			'	(if (null? var-expr-list) (list expr)',
			'		(cons',
			"			(cons 'set (car var-expr-list))",
			'			(construct-letrec-begin-body (cdr var-expr-list) expr)))))'
		].join('\n')
	);

	globalInfo.evaluate(
		[
			'(set construct-letrec (lambda (var-expr-list expr)',
			"	(list 'let (construct-letrec-let-body var-expr-list)",
			"		(cons 'begin (construct-letrec-begin-body var-expr-list expr)))))"
		].join('\n')
	);

	globalInfo.evaluate(
		[
			'(set do-letrec (lambda (var-expr-list expr rho)',
			'	(eval (construct-letrec var-expr-list expr) rho)))'
		].join('\n')
	);

	// cond
	globalInfo.evaluate(
		[
			'(set do-cond (lambda (expr-pair-list rho)',
			"	(if (null? expr-pair-list) '()",
			'		(if (eval (caar expr-pair-list) rho)',
			'			(eval (cadar expr-pair-list) rho)',
			'			(do-cond (cdr expr-pair-list) rho)))))'
		].join('\n')
	);

	// Functions from Figure 4.6 on page 124
	globalInfo.evaluate(
		[
			'(set eval (lambda (expr env)',
			'	(cond',
			'		((number? expr) expr)',
			'		((symbol? expr)',
			'			(if (assoc-contains-key expr env)',
			'				(assoc expr env)',
			'				(assoc expr global-environment)))',
			"		((= (car expr) 'quote) (cadr expr))",
			"		((= (car expr) 'if)",
			'			(if (null? (eval (cadr expr) env))',
			'				(eval (cadddr expr) env)',
			'				(eval (caddr expr) env)))',
			"		((= (car expr) 'begin) (do-begin (cdr expr) env)) ; Exercise 6a) on page 61",
			"		((= (car expr) 'print) ; Exercise 6a) on page 61",
			'			(print (eval (cadr expr) env)))',
			"		((= (car expr) 'set)",
			'			(let ((evaluated-expression (eval (caddr expr) env)))',
			'				(if (assoc-contains-key (cadr expr) env)',
			'					(begin',
			'						(rplac-assoc (cadr expr) evaluated-expression env)',
			'						evaluated-expression)',
			'					(begin',
			'						(set global-environment (mkassoc (cadr expr) evaluated-expression global-environment))',
			'						evaluated-expression))))',
			"		((= (car expr) 'let) (do-let (cadr expr) (caddr expr) env))",
			"		((= (car expr) 'let*) (do-let* (cadr expr) (caddr expr) env))",
			"		((= (car expr) 'letrec) (do-letrec (cadr expr) (caddr expr) env))",
			"		((= (car expr) 'cond) (do-cond (cdr expr) env))",
			"		((= (car expr) 'lambda) (list 'closure expr env))",
			"		((= (car expr) 'list) (evallist (cdr expr) env))",
			"		('T (apply (evallist expr env) env))",
			')))'
		].join('\n')
	);

	globalInfo.evaluate(
		[
			'(set evallist (lambda (el rho)',
			"	(if (null? el) '()",
			'		(cons',
			'			(eval (car el) rho)',
			'			(evallist (cdr el) rho)))))'
		].join('\n')
	);

	globalInfo.evaluate(
		[
			'(set mkassoc* (lambda (keys values al)',
			'	(if (null? keys) al',
			'		(mkassoc* (cdr keys) (cdr values)',
			'			(mkassoc (car keys) (car values) al)))))'
		].join('\n')
	);

	globalInfo.evaluate(
		[
			'(set apply (lambda (el env)',
			'	(if (is-closure? (car el))',
			'		(apply-closure (car el) (cdr el))',
			'		(apply-value-op (car el) (cdr el)))))'
		].join('\n')
	);

	globalInfo.evaluate(
		[
			'(set apply-closure (lambda (clo args)',
			'	(eval (body (funpart clo))',
			'		(mkassoc* (formals (funpart clo)) args (envpart clo)))))'
		].join('\n')
	);

	globalInfo.evaluate(
		[
			'(set apply-value-op (lambda (primop args)',
			'	(if (= (length args) 1)',
			'		(apply-unary-op (cadr primop) (car args))',
			'		(apply-binary-op (cadr primop) (car args) (cadr args)))))'
		].join('\n')
	);

	globalInfo.evaluate("(set is-closure? (lambda (f) (= (car f) 'closure)))");

	globalInfo.evaluate("(set is-primop? (lambda (f) (= (car f) 'primop)))");

	globalInfo.evaluate(
		[
			"(set valueops '(",
			'(+ (primop +))',
			'(- (primop -))',
			'(cons (primop cons))',
			'(* (primop *))',
			'(/ (primop /))',
			'(< (primop <))',
			'(> (primop >))',
			'(= (primop =))',
			'(cdr (primop cdr))',
			'(car (primop car))',
			'(number? (primop number?))',
			'(list? (primop list?))',
			'(symbol? (primop symbol?))',
			'(null? (primop null?))',
			'(closure? (primop closure?))',
			'(primop? (primop primop?))))'
		].join('\n')
	);

	// Functions adapted from Figure 2.8
	globalInfo.evaluate(
		[
			'(set r-e-p-loop (lambda (inputs)',
			'	(begin',
			"		(set global-environment '())",
			'		(r-e-p-loop* inputs))))'
		].join('\n')
	);

	globalInfo.evaluate(
		[
			'(set r-e-p-loop* (lambda (inputs)',
			"	(if (null? inputs) '()",
			'	(process-expr (car inputs) (cdr inputs)))))'
		].join('\n')
	);

	globalInfo.evaluate(
		[
			'(set process-expr (lambda (e inputs)',
			'	(cons (eval e valueops) ; print value of expression',
			'	(r-e-p-loop* inputs))))'
		].join('\n')
	);

	expect(globalInfo.evaluateToString("(eval '(+ 2 3) valueops)")).toBe('5');

	// Test from page 123
	globalInfo.evaluate("(set E (mkassoc 'double (eval '(lambda (a) (+ a a)) valueops) valueops))");
	expect(globalInfo.evaluateToString("(eval '(double 4) E)")).toBe('8');

	// select test
	expect(globalInfo.evaluateToString("(select '(1 3 4) '(10 12 14 16 18 20))")).toBe(
		'(12 16 18)'
	);

	// Test of "set" to ensure that we have completed the exercise.
	expect(
		globalInfo.evaluateToString(
			[
				"(select '(1 2 3) (r-e-p-loop '( ; We use 'select' because we don't want to test the value of the closure 'double'.",
				'(set double (lambda (a) (+ a a)))',
				'(double 4)',
				'(primop? double)',
				'(closure? double)',
				')))'
			].join('\n')
		)
	).toBe('(8 () T)');

	// letrec test: from Kamin page 126.
	/*
	Assert.AreEqual("(4)", Evaluate(@"
(r-e-p-loop '(
(letrec ; Try a letrec wth two bindings
(
	(countzeroes (lambda (l)
		(if (null? l) 0
			(if (= (car l) 0) (+ 1 (countzeroes (cdr l)))
				(countzeroes (cdr l))))))
	(countones (lambda (l)
		(if (null? l) 0
			(if (= (car l) 1) (+ 1 (countones (cdr l)))
				(countones (cdr l))))))
)
(countones (quote (1 2 3 1 0 1 1 5))))
))"));
	*/

	expect(
		globalInfo.evaluateToString(
			[
				'(r-e-p-loop (list',
				"	(list 'letrec ; Try a letrec wth two bindings",
				"		'(",
				'			(countzeroes (lambda (l)',
				'				(if (null? l) 0',
				'					(if (= (car l) 0) (+ 1 (countzeroes (cdr l)))',
				'					(countzeroes (cdr l))))))',
				'		(countones (lambda (l)',
				'			(if (null? l) 0',
				'				(if (= (car l) 1) (+ 1 (countones (cdr l)))',
				'					(countones (cdr l))))))',
				'		)',
				"		(list 'countones (list 'quote '(1 2 3 1 0 1 1 5))))",
				'))'
			].join('\n')
		)
	).toBe('(4)');

	// Test of letrec (see exercise 6 on page 150)
	expect(
		globalInfo.evaluateToString(
			[
				"(select '(1 2 3) (r-e-p-loop (list",
				"	(list 'set 'eval-ex6 (list 'lambda '(e)",
				"		(list 'letrec",
				'			(list',
				"				'(combine (lambda (f sum zero) (lambda (l) (if (null? l) zero (sum (f (car l)) ((combine f sum zero) (cdr l)))))))",
				"				'(id (lambda (x) x))",
				"				'(+/ (combine id + 0))",
				"				'(*/ (combine id * 1))",
				"				(list 'ev (list 'lambda '(expr)",
				"					(list 'if '(number? expr) 'expr",
				"						(list 'if (list '= '(car expr) (list 'quote '+))",
				"							'(+/ (evlis (cdr expr)))",
				"							'(*/ (evlis (cdr expr)))))))",
				"	'(mapcar (lambda (f l) (if (null? l) l (cons (f (car l)) (mapcar f (cdr l))))))",
				"	'(curry (lambda (f) (lambda (x) (lambda (y) (f x y)))))",
				"	'(mapc (curry mapcar))",
				"	'(evlis (mapc ev))",
				'	)',
				"	'(ev e))))",
				"	(list 'eval-ex6 (list 'quote '(+ 1 2 3 4 5)))",
				"	(list 'eval-ex6 (list 'quote '(* 1 2 3 4 5)))",
				"	(list 'eval-ex6 (list 'quote '(* (+ 1 2 3) (+ 4 5))))",
				')))'
			].join('\n')
		)
	).toBe('(15 120 54)');

	// Note: If you get an error saying that car's argument is null, it is probably because you forgot to declare something
	// (e.g. a function like id or mapc).

	// "list" tests.
	expect(
		globalInfo.evaluateToString(
			[
				"(r-e-p-loop '(",
				'	(list 2 3 5 7)',
				'	(list (+ 0 1) (+ 1 1) (+ 1 2) (+ 2 3))',
				'))'
			].join('\n')
		)
	).toBe('((2 3 5 7) (1 2 3 5))');

	// Test of the 'set' implementation that uses rplac-assoc.
	expect(
		globalInfo.evaluateToString(
			[
				"(select '(1) (r-e-p-loop '(",
				'	(set f (lambda (n)',
				'		(let ((g (lambda ()',
				'			(begin',
				'				(set n (+ n 1))',
				'				0))))',
				'			(begin',
				'				(g)',
				'				n))))',
				'	(f 13)',
				')))'
			].join('\n')
		)
	).toBe('(14)');

	// Test of the 'let' implementation that uses lambda.
	expect(
		globalInfo.evaluateToString(
			[
				"(r-e-p-loop '(",
				'	(let ((a 2) (b 3) (c 5) (d (+ 3 4)))',
				'	(* (+ a b) (+ c d)))',
				'))'
			].join('\n')
		)
	).toBe('(60)');

	// Test of the 'let*' implementation that uses nested lambda expressions.
	expect(
		globalInfo.evaluateToString(
			[
				"(r-e-p-loop '(",
				'	(let* ((a 1) (b (* a 2)) (c (* b 3)) (d (* c 4)))',
				'	d)',
				'))'
			].join('\n')
		)
	).toBe('(24)');
});

// globalInfo.evaluate(
// 	[
// 		'',
// 		'',
// 		'',
// 		'',
// 		'',
// 		''
// 	].join('\n')
// );
