// tom-weatherhead/thaw-parser/test/scheme-apl.test.ts

// An implementation of (some of) APL in Scheme.

'use strict';

import { LanguageSelector } from 'thaw-interpreter-types';

// import { createTokenizer } from 'thaw-lexical-analyzer';

// import { createParser, SyntaxException } from 'thaw-parser';

import { SchemeGlobalInfo } from '../../..';

import { createInfrastructure } from '../../create-infrastructure';

// const ls = LanguageSelector.Scheme;

// ThAW 2021-12-04 : Temporarily commented out this test
// because it was taking far too long (more than 10 minutes) on CircleCI.
// Perhaps we can re-enable it later for local testing only;
// maybe via: if (typeof process.env['CC_TEST_REPORTER_ID'] === 'undefined') { ... }

test('Scheme APL-Evaluator test', () => {
	// Arrange
	const { tokenizer, parser } = createInfrastructure(LanguageSelector.Scheme);
	const globalInfo = new SchemeGlobalInfo({ tokenizer, parser });

	// const grammar = createGrammar(ls);
	// const tokenizer = createTokenizer(grammar.defaultLexicalAnalyzer, ls);
	// const parser = createParser(grammar.defaultParser, grammar);
	// const globalInfo = new SchemeGlobalInfo({ tokenizer, parser });

	globalInfo.loadPresets();

	globalInfo.loadPreset('assoc');
	globalInfo.loadPreset('select');
	globalInfo.loadPreset('flatten');
	//globalInfo.loadPreset("compose");

	globalInfo.evaluate('(set cddr (compose cdr cdr))');
	globalInfo.evaluate('(set caddr (compose cdr cadr))');
	globalInfo.evaluate('(set cadddr (compose cdr caddr))');

	globalInfo.evaluate(
		[
			'(set get-type (lambda (x)',
			'(cond',
			"((number? x) 'scalar)",
			"((null? x) 'vector)",
			"((number? (car x)) 'vector)",
			"('T 'matrix))))"
		].join(' ')
	);
	globalInfo.evaluate('(set s1 7)');
	globalInfo.evaluate("(set v1 '(2 3 5 7))");
	globalInfo.evaluate("(set m1 '((3 4) 1 2 3 4 5 6 7 8 9 10 11 12))");

	expect(globalInfo.evaluateToString('(get-type s1)')).toBe('scalar');
	expect(globalInfo.evaluateToString("(get-type '())")).toBe('vector');
	expect(globalInfo.evaluateToString('(get-type v1)')).toBe('vector');
	expect(globalInfo.evaluateToString('(get-type m1)')).toBe('matrix');

	globalInfo.evaluate(
		[
			'(set shape (lambda (x)',
			'(let ((type (get-type x)))',
			'(cond',
			"    ((= type 'scalar) '())",
			"    ((= type 'vector) (list (length x)))",
			"    ('T (car x))))))"
		].join(' ')
	);

	expect(globalInfo.evaluateToString('(shape s1)')).toBe('()');
	expect(globalInfo.evaluateToString('(shape v1)')).toBe('(4)');
	expect(globalInfo.evaluateToString('(shape m1)')).toBe('(3 4)');

	globalInfo.evaluate(
		[
			'(set to-vector (lambda (x)',
			'(let ((type (get-type x)))',
			'(cond',
			"    ((= type 'scalar) (list x))",
			"    ((= type 'vector) x)",
			"   ('T (cdr x))))))"
		].join(' ')
	);

	expect(globalInfo.evaluateToString('(to-vector s1)')).toBe('(7)');
	expect(globalInfo.evaluateToString('(to-vector v1)')).toBe('(2 3 5 7)');
	expect(globalInfo.evaluateToString('(to-vector m1)')).toBe('(1 2 3 4 5 6 7 8 9 10 11 12)');

	globalInfo.evaluate(
		[
			'(set get-first-scalar (lambda (x)',
			'(let ((type (get-type x)))',
			'(cond',
			"    ((= type 'scalar) x)",
			"    ((= type 'vector) (car x))",
			"    ('T (cadr x))))))"
		].join(' ')
	);

	expect(globalInfo.evaluateToString('(get-first-scalar 7)')).toBe('7');
	expect(globalInfo.evaluateToString("(get-first-scalar '(13 14 15))")).toBe('13');
	expect(globalInfo.evaluateToString("(get-first-scalar '((2 2) 9 3 5 7))")).toBe('9');

	globalInfo.evaluate('(set +/ (combine id + 0))');
	globalInfo.evaluate('(set -/ (combine id - 0))');
	globalInfo.evaluate('(set */ (combine id * 1))');
	globalInfo.evaluate('(set // (combine id / 1))');

	globalInfo.evaluate(
		'(set to-scalar-if-possible (lambda (x) (if (= (*/ (shape x)) 1) (get-first-scalar x) x)))'
	);

	expect(globalInfo.evaluateToString('(to-scalar-if-possible 13)')).toBe('13');
	expect(globalInfo.evaluateToString("(to-scalar-if-possible '(7))")).toBe('7');
	expect(globalInfo.evaluateToString("(to-scalar-if-possible '(8 9))")).toBe('(8 9)');
	expect(globalInfo.evaluateToString("(to-scalar-if-possible '((1 1) 20))")).toBe('20');
	expect(globalInfo.evaluateToString("(to-scalar-if-possible '((2 2) 1 0 0 1))")).toBe(
		'((2 2) 1 0 0 1)'
	);

	globalInfo.evaluate(
		[
			'(set get-matrix-rows (lambda (m)',
			'(letrec ((get-matrix-rows* (lambda (r c l)',
			"    (if (= r 0) '()",
			'        (cons (take c l) (get-matrix-rows* (- r 1) c (skip c l)))))))',
			'(get-matrix-rows* (caar m) (cadar m) (cdr m)))))'
		].join(' ')
	);

	expect(globalInfo.evaluateToString('(get-matrix-rows m1)')).toBe(
		'((1 2 3 4) (5 6 7 8) (9 10 11 12))'
	);

	globalInfo.evaluate('(set max-of-pair (lambda (x y) (if (> x y) x y)))');
	globalInfo.evaluate('(set max/ (lambda (l) ((combine id max-of-pair (car l)) (cdr l))))');
	globalInfo.evaluate('(set apl-and (lambda (x y) (if (and (<> x 0) (<> y 0)) 1 0)))');
	globalInfo.evaluate('(set apl-or (lambda (x y) (if (or (<> x 0) (<> y 0)) 1 0)))');
	globalInfo.evaluate('(set and/ (combine id apl-and 1))');
	globalInfo.evaluate('(set or/ (combine id apl-or 0))');

	globalInfo.evaluate(
		['(set m-to-n (lambda (m n)', "(if (> m n) '()", '(cons m (m-to-n (+1 m) n)))))'].join(' ')
	);

	globalInfo.evaluate(
		[
			'(set repeat (lambda (n l)',
			'	(letrec ((repeat* (lambda (n l l-original)',
			'	        (cond',
			"	            ((= n 0) '())",
			'	            ((null? l) (repeat* n l-original l-original))',
			"	            ('T (cons (car l) (repeat* (- n 1) (cdr l) l-original)))))))",
			'	(repeat* n l l))))'
		].join(' ')
	);

	globalInfo.evaluate(
		[
			'(set restruct (lambda (desired-shape src-data)',
			'(let* ((length-of-desired-shape (length desired-shape))',
			'   (src-vector (to-vector src-data))',
			'   (dst-vector (repeat (*/ desired-shape) src-vector)))',
			'(cond',
			"    ((= length-of-desired-shape 0) 'restruct-to-scalar-error)",
			'    ((= length-of-desired-shape 1) dst-vector)',
			"    ('T (cons desired-shape dst-vector))))))"
		].join(' ')
	);

	globalInfo.evaluate(
		[
			'(set trans (lambda (matrix)',
			'(letrec ((get-column (lambda (n l) (mapcar ((curry nth) n) l)))',
			'     (get-data (lambda (n num-cols l)',
			'        (if (< n num-cols)',
			'            (append (get-column n l) (get-data (+1 n) num-cols l))',
			"            '())))",
			'     (new-shape (list (cadar matrix) (caar matrix))))',
			'(cons new-shape (get-data 0 (cadar matrix) (get-matrix-rows matrix))))))'
		].join(' ')
	);

	globalInfo.evaluate(
		[
			'(set [] (lambda (x y)',
			'(let ((type-of-x (get-type x))',
			'  (type-of-y (get-type y))',
			'  (vector-y (to-vector y))',
			'  (nth*-reversed-args (lambda (l n) (nth (- n 1) l))))',
			'(cond',
			"    ((= type-of-x 'scalar) '[]-x-scalar-error)",
			"    ((= type-of-y 'matrix) '[]-x-matrix-error)",
			"    ((= type-of-x 'vector) (mapcar ((curry nth*-reversed-args) x) vector-y))",
			"    ('T (restruct",
			'        (list (length vector-y) (cadar x))',
			'        (flatten (mapcar ((curry nth*-reversed-args) (get-matrix-rows x)) vector-y))))))))'
		].join(' ')
	);

	// Binary operators to implement:

	// - compress
	globalInfo.evaluate(
		[
			'(set compress (lambda (x y)',
			'(letrec ((type-of-x (get-type x))',
			'     (type-of-y (get-type y))',
			'     (is-logical (lambda (v)',
			"        (if (null? v) 'T",
			'            (if (or (= (car v) 0) (= (car v) 1))',
			'                (is-logical (cdr v))',
			"                '()))))",
			'     (compress* (lambda (logv l)',
			"        (if (or (null? logv) (null? l)) '()",
			'            (if (= (car logv) 0)',
			'                (compress* (cdr logv) (cdr l))',
			'                (cons (car l) (compress* (cdr logv) (cdr l))))))))',
			'(cond',
			"    ((<> type-of-x 'vector) 'compress-x-not-vector-error)",
			"    ((not (is-logical x)) 'compress-vector-not-logical-error)",
			"    ((= type-of-y 'scalar) 'compress-y-scalar-error)",
			"    ((= type-of-y 'vector) (compress* x y))",
			"    ('T (restruct",
			'        (list (+/ x) (cadar y))',
			'        (flatten (compress* x (get-matrix-rows y)))))))))'
		].join(' ')
	);

	globalInfo.evaluate(
		[
			'(set apply-binary-op (lambda (f x y)',
			'(letrec ((combine2 (lambda (f l1 l2)',
			"        (if (or (null? l1) (null? l2)) '()",
			'            (cons (f (car l1) (car l2)) (combine2 f (cdr l1) (cdr l2))))))',
			'     (apply-scalar-scalar (lambda (f x y) (f x y)))',
			'     (apply-scalar-vector (lambda (f x y) (mapcar (lambda (z) (f x z)) y)))',
			'     (apply-scalar-matrix (lambda (f x y) (cons (car y) (mapcar (lambda (z) (f x z)) (cdr y)))))',
			'     (apply-vector-scalar (lambda (f x y) (mapcar (lambda (z) (f z y)) x)))',
			'     (apply-vector-vector (lambda (f x y)',
			'        (if (= (length x) (length y))',
			'            (combine2 f x y)',
			"            'binary-op-vector-shape-mismatch)))",
			'     (apply-matrix-scalar (lambda (f x y) (cons (car x) (mapcar (lambda (z) (f z y)) (cdr x)))))',
			'     (apply-matrix-matrix (lambda (f x y)',
			'        (if (equal (car x) (car y))',
			'            (cons (car x) (combine2 f (cdr x) (cdr y)))',
			"            'binary-op-matrix-shape-mismatch)))",
			'     (apply-binary-op* (lambda (f x y)',
			'        (begin',
			'            (set x (to-scalar-if-possible x))',
			'            (set y (to-scalar-if-possible y))',
			'            (let ((type-of-x (get-type x))',
			'                  (type-of-y (get-type y)))',
			'                (cond',
			"                    ((= type-of-x 'scalar)",
			'                        (cond',
			"                            ((= type-of-y 'scalar) (apply-scalar-scalar f x y))",
			"                            ((= type-of-y 'vector) (apply-scalar-vector f x y))",
			"                            ('T (apply-scalar-matrix f x y))))",
			"                    ((= type-of-x 'vector)",
			'                        (cond',
			"                            ((= type-of-y 'scalar) (apply-vector-scalar f x y))",
			"                            ((= type-of-y 'vector) (apply-vector-vector f x y))",
			"                            ('T 'binary-op-vector-matrix-error)))",
			"                    ((= type-of-x 'matrix)",
			'                        (cond',
			"                            ((= type-of-y 'scalar) (apply-matrix-scalar f x y))",
			"                            ((= type-of-y 'vector) 'binary-op-matrix-vector-error)",
			"                            ('T (apply-matrix-matrix f x y)))))))))",
			'     (apl< (lambda (x y) (if (< x y) 1 0)))',
			'     (apl> (lambda (x y) (if (> x y) 1 0)))',
			'     (apl= (lambda (x y) (if (= x y) 1 0))))',
			'(cond',
			"    ((= f '+) (apply-binary-op* + x y))",
			"    ((= f '-) (apply-binary-op* - x y))",
			"    ((= f '*) (apply-binary-op* * x y))",
			"    ((= f '/) (apply-binary-op* / x y))",
			"    ((= f '<) (apply-binary-op* apl< x y))",
			"    ((= f '>) (apply-binary-op* apl> x y))",
			"    ((= f '=) (apply-binary-op* apl= x y))",
			"    ((= f 'max) (apply-binary-op* max-of-pair x y))",
			"    ((= f 'and) (apply-binary-op* apl-and x y))",
			"    ((= f 'or) (apply-binary-op* apl-or x y))",
			"    ((= f 'restruct) (restruct x y))",
			"    ((= f 'cat) (append (to-vector x) (to-vector y)))",
			"    ((= f '[]) ([] x y))",
			"    ((= f 'compress) (compress x y))",
			"    ('T 'binary-op-error!)",
			'))))'
		].join(' ')
	);

	globalInfo.evaluate(
		[
			'(set apply-unary-op (lambda (f x)',
			'(let* ((type-of-x (get-type x))',
			'   (apply-reduction-op (lambda (f x)',
			'        (cond',
			"            ((= type-of-x 'scalar) 'scalar-reduction-error)",
			"            ((= type-of-x 'vector) (f x))",
			"            ('T (mapcar f (get-matrix-rows x)))))))",
			'(cond',
			"    ((= f '+/) (apply-reduction-op +/ x))",
			"    ((= f '-/) (apply-reduction-op -/ x))",
			"    ((= f '*/) (apply-reduction-op */ x))",
			"    ((= f '//) (apply-reduction-op // x))",
			"    ((= f 'max/) (apply-reduction-op max/ x))",
			"    ((= f 'and/) (apply-reduction-op and/ x))",
			"    ((= f 'or/) (apply-reduction-op or/ x))",
			"    ((= f 'shape) (shape x))",
			"    ((= f 'indx) (m-to-n 1 x))",
			"    ((= f 'ravel) (to-vector x))",
			"    ((= f 'trans) (trans x))",
			"    ('T 'unary-op-error!)", // ; ('T f)
			'))))'
		].join(' ')
	);

	// begin
	globalInfo.evaluate(
		[
			'(set do-begin (lambda (expr-list rho fundefs)',
			'(if (null? (cdr expr-list))',
			'(eval (car expr-list) rho fundefs)',
			'(begin',
			'    (eval (car expr-list) rho fundefs)',
			'    (do-begin (cdr expr-list) rho fundefs)))))'
		].join(' ')
	);

	globalInfo.evaluate(
		[
			'(set eval (lambda (expr rho fundefs)',
			'(cond',
			'((number? expr) expr)',
			'((symbol? expr)',
			'    (if (assoc-contains-key expr rho)',
			'        (assoc expr rho)',
			'        (assoc expr global-environment)))',
			"((= (car expr) 'quote) (cadr expr))",
			"((= (car expr) 'if)",
			'    (if (= 0 (get-first-scalar (eval (cadr expr) rho fundefs)))',
			'        (eval (cadddr expr) rho fundefs)',
			'        (eval (caddr expr) rho fundefs)))',
			"((= (car expr) 'begin) (do-begin (cdr expr) rho fundefs))", // ; Exercise 6a) on page 61
			"((= (car expr) 'print)", // ; Exercise 6a) on page 61
			'    (print (eval (cadr expr) rho fundefs)))',
			"((= (car expr) 'set)",
			'    (let ((evaluated-expression (eval (caddr expr) rho fundefs)))',
			'        (if (assoc-contains-key (cadr expr) rho)',
			'            (begin',
			'                (rplac-assoc (cadr expr) evaluated-expression rho)',
			'                evaluated-expression)',
			'            (begin',
			'                (set global-environment (mkassoc (cadr expr) evaluated-expression global-environment))',
			'                evaluated-expression))))',
			'((userfun? (car expr) fundefs)',
			'    (apply-userfun',
			'        (assoc (car expr) fundefs)',
			'        (evallist (cdr expr) rho fundefs)',
			'        fundefs))',
			'((= (length expr) 2)',
			'    (apply-unary-op (car expr) (eval (cadr expr) rho fundefs)))',
			"('T (apply-binary-op (car expr)",
			'        (eval (cadr expr) rho fundefs)',
			'        (eval (caddr expr) rho fundefs))))))'
		].join(' ')
	);

	globalInfo.evaluate('(set userfun? (lambda (f fundefs) (assoc-contains-key f fundefs)))');

	globalInfo.evaluate(
		[
			'(set apply-userfun (lambda (fundef args fundefs)',
			'(eval (cadr fundef)', // ; body of function
			"(mkassoc* (car fundef) args '())", // ; local env
			'fundefs)))'
		].join(' ')
	);

	globalInfo.evaluate(
		[
			'(set evallist (lambda (el rho fundefs)',
			"(if (null? el) '()",
			'(cons (eval (car el) rho fundefs)',
			'    (evallist (cdr el) rho fundefs)))))'
		].join(' ')
	);

	globalInfo.evaluate(
		[
			'(set mkassoc* (lambda (keys values al)',
			'(if (null? keys) al',
			'(mkassoc* (cdr keys) (cdr values)',
			'    (mkassoc (car keys) (car values) al)))))'
		].join(' ')
	);

	globalInfo.evaluate(
		[
			'(set r-e-p-loop (lambda (inputs)',
			'(begin',
			"(set global-environment '())",
			"(r-e-p-loop* inputs '()))))"
		].join(' ')
	);

	globalInfo.evaluate(
		[
			'(set r-e-p-loop* (lambda (inputs fundefs)',
			'(cond',
			"((null? inputs) '())", // ; session done
			'((atom? (car inputs))', // ; input is variable or number
			'    (process-expr (car inputs) (cdr inputs) fundefs))',
			"((= (caar inputs) 'define)", // ; input is function definition
			'    (process-def (car inputs) (cdr inputs) fundefs))',
			"('T (process-expr (car inputs) (cdr inputs) fundefs)))))"
		].join(' ')
	);

	globalInfo.evaluate(
		[
			'(set process-def (lambda (e inputs fundefs)',
			'(cons (cadr e)', // ; echo function name
			'(r-e-p-loop* inputs',
			'    (mkassoc (cadr e) (cddr e) fundefs)))))'
		].join(' ')
	);

	globalInfo.evaluate(
		[
			'(set process-expr (lambda (e inputs fundefs)',
			"(cons (eval e '() fundefs)", // ; print value of expression
			'(r-e-p-loop* inputs fundefs))))'
		].join(' ')
	);

	// indx test
	expect(globalInfo.evaluateToString("(r-e-p-loop '((indx 8)))")).toBe('((1 2 3 4 5 6 7 8))');

	// max/ test
	expect(
		globalInfo.evaluateToString(
			[
				'(r-e-p-loop (list',
				"(list 'max/ (list 'quote '(2 4 6 8 1 3 5 7)))",
				"(list 'max/ (list 'quote '((3 4) 8 4 10 1 9 2 5 7 3 11 6 12)))))"
			].join(' ')
		)
	).toBe('(8 (10 9 12))');

	// restruct test
	expect(
		globalInfo.evaluateToString(
			[
				'(r-e-p-loop (list',
				"(list 'restruct (list 'quote '(7)) (list 'quote '(8 9)))",
				"(list 'restruct (list 'quote '(4 4)) (list 'quote '(1 0 0 0 0)))",
				'))'
			].join(' ')
		)
	).toBe('((8 9 8 9 8 9 8) ((4 4) 1 0 0 0 0 1 0 0 0 0 1 0 0 0 0 1))');

	// trans test
	expect(
		globalInfo.evaluateToString(
			[
				"(select '(1) (r-e-p-loop (list",
				"(list 'set 'm1 (list 'restruct (list 'quote '(3 4)) '(indx 12)))",
				"'(trans m1)",
				')))'
			].join(' ')
		)
	).toBe('(((4 3) 1 5 9 2 6 10 3 7 11 4 8 12))');

	// [] test
	expect(
		globalInfo.evaluateToString(
			[
				"(select '(2 3 4 5) (r-e-p-loop (list",
				"(list 'set 'v1 (list 'quote '(8 6 7 5 3 0 9)))",
				"(list 'set 'm1 (list 'restruct (list 'quote '(3 4)) '(indx 12)))",
				"'([] v1 4)",
				"(list '[] 'v1 (list 'quote '(3 1 7 6)))",
				"'([] m1 2)",
				"(list '[] 'm1 (list 'quote '(3 1)))",
				')))'
			].join(' ')
		)
	).toBe('((5) (7 8 9 0) ((1 4) 5 6 7 8) ((2 4) 9 10 11 12 1 2 3 4))');

	// compress test
	expect(
		globalInfo.evaluateToString(
			[
				"(select '(2 3) (r-e-p-loop (list",
				"(list 'set 'v1 (list 'quote '(8 6 7 5 3 0 9)))",
				"(list 'set 'm1 (list 'restruct (list 'quote '(4 4)) '(indx 16)))",
				"(list 'compress (list 'quote '(1 0 1 1 0 1 0)) 'v1)",
				"(list 'compress (list 'quote '(0 1 0 1)) 'm1)",
				')))'
			].join(' ')
		)
	).toBe('((8 7 5 0) ((2 4) 5 6 7 8 13 14 15 16))');

	// primes<= test (see pages 74-75)
	expect(
		globalInfo.evaluateToString(
			[
				"(select '(2 4) (r-e-p-loop '(",
				'(define mod (m n) (- m (* n (/ m n))))',
				'(define mod-outer-probe (v1 v2)',
				'(mod (trans (restruct (cat (shape v2) (shape v1)) v1))',
				'    (restruct (cat (shape v1) (shape v2)) v2)))',
				'(mod-outer-probe (indx 4) (indx 7))',
				// ; Perhaps we could implement 'let', and then use (let ((s (indx n))) ...)
				'(define primes<= (n) (compress (= 2 (+/ (= 0 (mod-outer-probe (set s (indx n)) s)))) s))',
				'(primes<= 7)',
				')))'
			].join(' ')
		)
	).toBe('(((4 7) 0 1 1 1 1 1 1 0 0 2 2 2 2 2 0 1 0 3 3 3 3 0 0 1 0 4 4 4) (2 3 5 7))');

	// +\ ("+-scan") test (see page 74).  This tests the "if" construct.
	expect(
		globalInfo.evaluateToString(
			[
				"(select '(0) (r-e-p-loop (list",
				"(list '= (list 'shape (list 'quote '(1 3 5 7))) 0)",
				')))'
			].join(' ')
		)
	).toBe('(0)');
	expect(
		globalInfo.evaluateToString(
			[
				"(select '(1) (r-e-p-loop (list",
				"'(define foo (v) (if (= (shape v) 0) 1 0))",
				"(list 'foo (list 'quote '(1 3 5 7)))",
				')))'
			].join(' ')
		)
	).toBe('(0)');

	expect(
		globalInfo.evaluateToString(
			[
				"(select '(2) (r-e-p-loop (list",
				"'(define dropend (v) ([] v (indx (- (shape v) 1))))",
				"'(define + (v)",
				'(if (= (shape v) 0) v',
				'    (cat (+ (dropend v)) (+/ v))))',
				"(list '+ (list 'quote '(1 3 5 7)))",
				')))'
			].join(' ')
		)
	).toBe('((1 4 9 16))');
});
