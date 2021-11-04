// tom-weatherhead/thaw-grammar/test/languages/smalltalk/smalltalk-grammar.test.ts

'use strict';

import { LanguageSelector } from 'thaw-interpreter-types';

// import { SyntaxException } from 'thaw-parser';

import {
	ISmalltalkExpression,
	ISmalltalkValue,
	SmalltalkEnvironmentFrame,
	SmalltalkGlobalInfo
} from '../../..';

import {
	createFnParser,
	createFnRecognizer,
	createInfrastructure
} from '../../create-infrastructure';

const ls = LanguageSelector.Smalltalk;

function createFnEval(): (str: string) => ISmalltalkValue {
	const { tokenizer, parser } = createInfrastructure(ls);
	const globalInfo = new SmalltalkGlobalInfo();

	globalInfo.loadPresets(tokenizer, parser);

	return (str: string) =>
		globalInfo.evaluate(parser.parse(tokenizer.tokenize(str)) as ISmalltalkExpression);
}

function evalStringsToValues(strs: string[], n = 1): ISmalltalkValue[] {
	const f = createFnEval();

	return strs.map(f).slice(-n);
}

function evalStringsToValue(strs: string[]): ISmalltalkValue {
	const values = evalStringsToValues(strs, 1);

	if (values.length < 1) {
		throw new Error('evalToValue() : values.length is zero.');
	}

	return values[0];
}

function evaluateStringToInteger(str: string): number | undefined {
	return evalStringsToValue([str]).toInteger();
}

function evaluateStringsToInteger(strs: string[]): number | undefined {
	return evalStringsToValue(strs).toInteger();
}

function evaluateStringsToIntegers(strs: string[], n = 1): Array<number | undefined> {
	return evalStringsToValues(strs, n).map(value => value.toInteger());
}

test('SmalltalkGrammar instance creation test', () => {
	// Arrange
	const { grammar } = createInfrastructure(ls);

	// Act
	// Assert
	expect(grammar).toBeTruthy();
});

test('SmalltalkGrammar parser instance creation test', () => {
	// Arrange
	// Act
	const { parser } = createInfrastructure(ls);

	// Assert
	expect(parser).toBeTruthy();
});

// Sample Smalltalk code (using Kamin's syntax) :

// ; Random.txt - A pseudorandom number generator
// ; Exercise 1 on page 344
// (class Rand Object ()
//     (seed)
//     (define init () (begin (initRand self 1) self))
//     (define initRand (n) (set seed n))
//     (define nextRand () (set seed (mod (+ (* seed 9) 5) 1024)))
// )
// (set r (init (new Rand)))
// (nextRand r)
// (nextRand r)

test('SmalltalkGrammar recognize test', () => {
	// Arrange
	const f = createFnRecognizer(ls);

	const str1 = [
		'(class Rand Object ()',
		'    (seed)',
		'    (define init () (begin (initRand self 1) self))',
		'    (define initRand (n) (set seed n))',
		'    (define nextRand () (set seed (mod (+ (* seed 9) 5) 1024)))',
		')'
	].join(' ');

	f(str1);
	f('(set r (init (new Rand)))');
	f('(nextRand r)');
	f('(nextRand r)');

	f('0'); // An integer
	f('0.5'); // A floating-point number
	f('#symbol');
	f('$c'); // A character
	f('x'); // A variable

	f('(define add (x y) (+ x y))'); // A function definition
	f('(+ 2 3)'); // An operator usage

	// expect(() => f('')).toThrow(SyntaxException);
});

// public void RecognizeTest()
// {
//     // From Kamin page 274.
//     parser.Recognize(tokenizer.Tokenize(@"(class FinancialHistory Object ()
//         (cashOnHand incomes expenditures)
//         (define initFinancialHistory (amount)
//             (begin
//                 (set cashOnHand amount)
//                 (set incomes (mkDictionary))
//                 (set expenditures (mkDictionary))
//                 self))
//         )"));
// }

test('SmalltalkGrammar addition test', () => {
	const localEnvironment = new SmalltalkEnvironmentFrame();
	const globalInfo = new SmalltalkGlobalInfo();

	const f = createFnParser<ISmalltalkExpression>(ls);
	const a = 2;
	const b = 3;

	const str = `(+ ${a} ${b})`;
	const actualSmalltalkExpression = f(str);
	const actualSmalltalkValue = actualSmalltalkExpression.evaluate(
		localEnvironment,
		undefined,
		undefined,
		globalInfo
	);

	// Evaluation: Method 1:
	expect(actualSmalltalkValue.isInteger).toBe(true);
	expect(actualSmalltalkValue.toInteger()).toBe(a + b);

	// Evaluation: Method 2:
	expect(evaluateStringToInteger(str)).toBe(a + b);
});

test('SmalltalkGrammar function definition test', () => {
	const a = 2;
	const b = 3;

	const actualSmalltalkValue = evalStringsToValue([
		'(define add (x y) (+ x y))',
		`(add ${a} ${b})`
	]);

	expect(actualSmalltalkValue.isInteger).toBe(true);
	expect(actualSmalltalkValue.toInteger()).toBe(a + b);
});

test('SmalltalkGrammar class definition test 1', () => {
	const str1 = [
		'(class Counter Object ()',
		'	(x)',
		'	(define init () (begin (set x 0) self))',
		'	(define get () x)',
		'	(define inc () (set x (+ x 1)))',
		')'
	].join(' ');

	const actualSmalltalkValue = evalStringsToValue([
		str1,
		'(set c (init (new Counter)))',
		'(inc c)',
		'(inc c)',
		'(inc c)',
		'(get c)'
	]);

	expect(actualSmalltalkValue.isInteger).toBe(true);
	expect(actualSmalltalkValue.toInteger()).toBe(3);
});

test('SmalltalkGrammar if test', () => {
	expect(evaluateStringToInteger('(if (= 2 2) 5 7)')).toBe(5);
	expect(evaluateStringToInteger('(if (= 2 3) 5 7)')).toBe(7);
});

test('SmalltalkGrammar while test', () => {
	const str1 = [
		'(define powInt (x y)',
		'	(let ((result 1))',
		'		(begin',
		'			(while (> y 0)',
		'				(begin',
		'					(set result (* result x))',
		'					(set y (- y 1))',
		'				)',
		'			)',
		'			result',
		'		)',
		'	)',
		')'
	].join(' ');

	const actualSmalltalkValue = evalStringsToValue([str1, '(powInt 3 4)']);

	expect(actualSmalltalkValue.isInteger).toBe(true);
	expect(actualSmalltalkValue.toInteger()).toBe(81);
});

test('SmalltalkGrammar cond test', () => {
	const actualValues = evalStringsToValues(
		[
			'(define condtest (n) (cond ((= n 1) #First) ((= n 2) #Second) ((= n 3) #Third) (true #Other)))',
			'(condtest 0)',
			'(condtest 1)',
			'(condtest 2)',
			'(condtest 3)',
			'(condtest 4)'
		],
		5
	).map((value) => value.toString());

	expect(actualValues.length).toBe(5);

	expect(actualValues[0]).toBe('Other');
	expect(actualValues[1]).toBe('First');
	expect(actualValues[2]).toBe('Second');
	expect(actualValues[3]).toBe('Third');
	expect(actualValues[4]).toBe('Other');
});

test('SmalltalkGrammar let test 1', () => {
	expect(evaluateStringToInteger('(let ((n (+ 2 3))) n)')).toBe(5);
});

test('SmalltalkGrammar let test 2', () => {
	const str1 = ['(define increment (n)', '	(let ((m 1))', '		(+ n m)', '	)', ')'].join(' ');
	const str2 = '(increment 7)';

	expect(evaluateStringsToInteger([str1, str2])).toBe(8);
});

test('SmalltalkGrammar let* test', () => {
	expect(evaluateStringToInteger('(let* ((x (+ 2 3)) (y (* x x))) y)')).toBe(25);
});

// [Test]
// public void PlusTest()
// {
//     Assert.AreEqual("5", Evaluate("(+ 2 3)"));
//     Assert.AreEqual("5.25", Evaluate("(+ 2.25 3)"));
//     Assert.AreEqual("5.5", Evaluate("(+ 2 3.5)"));
//     Assert.AreEqual("5.75", Evaluate("(+ 2.25 3.5)"));
//     Assert.AreEqual("5.0", Evaluate("(+ 2.0 3.0)"));
//     Assert.AreEqual("6.0", Evaluate("(+ 2.5 3.5)"));
// }

// [Test]
// public void UserDefFuncTest()
// {
//     Assert.AreEqual("14", Evaluate("(+1 13)"));
// }

// [Test]
// public void GCDTest()
// {
//     Assert.AreEqual("8", Evaluate("(gcd 16384 24)"));
//     Assert.AreEqual("1", Evaluate("(gcd 100 81)"));
// }

test('SmalltalkGrammar variable precedence test', () => {
	// See Kamin page 295.

//     Evaluate("(set x 1)");
//     Evaluate(@"
// (class C Object ()
// (x)
// (define init () (begin (set x 2) self))
// (define f () x)
// (define g (x) x)
// )");
//     Evaluate("(set a (init (new C)))");
//
//     Assert.AreEqual("1", Evaluate("x"));
//     Assert.AreEqual("2", Evaluate("(f a)"));
//     Assert.AreEqual("3", Evaluate("(g a 3)"));

	const actualResults = evaluateStringsToIntegers([
		'(set x 1)',
		[
			'(class C Object ()',
			'	(x)',
			'	(define init () (begin (set x 2) self))',
			'	(define f () x)',
			'	(define g (x) x)',
			')'
		].join(' '),
		'(set a (init (new C)))',
		'x',
		'(f a)',
		'(g a 3)'
	], 3);

	expect(actualResults.length).toBe(3);

	expect(actualResults[0]).toBe(1);
	expect(actualResults[1]).toBe(2);
	expect(actualResults[2]).toBe(3);
});

// [Test]
// public void ObjectEqualityTest()
// {
//     Evaluate(@"
// (class C Object ()
// (x)
// (define init (n) (begin (set x n) self))
// (define x () x)
// (define = (other)
// (if (= (typename self) (typename other))
//     (= x (x other))
//     false
// )
// )
// )");
//     Evaluate(@"
// (class D C () ()
// (define foo () 0)
// )");
//     Evaluate(@"
// (class E Object ()
// (x)
// (define init (n) (begin (set x n) self))
// (define x () x)
// (define = (other)
// (if (= (typename self) (typename other))
//     (= x (x other))
//     false
// )
// )
// )");
//     Evaluate("(set a (init (new C) 7))");
//     Evaluate("(set b (init (new C) 7))");
//     Evaluate("(set c (init (new C) 13))");
//     Evaluate("(set d (init (new D) 7))");
//     Evaluate("(set e (init (new E) 7))");
//
//     Assert.AreEqual(trueValueAsString, Evaluate("(= a a)"));
//     Assert.AreEqual(trueValueAsString, Evaluate("(= a b)"));
//     Assert.AreEqual(falseValueAsString, Evaluate("(= a c)"));
//     Assert.AreEqual(falseValueAsString, Evaluate("(= c a)"));
//     Assert.AreEqual(falseValueAsString, Evaluate("(= a d)"));
//     Assert.AreEqual(falseValueAsString, Evaluate("(= d a)"));
//     Assert.AreEqual(falseValueAsString, Evaluate("(= a e)"));
//     Assert.AreEqual(falseValueAsString, Evaluate("(= e a)"));
//     Assert.AreEqual(falseValueAsString, Evaluate("(= a 7)"));
//     Assert.AreEqual(falseValueAsString, Evaluate("(= 7 a)"));
// }
//
// [Test]
// public void FinancialHistoryTest()
// {
//     globalInfo.LoadPreset("collection");
//
//     // From Kamin page 274.
//     const string financialHistoryClass = @"
// (class FinancialHistory Object ()
// (cashOnHand incomes expenditures)
// (define init (amount)
// (begin
//     (set cashOnHand amount)
//     (set incomes (mkDictionary))
//     (set expenditures (mkDictionary))
//     self))
// (define receive:from: (amount source)
// (begin
//     (at:put: incomes source (+ (totalReceivedFrom: self source) amount))
//     (set cashOnHand (+ cashOnHand amount))))
// (define spend:for: (amount reason)
// (begin
//     (at:put: expenditures reason (+ (totalSpentFor: self reason) amount))
//     (set cashOnHand (- cashOnHand amount))))
// (define cashOnHand () cashOnHand)
// (define totalReceivedFrom: (source)
// (if (includesKey: incomes source)
//     (at: incomes source)
//     0))
// (define totalSpentFor: (reason)
// (if (includesKey: expenditures reason)
//     (at: expenditures reason)
//     0))
// )";
//
//     Evaluate(financialHistoryClass);
//     Evaluate("(define mkFinancialHistory (amount) (init (new FinancialHistory) amount))");
//
//     // Page 275:
//     Evaluate("(set myaccount (mkFinancialHistory 1000))");
//     Evaluate("(spend:for: myaccount 50 #insurance)");
//     Evaluate("(receive:from: myaccount 200 #salary)");
//     Assert.AreEqual("1150", Evaluate("(cashOnHand myaccount)"));
//     Evaluate("(spend:for: myaccount 100 #books)");
//     Assert.AreEqual("1050", Evaluate("(cashOnHand myaccount)"));
//
//     // Page 284:
//     Evaluate("(set S (mkSet))");
//     Assert.AreEqual("0", Evaluate("(size S)"));
//     Evaluate("(add: S 2)");
//     Evaluate("(add: S #abc)");
//     Assert.AreEqual(trueValueAsString, Evaluate("(includes: S 2)"));
//     Evaluate("(add: S 2)");
//     Assert.AreEqual("2", Evaluate("(size S)"));
//     Assert.AreEqual("abc", Evaluate("(first S)"));
//     Assert.AreEqual("2", Evaluate("(next S)"));
//     Assert.AreEqual("nil", Evaluate("(next S)"));
//
//     // Page 289:
//     Evaluate("(set L (mkList))");
//     Evaluate("(add: L #a)");
//     Evaluate("(add: L #b)");
//     Assert.AreEqual("b", Evaluate("(first L)"));
//     Assert.AreEqual("a", Evaluate("(next L)"));
//     Assert.AreEqual("nil", Evaluate("(next L)"));
//     Assert.AreEqual("a", Evaluate("(at: L 2)"));
// }
//
// [Test]
// public void InheritanceTest()
// {
//     // From Kamin page 279.
//     const string classC = @"
// (class C Object () ()
// (define m1 () (m2 self))
// (define m2 () #C))";
//     const string classD = @"
// (class D C () ()
// (define m2 () #D))";
//
//     Evaluate(classC);
//     Evaluate(classD);
//     Evaluate("(set x (new D))");
//     Assert.AreEqual("D", Evaluate("(m1 x)"));
// }
//
// [Test]
// public void SuperTest1()
// {
//     Evaluate(@"
// (class C Object () ()
// (define f () 7)
// (define g () 13))");
//     Evaluate(@"
// (class D C () ()
// (define f () (f super))
// (define g () 101)
// (define h () (g super)))");
//     Evaluate("(set x (new D))");
//     Assert.AreEqual("7", Evaluate("(f x)"));
//     Assert.AreEqual("13", Evaluate("(h x)"));
// }
//
// [Test]
// public void SuperTest2()
// {
//     Evaluate(@"
// (class A Object () ()
// (define f () 1))");
//     Evaluate(@"
// (class B A () ()
// (define f () 2))");
//     Evaluate(@"
// (class C B () ()
// (define h () (f self)))");
//     Evaluate(@"
// (class D C () ()
// (define f () 3)
// (define g () (f super))
// (define i () (h super)))");
//     Evaluate(@"
// (class E D () ()
// (define f () 4))");
//     Evaluate("(set x (new E))");
//     Assert.AreEqual("2", Evaluate("(g x)"));
//     Assert.AreEqual("4", Evaluate("(i x)"));
// }
//
// [Test]
// public void ClassVariableTest()
// {
//     Evaluate(@"
// (class C Object (cv) ()
// (define f (n) (set cv n))
// (define g () cv))");
//     Evaluate("(set x (new C))");
//     Evaluate("(set y (new C))");
//
//     Assert.AreEqual("0", Evaluate("(g y)"));
//
//     Evaluate("(f x 7)");
//
//     Assert.AreEqual("7", Evaluate("(g y)"));
// }
//
// [Test]
// public void UserValueToStringTest()
// {
//     Evaluate(@"
// (class Point Object () (x y)
// (define init (xval yval)
// (begin
//     (set x xval)
//     (set y yval)
//     self))
// )");
//     Evaluate("(set p (init (new Point) 7 13))");
//
//     Assert.AreEqual("7\r\n13", Evaluate("p"));
// }
//
// [Test]
// public void Exercise3Test()
// {
//     globalInfo.LoadPreset("collection");
//
//     EvaluateToISmalltalkValue("(set L (mkList))");
//     EvaluateToISmalltalkValue("(add: L 7)");
//     EvaluateToISmalltalkValue("(add: L 8)");
//     EvaluateToISmalltalkValue("(add: L 7)");
//     EvaluateToISmalltalkValue("(add: L 9)");
//     EvaluateToISmalltalkValue("(add: L 7)");
//
//     // Exercise 3a)
//     EvaluateToISmalltalkValue("(set S (asSet L))");
//     Assert.AreEqual(trueValueAsString, Evaluate("(includes: S 7)"));
//     Assert.AreEqual(trueValueAsString, Evaluate("(includes: S 8)"));
//     Assert.AreEqual(trueValueAsString, Evaluate("(includes: S 9)"));
//     Assert.AreEqual(falseValueAsString, Evaluate("(includes: S 10)"));
//
//     // Exercise 3b)
//     Assert.AreEqual("3", Evaluate("(occurrencesOf: L 7)"));
//     Assert.AreEqual("1", Evaluate("(occurrencesOf: L 8)"));
//     Assert.AreEqual("1", Evaluate("(occurrencesOf: L 9)"));
//     Assert.AreEqual("0", Evaluate("(occurrencesOf: L 10)"));
//
//     // Exercise 3c)
//     EvaluateToISmalltalkValue("(set S2 (mkSet))");
//     EvaluateToISmalltalkValue("(add: S2 13)");
//     EvaluateToISmalltalkValue("(add: S2 14)");
//     EvaluateToISmalltalkValue("(addAll: L S2)");
//     Assert.AreEqual(trueValueAsString, Evaluate("(includes: L 7)"));
//     Assert.AreEqual(trueValueAsString, Evaluate("(includes: L 8)"));
//     Assert.AreEqual(trueValueAsString, Evaluate("(includes: L 9)"));
//     Assert.AreEqual(falseValueAsString, Evaluate("(includes: L 10)"));
//     Assert.AreEqual(trueValueAsString, Evaluate("(includes: L 13)"));
//     Assert.AreEqual(trueValueAsString, Evaluate("(includes: L 14)"));
// }
//
// [Test]
// public void NumberTest()    // From Kamin; example in Section 7.3 (pages 298-304)
// {
//     Evaluate(@"
// (class Number Object ()
// () ; abstract class
// (define + (x) #subclassResponsibility)
// (define negate () #subclassResponsibility)
// (define * (x) #subclassResponsibility)
// (define recip () #subclassResponsibility)
// (define = (x) #subclassResponsibility)
// (define < (x) #subclassResponsibility)
// (define zero () #subclassResponsibility)
// (define one () #subclassResponsibility)
// (define print () #subclassResponsibility)
// (define - (y) (+ self (negate y)))
// (define / (y) (* self (recip y)))
// (define > (y) (< y self))
// (define +1 () (+ self (one self)))
// (define sub1 () (- self (one self)))
// (define isZero () (= self (zero self)))
// (define isNegative () (< self (zero self)))
// (define abs () (if (isNegative self) (negate self) self))
// (define sqr () (* self self))
// (define sqrt (epsilon) ; find square root of receiver within epsilon
// (begin
//     (set this-step (+1 (zero self)))
//     (set two (+1 this-step))
//     (set next-step (/ (+ this-step (/ self this-step)) two))
//     (while (> (abs (- this-step next-step)) epsilon)
//         (begin
//             (set this-step next-step)
//             (set next-step (/ (+ this-step (/ self this-step)) two))))
//     next-step))
// )");
//     Evaluate(@"
// (class Fraction Number ()
// (x y)
// (define init (a b)
// (begin
//     (setFraction self a b)
//     (sign-reduce self)
//     (div-reduce self)))
// (define setFraction (a b) (begin (set x a) (set y b) self))
// (define x () x)
// (define y () y)
// (define + (f)
// (div-reduce
//     (setFraction (new Fraction)
//         (+ (* x (y f)) (* (x f) y))
//         (* y (y f)))))
// (define negate () (setFraction (new Fraction) (- 0 x) y))
// (define * (f)
// (div-reduce
//     (setFraction (new Fraction)
//         (* x (x f))
//         (* y (y f)))))
// (define recip () (sign-reduce (setFraction (new Fraction) y x)))
// (define = (f) (and (= x (x f)) (= y (y f))))
// (define < (f) (< (* x (y f)) (* (x f) y)))
// (define zero () (setFraction (new Fraction) 0 1))
// (define one () (setFraction (new Fraction) 1 1))
// (define print () (begin (print x) (print y)))
// ; div-reduce and sign-reduce should not be exported
// (define div-reduce ()
// (begin
//     (if (= x 0)
//         (set y 1)
//         (begin
//             (set temp (gcd (abs x) y))
//             (set x (/ x temp))
//             (set y (/ y temp))))
//     self))
// (define sign-reduce ()
// (begin
//     (if (< y 0)
//         (begin (set x (- 0 x)) (set y (- 0 y)))
//         0)
//     self))
// )");
//     Evaluate("(define mkFraction (a b) (init (new Fraction) a b))");
//     Evaluate(@"
// (class Float Number ()
// (mant expo)
// (define init (m e) (begin (set mant m) (set expo e) self))
// (define mant () mant)
// (define expo () expo)
// (define + (x)
// (begin
//     (if (< expo (expo x))
//         (begin
//             (set min self)
//             (set max x))
//         (begin
//             (set min x)
//             (set max self)))
//     (set delta (- (expo max) (expo min)))
//     (set temp (+ (* (mant max) (powerof10 self delta)) (mant min)))
//     (normalize
//         (init (new Float) temp (if (= temp 0) 0 (expo min))))))
// (define negate () (init (new Float) (- 0 mant) expo))
// (define * (x)
// (normalize (init (new Float)
//     (* mant (mant x))
//     (+ expo (expo x)))))
// (define recip ()
// (if (isZero self) self
//     (normalize (init (new Float)
//         (/ 100000000 mant)
//         (- (- 0 8) expo)))))
// (define zero () (init (new Float) 0 0))
// (define one () (init (new Float) 1 0))
// (define = (x) (isZero (- self x)))
// (define < (x) (isNegative (- self x)))
// (define print () (begin (print mant) (print expo)))
// (define isZero () (= mant 0))
// (define isNegative () (< mant 0))
// ; normalize and powerof10 should not be exported
// (define powerof10 (d) (if (= d 0) 1 (* 10 (powerof10 self (- d 1)))))
// (define normalize ()
// (begin
//     (while (> (abs mant) 10000)
//         (begin
//             (set mant (/ mant 10))
//             (set expo (+ expo 1))))
//     self))
// )");
//     Evaluate("(define mkFloat (m e) (init (new Float) m e))");
//
//     // Fraction test from page 302.
//     Evaluate("(set eps (mkFraction 1 2))");
//     Evaluate("(set f1 (mkFraction 17 1))");
//     Assert.AreEqual("3437249\r\n833049", Evaluate("(sqrt f1 eps)"));
//
//     // Float test from page 304.
//     Evaluate("(set eps (mkFloat 5 -1))");
//     Evaluate("(set x1 (mkFloat 17 0))");
//     Assert.AreEqual("4125\r\n-3", Evaluate("(sqrt x1 eps)"));
// }
//
// [Test]
// public void BagTest()    // From Kamin; exercise 4 on page 345.
// {
//     globalInfo.LoadPreset("collection");
//
//     Evaluate(@"
// (class Bag Set () ()
// ; Instead of defining an init method for this class, just use the inherited one.
// ; (define init () (begin (init super) self))
// (define add: (item) (add: members item))
// (define count: (item)
// (let ((tempitem (first self))
//       (count 0))
//     (begin
//         (while (notNil tempitem)
//             (begin
//                 (if (= tempitem item)
//                     (set count (+1 count))
//                     0)
//                 (set tempitem (next self))))
//         count)))
// )");
//     Evaluate("(define mkBag () (init (new Bag)))");
//
//     Evaluate("(set B (mkBag))");
//     Evaluate("(add: B 1)");
//     Evaluate("(add: B 2)");
//     Evaluate("(add: B 1)");
//     Assert.AreEqual("1", Evaluate("(first B)"));
//     Assert.AreEqual("2", Evaluate("(next B)"));
//     Assert.AreEqual("1", Evaluate("(next B)"));
//     Assert.AreEqual("nil", Evaluate("(next B)"));
//     Assert.AreEqual("2", Evaluate("(count: B 1)"));
// }
//
// [Test]
// public void TwoWayListTest1()    // From Kamin; exercise 5 on pages 345-346.
// {
//     globalInfo.LoadPreset("collection");
//
//     // This class implements a doubly-linked list.
//     Evaluate(@"
// (class TwoWayList List ()
// (prev-cdr)
// (define init () (begin
// (init super)
// (set prev-cdr (new TwoWayList))
// (car: prev-cdr nil)
// ; Infinite loop - perhaps because Evaluate() calls ToString() on a circular data structure.  Use EvaluateToISmalltalkValue() instead.
// (cdr: prev-cdr self) ; Kills.
// self))
// (define prev-cdr () prev-cdr)
// (define prev-cdr: (x) (set prev-cdr x))
// (define add: (item)
// (let ((temp (new TwoWayList)))
//     (begin
//         (car: temp car)
//         (cdr: temp cdr)
//         (prev-cdr: temp self)
//         (set cdr temp)
//         (set car item))))
// (define prev ()
// (if (isNil (car currentCell)) nil
//     (begin
//         (set currentKey (- currentKey 1))
//         (set currentCell (prev-cdr currentCell))
//         (car currentCell))))
// )");
//     Evaluate("(define mkTwoWayList () (init (new TwoWayList)))");
//
//     // EvaluateToISmalltalkValue() does not call ToString() on the returned value.
//     // This is vital, since calling ToString() on a circular data structure such as a doubly-linked list would cause an infinite loop.
//     EvaluateToISmalltalkValue("(set L (mkTwoWayList))");
//     EvaluateToISmalltalkValue("(add: L 2)");
//     EvaluateToISmalltalkValue("(add: L 1)");
//     Assert.AreEqual("1", Evaluate("(first L)"));
//     Assert.AreEqual("2", Evaluate("(next L)"));
//     Assert.AreEqual("1", Evaluate("(prev L)"));
//     Assert.AreEqual("2", Evaluate("(next L)"));
//     Assert.AreEqual("1", Evaluate("(prev L)"));
//     Assert.AreEqual("nil", Evaluate("(prev L)"));
// }
//
// [Test]
// public void TwoWayListTest2()    // From Kamin; exercise 5 on pages 345-346.
// {
//     globalInfo.LoadPreset("collection");
//
//     // This class does not implement a doubly-linked list.
//     Evaluate(@"
// (class TwoWayList List ()
// (nilCellAtFront)
// (define init () (begin
// (init super)
// (set nilCellAtFront (new TwoWayList))
// (car: nilCellAtFront nil)
// self))
// (define add: (item)
// (let ((temp (new TwoWayList)))
//     (begin
//         (car: temp car)
//         (cdr: temp cdr)
//         (set cdr temp)
//         (set car item))))
// (define prev ()
// (if (isNil (car currentCell)) nil
//     (let ((prevKey (- currentKey 1))
//           (nextCell self))
//         (begin
//             (set currentKey prevKey)
//             (set currentCell nilCellAtFront)
//             (while (> prevKey 0)
//                 (begin
//                     (set currentCell nextCell)
//                     (set nextCell (cdr nextCell))
//                     (set prevKey (- prevKey 1))))
//             (car currentCell)))))
// )");
//     Evaluate("(define mkTwoWayList () (init (new TwoWayList)))");
//
//     Evaluate("(set L (mkTwoWayList))");
//     Evaluate("(add: L 2)");
//     Evaluate("(add: L 1)");
//     Assert.AreEqual("1", Evaluate("(first L)"));
//     Assert.AreEqual("2", Evaluate("(next L)"));
//     Assert.AreEqual("1", Evaluate("(prev L)"));
//     Assert.AreEqual("2", Evaluate("(next L)"));
//     Assert.AreEqual("1", Evaluate("(prev L)"));
//     Assert.AreEqual("nil", Evaluate("(prev L)"));
// }
//
// [Test]
// public void MagnitudeTest()    // From Kamin; exercise 6 on page 346.
// {
//     // Exercise 6a)
//     Evaluate(@"
// (class Magnitude Object ()
// () ; Abstract class
// (define < (x) #subclassResponsibility)
// (define +1 () #subclassResponsibility)
// (define > (x) (< x self))
// (define >= (x) (not (< self x)))
// (define <= (x) (not (> self x)))
// (define min (x) (if (< self x) self x))
// (define max (x) (if (> self x) self x))
// (define between (lower higher) (and (< lower self) (< self higher)))
// )");
//
//     Evaluate(@"
// (class Number Magnitude ()
// () ; abstract class
// (define + (x) #subclassResponsibility)
// (define negate () #subclassResponsibility)
// (define * (x) #subclassResponsibility)
// (define recip () #subclassResponsibility)
// (define = (x) #subclassResponsibility)
// (define zero () #subclassResponsibility)
// (define one () #subclassResponsibility)
// (define print () #subclassResponsibility)
// (define - (y) (+ self (negate y)))
// (define / (y) (* self (recip y)))
// (define +1 () (+ self (one self)))
// (define sub1 () (- self (one self)))
// (define isZero () (= self (zero self)))
// (define isNegative () (< self (zero self)))
// (define abs () (if (isNegative self) (negate self) self))
// (define sqr () (* self self))
// (define sqrt (epsilon) ; find square root of receiver within epsilon
// (begin
//     (set this-step (+1 (zero self)))
//     (set two (+1 this-step))
//     (set next-step (/ (+ this-step (/ self this-step)) two))
//     (while (> (abs (- this-step next-step)) epsilon)
//         (begin
//             (set this-step next-step)
//             (set next-step (/ (+ this-step (/ self this-step)) two))))
//     next-step))
// )");
//     Evaluate(@"
// (class Fraction Number ()
// (x y)
// (define init (a b)
// (begin
//     (setFraction self a b)
//     (sign-reduce self)
//     (div-reduce self)))
// (define setFraction (a b) (begin (set x a) (set y b) self))
// (define x () x)
// (define y () y)
// (define + (f)
// (div-reduce
//     (setFraction (new Fraction)
//         (+ (* x (y f)) (* (x f) y))
//         (* y (y f)))))
// (define negate () (setFraction (new Fraction) (- 0 x) y))
// (define * (f)
// (div-reduce
//     (setFraction (new Fraction)
//         (* x (x f))
//         (* y (y f)))))
// (define recip () (sign-reduce (setFraction (new Fraction) y x)))
// (define = (f) (and (= x (x f)) (= y (y f))))
// (define < (f) (< (* x (y f)) (* (x f) y)))
// (define zero () (setFraction (new Fraction) 0 1))
// (define one () (setFraction (new Fraction) 1 1))
// (define print () (begin (print x) (print y)))
// ; div-reduce and sign-reduce should not be exported
// (define div-reduce ()
// (begin
//     (if (= x 0)
//         (set y 1)
//         (begin
//             (set temp (gcd (abs x) y))
//             (set x (/ x temp))
//             (set y (/ y temp))))
//     self))
// (define sign-reduce ()
// (begin
//     (if (< y 0)
//         (begin (set x (- 0 x)) (set y (- 0 y)))
//         0)
//     self))
// )");
//     Evaluate("(define mkFraction (a b) (init (new Fraction) a b))");
//     Evaluate(@"
// (class Float Number ()
// (mant expo)
// (define init (m e) (begin (set mant m) (set expo e) self))
// (define mant () mant)
// (define expo () expo)
// (define + (x)
// (begin
//     (if (< expo (expo x))
//         (begin
//             (set min self)
//             (set max x))
//         (begin
//             (set min x)
//             (set max self)))
//     (set delta (- (expo max) (expo min)))
//     (set temp (+ (* (mant max) (powerof10 self delta)) (mant min)))
//     (normalize
//         (init (new Float) temp (if (= temp 0) 0 (expo min))))))
// (define negate () (init (new Float) (- 0 mant) expo))
// (define * (x)
// (normalize (init (new Float)
//     (* mant (mant x))
//     (+ expo (expo x)))))
// (define recip ()
// (if (isZero self) self
//     (normalize (init (new Float)
//         (/ 100000000 mant)
//         (- (- 0 8) expo)))))
// (define zero () (init (new Float) 0 0))
// (define one () (init (new Float) 1 0))
// (define = (x) (isZero (- self x)))
// (define < (x) (isNegative (- self x)))
// (define print () (begin (print mant) (print expo)))
// (define isZero () (= mant 0))
// (define isNegative () (< mant 0))
// ; normalize and powerof10 should not be exported
// (define powerof10 (d) (if (= d 0) 1 (* 10 (powerof10 self (- d 1)))))
// (define normalize ()
// (begin
//     (while (> (abs mant) 10000)
//         (begin
//             (set mant (/ mant 10))
//             (set expo (+ expo 1))))
//     self))
// )");
//     Evaluate("(define mkFloat (m e) (init (new Float) m e))");
//
//     // Fraction test from page 302.
//     Evaluate("(set eps (mkFraction 1 2))");
//     Evaluate("(set f1 (mkFraction 17 1))");
//     Assert.AreEqual("3437249\r\n833049", Evaluate("(sqrt f1 eps)"));
//
//     // Float test from page 304.
//     Evaluate("(set eps (mkFloat 5 -1))");
//     Evaluate("(set x1 (mkFloat 17 0))");
//     Assert.AreEqual("4125\r\n-3", Evaluate("(sqrt x1 eps)"));
//
//     // Exercise 6b)
//     globalInfo.LoadPreset("collection");    // For the Dictionary class.
//
//     EvaluateToISmalltalkValue("(set monthLengths (mkDictionary))");
//     EvaluateToISmalltalkValue("(at:put: monthLengths 1 31)");
//     EvaluateToISmalltalkValue("(at:put: monthLengths 2 28)");
//     EvaluateToISmalltalkValue("(at:put: monthLengths 3 31)");
//     EvaluateToISmalltalkValue("(at:put: monthLengths 4 30)");
//     EvaluateToISmalltalkValue("(at:put: monthLengths 5 31)");
//     EvaluateToISmalltalkValue("(at:put: monthLengths 6 30)");
//     EvaluateToISmalltalkValue("(at:put: monthLengths 7 31)");
//     EvaluateToISmalltalkValue("(at:put: monthLengths 8 31)");
//     EvaluateToISmalltalkValue("(at:put: monthLengths 9 30)");
//     EvaluateToISmalltalkValue("(at:put: monthLengths 10 31)");
//     EvaluateToISmalltalkValue("(at:put: monthLengths 11 30)");
//     EvaluateToISmalltalkValue("(at:put: monthLengths 12 31)");
//
//     Evaluate(@"
// (class Date Magnitude ()
// (year month day)
// (define init (y m d) (begin
// (set year y)
// (set month m)
// (set day d)
// self))
// (define year () year)
// (define month () month)
// (define day () day)
// (define < (x)
// (if (< year (year x)) true
// (if (> year (year x)) false
// (if (< month (month x)) true
// (if (> month (month x)) false
// (< day (day x)))))))
// (define isInLeapYear ()
// (if (= 0 (mod year 400)) true   ; The rules of the Gregorian calendar.
// (if (= 0 (mod year 100)) false
// (= 0 (mod year 4)))))
// (define +1 ()
// (let ((daysInCurrentMonth (+ (snd (associationAt: monthLengths month)) (if (and (= month 2) (isInLeapYear self)) 1 0))))
//     (if (< day daysInCurrentMonth) (mkDate year month (+1 day))
//     (if (< month 12) (mkDate year (+1 month) 1)
//     (mkDate (+1 year) 1 1)))))
// )");
//     Evaluate("(define mkDate (y m d) (init (new Date) y m d))");
//
//     // Test <
//     Evaluate("(set date1 (mkDate 2013 2 6))");
//     Assert.AreEqual(falseValueAsString, Evaluate("(< date1 (mkDate 2012 12 31))"));
//     Assert.AreEqual(trueValueAsString, Evaluate("(< date1 (mkDate 2014 1 1))"));
//     Assert.AreEqual(falseValueAsString, Evaluate("(< date1 (mkDate 2013 1 31))"));
//     Assert.AreEqual(trueValueAsString, Evaluate("(< date1 (mkDate 2013 3 1))"));
//     Assert.AreEqual(falseValueAsString, Evaluate("(< date1 (mkDate 2013 2 5))"));
//     Assert.AreEqual(trueValueAsString, Evaluate("(< date1 (mkDate 2013 2 7))"));
//     Assert.AreEqual(falseValueAsString, Evaluate("(< date1 date1)"));
//
//     // Test +1
//     Assert.AreEqual("28", Evaluate("(snd (associationAt: monthLengths 2))"));
//     Assert.AreEqual("2013\r\n2\r\n7", Evaluate("(+1 (mkDate 2013 2 6))"));
//     Assert.AreEqual("2013\r\n3\r\n1", Evaluate("(+1 (mkDate 2013 2 28))"));
//     Assert.AreEqual("2014\r\n1\r\n1", Evaluate("(+1 (mkDate 2013 12 31))"));
//     Assert.AreEqual("2000\r\n2\r\n29", Evaluate("(+1 (mkDate 2000 2 28))"));
//     Assert.AreEqual("1900\r\n3\r\n1", Evaluate("(+1 (mkDate 1900 2 28))"));
//     Assert.AreEqual("2012\r\n2\r\n29", Evaluate("(+1 (mkDate 2012 2 28))"));
//
//     // Exercise 6c)
//     Evaluate(@"
// (class Time Magnitude ()
// (hour minute second)
// (define init (h m s) (begin
// (set hour h)
// (set minute m)
// (set second s)
// self))
// (define hour () hour)
// (define minute () minute)
// (define second () second)
// (define < (x)
// (if (< hour (hour x)) true
// (if (> hour (hour x)) false
// (if (< minute (minute x)) true
// (if (> minute (minute x)) false
// (< second (second x)))))))
// (define +1 ()
// (if (< second 59) (mkTime hour minute (+1 second))
// (if (< minute 59) (mkTime hour (+1 minute) 0)
// (if (< hour 23) (mkTime (+1 hour) 0 0)
// (mkTime 0 0 0)))))
// )");
//     Evaluate("(define mkTime (h m s) (init (new Time) h m s))");
//
//     // Test <
//     Evaluate("(set time1 (mkTime 11 23 58))");
//     Assert.AreEqual(falseValueAsString, Evaluate("(< time1 time1)"));
//     Assert.AreEqual(falseValueAsString, Evaluate("(< time1 (mkTime 11 23 57))"));
//     Assert.AreEqual(trueValueAsString, Evaluate("(< time1 (mkTime 11 23 59))"));
//     Assert.AreEqual(falseValueAsString, Evaluate("(< time1 (mkTime 11 22 59))"));
//     Assert.AreEqual(trueValueAsString, Evaluate("(< time1 (mkTime 11 24 0))"));
//     Assert.AreEqual(falseValueAsString, Evaluate("(< time1 (mkTime 10 59 59))"));
//     Assert.AreEqual(trueValueAsString, Evaluate("(< time1 (mkTime 12 0 0))"));
//
//     // Test +1
//     Assert.AreEqual("11\r\n23\r\n59", Evaluate("(+1 (mkTime 11 23 58))"));
//     Assert.AreEqual("11\r\n24\r\n0", Evaluate("(+1 (mkTime 11 23 59))"));
//     Assert.AreEqual("12\r\n0\r\n0", Evaluate("(+1 (mkTime 11 59 59))"));
//     Assert.AreEqual("0\r\n0\r\n0", Evaluate("(+1 (mkTime 23 59 59))"));
// }
//
// [Test]
// public void AddBubbleDownTest() // 2013/12/04
// {
//     // This tests a bug fix to SmalltalkEnvironmentFrame<T>.AddBubbleDown(), where Add() used to be called unconditionally at the end of the function.
//     // NOTE: SmalltalkEnvironmentFrame<T>.AddBubbleDown() is not used, but this test tests the behaviour of "set" anyways.
//     Evaluate("(define test2 () foo)");
//     Evaluate(@"
// (define test1 () (begin
// (set foo 7) ; The buggy code was adding this variable to both the global and local frames.
// (set foo 13) ; The buggy code was modifying only the 'foo' in the local frame, since that's where AddBubbleDown() found 'foo' first.
// (test2) ; This returns the value of the 'foo' in the global frame.
// ))");
//
//     Assert.AreEqual("13", Evaluate("(test1)"));
// }
//
// [Test]
// public void TwoLocalEnvironmentsTest() // 2013/12/05
// {
//     Evaluate("(define test (x) (let ((y 13)) x))");
//
//     Assert.AreEqual("7", Evaluate("(test 7)"));
// }
//
// [Test]
// public void TwoLocalEnvironmentsSetTest() // 2013/12/05
// {
//     // To understand why this function contains two consecutive sets of the same variable, see the test AddBubbleDownTest().
//     Evaluate("(define test (x) (begin (let ((y 13)) (begin (set x 19) (set x 20))) x))");
//
//     Assert.AreEqual("20", Evaluate("(test 7)"));
// }
//
// [Test]
// public void ValueTypePredicatesTest()
// {
//     Assert.AreEqual(trueValueAsString, Evaluate("(number? 7)"));            // Number (integer)
//     Assert.AreEqual(trueValueAsString, Evaluate("(number? 7.5)"));          // Number (floating-point)
//     Assert.AreEqual(falseValueAsString, Evaluate("(number? #abc)"));        // Symbol
//     Assert.AreEqual(falseValueAsString, Evaluate("(number? $a)"));          // Character
//     Assert.AreEqual(falseValueAsString, Evaluate("(number? 'abc')"));       // String
//     Assert.AreEqual(falseValueAsString, Evaluate("(number? nil)"));         // Object
//     Assert.AreEqual(falseValueAsString, Evaluate("(number? #(2 3 5 7))"));  // Array
//
//     Assert.AreEqual(falseValueAsString, Evaluate("(symbol? 7)"));           // Number (integer)
//     Assert.AreEqual(falseValueAsString, Evaluate("(symbol? 7.5)"));         // Number (floating-point)
//     Assert.AreEqual(trueValueAsString, Evaluate("(symbol? #abc)"));         // Symbol
//     Assert.AreEqual(falseValueAsString, Evaluate("(symbol? $a)"));          // Character
//     Assert.AreEqual(falseValueAsString, Evaluate("(symbol? 'abc')"));       // String
//     Assert.AreEqual(falseValueAsString, Evaluate("(symbol? nil)"));         // Object
//     Assert.AreEqual(falseValueAsString, Evaluate("(symbol? #(2 3 5 7))"));  // Array
//
//     Assert.AreEqual(falseValueAsString, Evaluate("(char? 7)"));             // Number (integer)
//     Assert.AreEqual(falseValueAsString, Evaluate("(char? 7.5)"));           // Number (floating-point)
//     Assert.AreEqual(falseValueAsString, Evaluate("(char? #abc)"));          // Symbol
//     Assert.AreEqual(trueValueAsString, Evaluate("(char? $a)"));             // Character
//     Assert.AreEqual(falseValueAsString, Evaluate("(char? 'abc')"));         // String
//     Assert.AreEqual(falseValueAsString, Evaluate("(char? nil)"));           // Object
//     Assert.AreEqual(falseValueAsString, Evaluate("(char? #(2 3 5 7))"));    // Array
//
//     Assert.AreEqual(falseValueAsString, Evaluate("(string? 7)"));           // Number (integer)
//     Assert.AreEqual(falseValueAsString, Evaluate("(string? 7.5)"));         // Number (floating-point)
//     Assert.AreEqual(falseValueAsString, Evaluate("(string? #abc)"));        // Symbol
//     Assert.AreEqual(falseValueAsString, Evaluate("(string? $a)"));          // Character
//     Assert.AreEqual(trueValueAsString, Evaluate("(string? 'abc')"));        // String
//     Assert.AreEqual(falseValueAsString, Evaluate("(string? nil)"));         // Object
//     Assert.AreEqual(falseValueAsString, Evaluate("(string? #(2 3 5 7))"));  // Array
//
//     Assert.AreEqual(falseValueAsString, Evaluate("(object? 7)"));           // Number (integer)
//     Assert.AreEqual(falseValueAsString, Evaluate("(object? 7.5)"));         // Number (floating-point)
//     Assert.AreEqual(falseValueAsString, Evaluate("(object? #abc)"));        // Symbol
//     Assert.AreEqual(falseValueAsString, Evaluate("(object? $a)"));          // Character
//     Assert.AreEqual(falseValueAsString, Evaluate("(object? 'abc')"));       // String
//     Assert.AreEqual(trueValueAsString, Evaluate("(object? nil)"));          // Object
//     Assert.AreEqual(falseValueAsString, Evaluate("(object? #(2 3 5 7))"));  // Array
//
//     Assert.AreEqual(falseValueAsString, Evaluate("(array? 7)"));            // Number (integer)
//     Assert.AreEqual(falseValueAsString, Evaluate("(array? 7.5)"));          // Number (floating-point)
//     Assert.AreEqual(falseValueAsString, Evaluate("(array? #abc)"));         // Symbol
//     Assert.AreEqual(falseValueAsString, Evaluate("(array? $a)"));           // Character
//     Assert.AreEqual(falseValueAsString, Evaluate("(array? 'abc')"));        // String
//     Assert.AreEqual(falseValueAsString, Evaluate("(array? nil)"));          // Object
//     Assert.AreEqual(trueValueAsString, Evaluate("(array? #(2 3 5 7))"));    // Array
// }
//
// [Test]
// public void ToStringTest()
// {
//     Assert.AreEqual("7", Evaluate("(tostring 7)"));                 // Number (integer)
//     Assert.AreEqual("7.5", Evaluate("(tostring 7.5)"));             // Number (floating-point)
//     Assert.AreEqual("abc", Evaluate("(tostring #abc)"));            // Symbol
//     Assert.AreEqual("a", Evaluate("(tostring $a)"));                // Character
//     Assert.AreEqual("abc", Evaluate("(tostring 'abc')"));           // String
//     Assert.AreEqual("nil", Evaluate("(tostring nil)"));             // Object
//     Assert.AreEqual("2 3 5 7", Evaluate("(tostring #(2 3 5 7))"));  // Array
// }
//
// [Test]
// public void StringToSymbolTest()
// {
//     // We cannot create a symbol out of an empty string:
//     Assert.Throws<EvaluationException>(() => Evaluate("(stringtosymbol '')"));
//
//     Assert.AreEqual("abc", Evaluate("(set x (stringtosymbol 'abc'))"));
//     Assert.AreEqual(trueValueAsString, Evaluate("(symbol? x)"));
//     Assert.AreEqual(falseValueAsString, Evaluate("(string? x)"));
// }
//
// [Test]
// public void FloorTest()
// {
//     Assert.AreEqual("7", Evaluate("(floor 7.25)"));
//     Assert.AreEqual("13", Evaluate("(floor 13.75)"));   // "floor" does a floor, not a round.
//     Assert.AreEqual("-8", Evaluate("(floor -7.25)"));
// }
//
// [Test]
// public void ThrowTest()
// {
//     Assert.Throws<SmalltalkException>(() => Evaluate("(throw 'abc')"));
// }
//
// [Test]
// public void StringLessThanTest()
// {
//     Assert.AreEqual(falseValueAsString, Evaluate("(string< 'aaa' 'aaa')"));
//     Assert.AreEqual(trueValueAsString, Evaluate("(string< 'aaa' 'abc')"));
//     Assert.AreEqual(falseValueAsString, Evaluate("(string< 'abc' 'aaa')"));
//     Assert.AreEqual(trueValueAsString, Evaluate("(string< 'a' 'aaa')"));
//     Assert.AreEqual(trueValueAsString, Evaluate("(string< 'a' 'A')"));
// }
//
// [Test]
// public void StringLengthTest()
// {
//     Assert.AreEqual("0", Evaluate("(strlen '')"));
//     Assert.AreEqual("3", Evaluate("(strlen 'abc')"));
// }
//
// [Test]
// public void SubstringTest()
// {
//     Evaluate("(set s 'abcde')");
//
//     Assert.AreEqual("", Evaluate("(substr s 1 0)"));
//     Assert.AreEqual("bcd", Evaluate("(substr s 1 3)"));
//
//     Assert.Throws<EvaluationException>(() => Evaluate("(substr s -1 1)"));
//     Assert.Throws<EvaluationException>(() => Evaluate("(substr s 1 -1)"));
//     Assert.Throws<EvaluationException>(() => Evaluate("(substr s 1 20)"));
//     Assert.Throws<EvaluationException>(() => Evaluate("(substr s 20 1)"));
// }
//
// [Test]
// public void TypenameTest()
// {
//     Assert.AreEqual("int", Evaluate("(typename 7)"));
//     Assert.AreEqual("float", Evaluate("(typename 7.5)"));
//     Assert.AreEqual("symbol", Evaluate("(typename #abc)"));
//     Assert.AreEqual("char", Evaluate("(typename $a)"));
//     Assert.AreEqual("string", Evaluate("(typename 'abc')"));
//     Assert.AreEqual("UndefinedObject", Evaluate("(typename nil)"));
//     Assert.AreEqual("FalseValue", Evaluate("(typename false)"));
//     Assert.AreEqual("TrueValue", Evaluate("(typename true)"));
//     Assert.AreEqual("array", Evaluate("(typename #(2 3 5 7))"));
// }
//
// [Test]
// public void ReferenceEqualsTest()
// {
//     Evaluate("(set x 7)");
//     Evaluate("(set y 7)");
//
//     Assert.AreEqual(trueValueAsString, Evaluate("(ref= x x)"));
//
//     Assert.AreEqual(trueValueAsString, Evaluate("(= x y)"));
//     Assert.AreEqual(falseValueAsString, Evaluate("(ref= x y)"));
// }
//
// [Test]
// public void StringConcatenationTest()
// {
//     Assert.AreEqual(string.Empty, Evaluate("(strcat)"));
//     Assert.AreEqual("abc", Evaluate("(strcat 'abc')"));
//     Assert.AreEqual("abcde", Evaluate("(strcat 'abc' 'de')"));
//     Assert.AreEqual("7abc7.5de", Evaluate("(strcat 7 'abc' 7.5 #de)"));
//     Assert.Throws<EvaluationException>(() => Evaluate("(strcat nil)"));
// }
//
// [Test]
// public void ArrayValueTypeTest()
// {
//     Assert.AreEqual("0 0 0", Evaluate("(set a (newarray 3))"));
//     Assert.AreEqual("3", Evaluate("(arraylength a)"));
//
//     Assert.AreEqual("7", Evaluate("(arrayset a 3 7)"));
//     Assert.AreEqual("13", Evaluate("(arrayset a 2 13)"));
//     Assert.Throws<EvaluationException>(() => Evaluate("(arrayset a 0 91)"));
//     Assert.Throws<EvaluationException>(() => Evaluate("(arrayset a 4 91)"));
//
//     Assert.Throws<EvaluationException>(() => Evaluate("(arrayget a 0)"));
//     Assert.AreEqual("0", Evaluate("(arrayget a 1)"));
//     Assert.AreEqual("13", Evaluate("(arrayget a 2)"));
//     Assert.AreEqual("7", Evaluate("(arrayget a 3)"));
//     Assert.Throws<EvaluationException>(() => Evaluate("(arrayget a 4)"));
//
//     Assert.AreEqual("0 13 7", Evaluate("a"));
// }
//
// [Test]
// public void ArrayClassTest()
// {
//     globalInfo.LoadPreset("collection");
//
//     Evaluate("(set a (mkArray 11 3))"); // Element indices are 11-13
//     Assert.AreEqual("3", Evaluate("(size a)"));
//     Assert.AreEqual("11", Evaluate("(firstKey a)"));
//     Assert.AreEqual("13", Evaluate("(lastKey a)"));
//
//     Assert.AreEqual("7", Evaluate("(at:put: a 13 7)"));
//     Assert.AreEqual("13", Evaluate("(at:put: a 12 13)"));
//     Assert.AreEqual("nil", Evaluate("(at:put: a 10 91)"));
//     Assert.AreEqual("nil", Evaluate("(at:put: a 14 91)"));
//
//     Assert.AreEqual("nil", Evaluate("(at: a 10)"));
//     Assert.AreEqual("0", Evaluate("(at: a 11)"));
//     Assert.AreEqual("13", Evaluate("(at: a 12)"));
//     Assert.AreEqual("7", Evaluate("(at: a 13)"));
//     Assert.AreEqual("nil", Evaluate("(at: a 14)"));
//
//     Assert.AreEqual("0", Evaluate("(first a)"));
//     Assert.AreEqual("13", Evaluate("(next a)"));
//     Assert.AreEqual("7", Evaluate("(next a)"));
//     Assert.AreEqual("nil", Evaluate("(next a)"));
//
//     Assert.AreEqual(trueValueAsString, Evaluate("(isEmpty (mkArray 0 0))"));
//     Assert.AreEqual(falseValueAsString, Evaluate("(isEmpty a)"));
//
//     Assert.AreEqual(trueValueAsString, Evaluate("(includes: a 13)"));
//     Assert.AreEqual(falseValueAsString, Evaluate("(includes: a 14)"));
//
//     Assert.AreEqual(falseValueAsString, Evaluate("(includesKey: a 10)"));
//     Assert.AreEqual(trueValueAsString, Evaluate("(includesKey: a 11)"));
//     Assert.AreEqual(trueValueAsString, Evaluate("(includesKey: a 12)"));
//     Assert.AreEqual(trueValueAsString, Evaluate("(includesKey: a 13)"));
//     Assert.AreEqual(falseValueAsString, Evaluate("(includesKey: a 14)"));
//
//     Assert.AreEqual("13", Evaluate("(indexOf: a 7)"));
//     Assert.AreEqual("12", Evaluate("(indexOf: a 13)"));
//     Assert.AreEqual("nil", Evaluate("(indexOf: a 20)"));
// }
//
// [Test]
// public void LessThanTest()
// {
//     // Compare integer to integer:
//     Assert.AreEqual(trueValueAsString, Evaluate("(< 7 13)")); // Less than.
//     Assert.AreEqual(falseValueAsString, Evaluate("(< 7 7)")); // Equal.
//     Assert.AreEqual(falseValueAsString, Evaluate("(< 13 7)")); // Greater than.
//
//     // Compare integer to float:
//     Assert.AreEqual(trueValueAsString, Evaluate("(< 7 13.5)")); // Less than.
//     Assert.AreEqual(falseValueAsString, Evaluate("(< 7 7.0)")); // Equal.
//     Assert.AreEqual(falseValueAsString, Evaluate("(< 13 7.25)")); // Greater than.
//
//     // Compare float to integer:
//     Assert.AreEqual(trueValueAsString, Evaluate("(< 7.25 13)")); // Less than.
//     Assert.AreEqual(falseValueAsString, Evaluate("(< 7.0 7)")); // Equal.
//     Assert.AreEqual(falseValueAsString, Evaluate("(< 13.25 7)")); // Greater than.
//
//     // Compare float to float:
//     Assert.AreEqual(trueValueAsString, Evaluate("(< 7.25 13.5)")); // Less than.
//     Assert.AreEqual(falseValueAsString, Evaluate("(< 7.25 7.25)")); // Equal.
//     Assert.AreEqual(falseValueAsString, Evaluate("(< 13.5 7.25)")); // Greater than.
//
//     // Compare symbols:
//     Assert.AreEqual(trueValueAsString, Evaluate("(< #aaa #abc)")); // Less than.
//     Assert.AreEqual(falseValueAsString, Evaluate("(< #aaa #aaa)")); // Equal.
//     Assert.AreEqual(falseValueAsString, Evaluate("(< #abc #aaa)")); // Greater than.
//
//     // Compare characters:
//     Assert.AreEqual(trueValueAsString, Evaluate("(< $a $b)")); // Less than.
//     Assert.AreEqual(falseValueAsString, Evaluate("(< $a $a)")); // Equal.
//     Assert.AreEqual(falseValueAsString, Evaluate("(< $b $a)")); // Greater than.
//
//     // Compare strings:
//     Assert.AreEqual(trueValueAsString, Evaluate("(< 'aaa' 'abc')")); // Less than.
//     Assert.AreEqual(falseValueAsString, Evaluate("(< 'aaa' 'aaa')")); // Equal.
//     Assert.AreEqual(falseValueAsString, Evaluate("(< 'abc' 'aaa')")); // Greater than.
// }
//
// [Test]
// public void StackTest()
// {
//     globalInfo.LoadPreset("collection");
//
//     Evaluate("(set s (mkStack))");
//
//     Assert.AreEqual(trueValueAsString, Evaluate("(isEmpty s)"));
//     Assert.AreEqual("nil", Evaluate("(peek s)"));
//     Assert.AreEqual("nil", Evaluate("(pop s)"));
//
//     Evaluate("(push: s 2)");
//     Evaluate("(push: s 3)");
//     Evaluate("(push: s 5)");
//
//     Assert.AreEqual(falseValueAsString, Evaluate("(isEmpty s)"));
//     Assert.AreEqual("5", Evaluate("(peek s)"));
//     Assert.AreEqual("5", Evaluate("(pop s)"));
//     Assert.AreEqual(falseValueAsString, Evaluate("(isEmpty s)"));
//     Assert.AreEqual("3", Evaluate("(peek s)"));
//     Assert.AreEqual("3", Evaluate("(pop s)"));
//     Assert.AreEqual(falseValueAsString, Evaluate("(isEmpty s)"));
//     Assert.AreEqual("2", Evaluate("(peek s)"));
//     Assert.AreEqual("2", Evaluate("(pop s)"));
//     Assert.AreEqual(trueValueAsString, Evaluate("(isEmpty s)"));
//     Assert.AreEqual("nil", Evaluate("(peek s)"));
//     Assert.AreEqual("nil", Evaluate("(pop s)"));
// }
//
// [Test]
// public void QueueTest()
// {
//     globalInfo.LoadPreset("collection");
//
//     Evaluate("(set q (mkQueue))");
//
//     Assert.AreEqual(trueValueAsString, Evaluate("(isEmpty q)"));
//     Assert.AreEqual("nil", Evaluate("(peek q)"));
//     Assert.AreEqual("nil", Evaluate("(dequeue q)"));
//
//     Evaluate("(enqueue: q 2)");
//     Evaluate("(enqueue: q 3)");
//     Evaluate("(enqueue: q 5)");
//
//     Assert.AreEqual(falseValueAsString, Evaluate("(isEmpty q)"));
//     Assert.AreEqual("2", Evaluate("(peek q)"));
//     Assert.AreEqual("2", Evaluate("(dequeue q)"));
//     Assert.AreEqual(falseValueAsString, Evaluate("(isEmpty q)"));
//     Assert.AreEqual("3", Evaluate("(peek q)"));
//     Assert.AreEqual("3", Evaluate("(dequeue q)"));
//     Assert.AreEqual(falseValueAsString, Evaluate("(isEmpty q)"));
//     Assert.AreEqual("5", Evaluate("(peek q)"));
//     Assert.AreEqual("5", Evaluate("(dequeue q)"));
//     Assert.AreEqual(trueValueAsString, Evaluate("(isEmpty q)"));
//     Assert.AreEqual("nil", Evaluate("(peek q)"));
//     Assert.AreEqual("nil", Evaluate("(dequeue q)"));
// }
//
// [Test]
// public void PriorityQueueTest()
// {
//     globalInfo.LoadPreset("collection");
//
//     Evaluate("(set q (mkPriorityQueue))");
//
//     Assert.AreEqual(trueValueAsString, Evaluate("(isEmpty q)"));
//     Assert.AreEqual("nil", Evaluate("(peek q)"));
//     Assert.AreEqual("nil", Evaluate("(dequeue q)"));
//
//     Evaluate("(enqueue: q (mkAssociation 3 'three'))");
//     Evaluate("(enqueue: q (mkAssociation 7 'seven'))");
//     Evaluate("(enqueue: q (mkAssociation 2 'two'))");
//     Evaluate("(enqueue: q (mkAssociation 5 'five'))");
//
//     Assert.AreEqual(falseValueAsString, Evaluate("(isEmpty q)"));
//     Assert.AreEqual("2\r\ntwo", Evaluate("(peek q)"));
//     Assert.AreEqual("2\r\ntwo", Evaluate("(dequeue q)"));
//     Assert.AreEqual(falseValueAsString, Evaluate("(isEmpty q)"));
//     Assert.AreEqual("3\r\nthree", Evaluate("(peek q)"));
//     Assert.AreEqual("3\r\nthree", Evaluate("(dequeue q)"));
//     Assert.AreEqual(falseValueAsString, Evaluate("(isEmpty q)"));
//     Assert.AreEqual("5\r\nfive", Evaluate("(peek q)"));
//     Assert.AreEqual("5\r\nfive", Evaluate("(dequeue q)"));
//     Assert.AreEqual(falseValueAsString, Evaluate("(isEmpty q)"));
//     Assert.AreEqual("7\r\nseven", Evaluate("(peek q)"));
//     Assert.AreEqual("7\r\nseven", Evaluate("(dequeue q)"));
//     Assert.AreEqual(trueValueAsString, Evaluate("(isEmpty q)"));
//     Assert.AreEqual("nil", Evaluate("(peek q)"));
//     Assert.AreEqual("nil", Evaluate("(dequeue q)"));
// }
//
// [Test]
// public void SpaceshipTest() // <=> : The "spaceship" (CompareTo) operator from Perl.
// {
//     Evaluate(@"
// (define <=> (a b)
// (cond
// ((< a b) -1)
// ((< b a) 1)
// (true 0)
// )
// )");
//
//     Assert.AreEqual("-1", Evaluate("(<=> 7 13)"));
//     Assert.AreEqual("0", Evaluate("(<=> 7 7)"));
//     Assert.AreEqual("1", Evaluate("(<=> 13 7)"));
// }
//
// [Test]
// public void BooleanTest() // 2014/12/09 : Object-oriented Boolean operators
// {
//     Assert.AreEqual("false", Evaluate("(and false false)"));
//     Assert.AreEqual("false", Evaluate("(and false true)"));
//     Assert.AreEqual("false", Evaluate("(and true false)"));
//     Assert.AreEqual("true", Evaluate("(and true true)"));
//
//     Assert.AreEqual("false", Evaluate("(or false false)"));
//     Assert.AreEqual("true", Evaluate("(or false true)"));
//     Assert.AreEqual("true", Evaluate("(or true false)"));
//     Assert.AreEqual("true", Evaluate("(or true true)"));
//
//     Assert.AreEqual("false", Evaluate("(xor false false)"));
//     Assert.AreEqual("true", Evaluate("(xor false true)"));
//     Assert.AreEqual("true", Evaluate("(xor true false)"));
//     Assert.AreEqual("false", Evaluate("(xor true true)"));
//
//     Assert.AreEqual("true", Evaluate("(not false)"));
//     Assert.AreEqual("false", Evaluate("(not true)"));
// }
