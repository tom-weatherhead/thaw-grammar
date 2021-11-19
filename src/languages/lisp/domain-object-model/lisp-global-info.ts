// tom-weatherhead/thaw-grammar/src/languages/lisp/domain-object-model/lisp-global-info.ts

import { IParser, ITokenizer } from 'thaw-interpreter-types';

import { ArgumentException } from 'thaw-interpreter-core';

import { GlobalInfoBase } from '../../../common/domain-object-model/global-info-base';

import { IntegerLiteral, isIntegerLiteral } from './integer-literal';
// import { INumber } from './inumber';
import { ISExpression } from './isexpression';
import { LISPSymbol } from './lisp-symbol';
import { NullSExpression } from './null-sexpression';

export class LISPGlobalInfo extends GlobalInfoBase<ISExpression> {
	private readonly trueValueForAccessor: ISExpression = new LISPSymbol('T'); // Symbols are immutable
	private readonly falseValueForAccessor: ISExpression = new NullSExpression(); // This is immutable too
	// private readonly Dictionary<Name, IMacroDefinition<ISExpression>> MacroDefs = new Dictionary<Name, IMacroDefinition<ISExpression>>();
	// public static readonly Variable<ISExpression> varStackTrace = new Variable<ISExpression>("__STACK_TRACE__", 0, 0);

	constructor(
		options: {
			parser?: IParser;
			tokenizer?: ITokenizer;
		} = {}
	) {
		super(options);
	}

	public override loadPreset(presetName: string): string {
		switch (presetName) {
			// 			case "assoc":
			// 				// Association list functions (from page 32)
			// 				this.evaluate("(define caar (x) (car (car x)))");
			// 				this.evaluate("(define cadar (x) (car (cdr (car x))))");
			// 				this.evaluate(@"
			// (define assoc (x alist)
			// (if (null? alist) '()
			// 	(if (= x (caar alist)) (cadar alist)
			// 		(assoc x (cdr alist)))))");
			// 				this.evaluate(@"
			// (define mkassoc (x y alist)
			// (if (null? alist)
			// 	(list (list x y))
			// 	(if (= x (caar alist)) (cons (list x y) (cdr alist))
			// 		(cons (car alist) (mkassoc x y (cdr alist))))))");
			// 				// Additional function
			// 				this.evaluate(@"
			// (define assoc-contains-key (x alist)
			// (if (null? alist) '()
			// 	(if (= x (caar alist)) 'T
			// 		(assoc-contains-key x (cdr alist)))))");
			// 				// From page 55
			// 				this.evaluate(@"
			// (define rplac-assoc (x y alist)
			// (if (null? alist) '()
			// 	(if (= x (caar alist))
			// 		(rplacd (car alist) (list y))
			// 		(if (null? (cdr alist))
			// 			(rplacd alist (list (list x y)))
			// 			(rplac-assoc x y (cdr alist))))))");
			// 				break;

			// 			case "set":
			// 				// Set functions (from page 34)
			// 				this.evaluate("(set nullset '())");
			// 				this.evaluate(@"
			// (define member? (x s)
			// (if (null? s) '()
			// 	(if (equal x (car s)) 'T (member? x (cdr s)))))");
			// 				this.evaluate("(define addelt (x s) (if (member? x s) s (cons x s)))");
			// 				this.evaluate("(define size (s) (length s))");
			// 				this.evaluate(@"
			// (define union (s1 s2)
			// (if (null? s1) s2
			// 	(if (member? (car s1) s2)
			// 		(union (cdr s1) s2)
			// 		(cons (car s1) (union (cdr s1) s2)))))");
			// 				// Additional set functions (from page 43)
			// 				this.evaluate(@"
			// (define inter (s1 s2)
			// (if (null? s1) s1
			// 	(if (member? (car s1) s2)
			// 		(cons (car s1) (inter (cdr s1) s2))
			// 		(inter (cdr s1) s2))))");
			// 				this.evaluate(@"
			// (define diff (s1 s2)
			// (if (null? s1) s1
			// 	(if (null? s2) s1
			// 		(if (member? (car s1) s2)
			// 			(diff (cdr s1) s2)
			// 			(cons (car s1) (diff (cdr s1) s2))))))");
			// 				break;

			case 'queue':
				// Queue functions (from page 37)
				this.evaluate("(set empty-queue '())");
				this.evaluate('(define front (q) (car q))');
				this.evaluate('(define rm-front (q) (cdr q))');
				this.evaluate(
					'(define enqueue (t q) (if (null? q) (list t) (cons (car q) (enqueue t (cdr q)))))'
				);
				this.evaluate('(define empty? (q) (null? q))');
				break;

			default:
				return super.loadPreset(presetName);
		}

		return `The preset '${presetName}' has been successfully loaded.`;
	}

	public override loadPresets(): void {
		// 		GlobalEnvironment.Add(varStackTrace, new NullSExpression());
		// 		GlobalEnvironment.Add(new Variable<ISExpression>("e", 0, 0), new FloatLiteral(Math.E));
		// 		GlobalEnvironment.Add(new Variable<ISExpression>("pi", 0, 0), new FloatLiteral(Math.PI));

		// 		Evaluate("(define > (x y) (< y x))");
		this.evaluate("(define not (x) (if x '() 'T))"); // Page 30
		this.evaluate('(define and (x y) (if x y x))'); // Page 30
		this.evaluate('(define or (x y) (if x x y))'); // Page 30
		this.evaluate('(define atom? (x) (or (null? x) (or (number? x) (symbol? x))))'); // Page 31
		// 		this.evaluate(@"
		// (define equal (l1 l2)
		// (if (atom? l1) (= l1 l2)
		// 	(if (atom? l2) '()
		// 		(if (equal (car l1) (car l2))
		// 			(equal (cdr l1) (cdr l2))
		// 			'()))))"); // Page 31
		this.evaluate('(define +1 (n) (+ n 1))');
		this.evaluate(
			'(define append (list1 list2) (if (null? list1) list2 (cons (car list1) (append (cdr list1) list2))))'
		); // Similar to Page 45
		this.evaluate('(define length (l) (if (null? l) 0 (+1 (length (cdr l)))))'); // Page 29
		this.evaluate('(define nth (n l) (if (= n 0) (car l) (nth (- n 1) (cdr l))))'); // Page 43
		this.evaluate('(define mod (m n) (- m (* n (/ m n))))'); // Page 8
		this.evaluate('(define gcd (m n) (if (= n 0) m (gcd n (mod m n))))'); // Page 8
	}

	public get falseValue(): ISExpression {
		return this.falseValueForAccessor;
	}

	public get trueValue(): ISExpression {
		return this.trueValueForAccessor;
	}

	public override valueIsFalse(value: ISExpression): boolean {
		return value.isNull();
	}

	// public override Dictionary<Name, IMacroDefinition<ISExpression>> MacroDefinitions
	// {
	// 	get
	// 	{
	// 		return MacroDefs;
	// 	}
	// }

	public valueIsInteger(value: ISExpression): boolean {
		// return (value as IntegerLiteral) !== undefined;

		return isIntegerLiteral(value);
	}

	public valueAsInteger(value: ISExpression): number {
		if (!isIntegerLiteral(value)) {
			throw new ArgumentException(
				'valueAsInteger() : The value is not an IntegerLiteral.',
				'value'
			);
		}

		return value.toInteger();
	}

	public integerAsValue(value: number): ISExpression {
		return new IntegerLiteral(value);
	}

	// public static CreateStackTraceInNewEnvironmentFrame(EnvironmentFrame<ISExpression> oldEnvFrame, EnvironmentFrame<ISExpression> newEnvFrame,
	// 	int line, int column): void {
	// 	var oldStackTrace = oldEnvFrame.Lookup(varStackTrace);
	// 	var list1 = new SExpressionList(new IntegerLiteral(column), new NullSExpression());
	// 	var list2 = new SExpressionList(new IntegerLiteral(line), list1);
	// 	var newStackTrace = new SExpressionList(list2, oldStackTrace);

	// 	newEnvFrame.Add(varStackTrace, newStackTrace); // Add(), not AddBubbleDown().
	// 	//Console.WriteLine("CreateStackTraceInNewEnvironmentFrame(): Added (line, column) = ({0}, {1}).", line, column);
	// 	//Console.WriteLine("newStackTrace = {0}", newStackTrace);
	// }

	public override setDebug(debug: boolean): boolean {
		this.debug = debug;

		return true;
	}
}
