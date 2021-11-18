// tom-weatherhead/thaw-grammar/src/languages/scheme/domain-object-model/scheme-global-info.ts

import { IParser, ITokenizer } from 'thaw-interpreter-types';

import { ArgumentException } from 'thaw-interpreter-core';

import { GlobalInfoBase } from '../../../common/domain-object-model/global-info-base';

// import { IExpression } from '../../../common/domain-object-model/iexpression';

import { FloatLiteral } from '../../lisp/domain-object-model/float-literal';
import { IntegerLiteral } from '../../lisp/domain-object-model/integer-literal';
// import { INumber } from '../../lisp/domain-object-model/inumber';
import { ISExpression } from '../../lisp/domain-object-model/isexpression';
import { LISPSymbol } from '../../lisp/domain-object-model/lisp-symbol';
import { NullSExpression } from '../../lisp/domain-object-model/null-sexpression';

export class SchemeGlobalInfo extends GlobalInfoBase<ISExpression> {
	// private readonly tokenizer: ITokenizer | undefined;
	// private readonly parser: IParser | undefined;
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

	protected loadSASLSafePresets(): void {
		// These presets do not use side effects: set, begin, while, etc.
		// this.globalEnvironment.add(varStackTrace, new NullSExpression());
		// this.globalEnvironment.add(new Variable<ISExpression>("e", 0, 0), new FloatLiteral(Math.E));
		// this.globalEnvironment.add(new Variable<ISExpression>("pi", 0, 0), new FloatLiteral(Math.PI));

		// Define commonly-used lambda expressions here.
		// Of particular importance are combine, compose, and curry.

		//Evaluate("(set combine (lambda (f sum zero) (lambda (l) (if (null? l) zero (sum (f (car l)) ((combine f sum zero) (cdr l)))))))"); // Version 1: see page 102
		this.evaluate(
			'(set combine (lambda (f sum zero) (letrec ((loop (lambda (l) (if (null? l) zero (sum (f (car l)) (loop (cdr l))))))) loop)))'
		); // Version 2, using letrec: see page 126
		this.evaluate('(set compose (lambda (f g) (lambda (x) (g (f x)))))');
		this.evaluate('(set curry (lambda (f) (lambda (x) (lambda (y) (f x y)))))');

		this.evaluate('(set compose2args (lambda (f g) (lambda (x y) (g (f x y)))))');
		this.evaluate('(set reverse2args (lambda (f) (lambda (x y) (f y x))))');

		this.evaluate('(set > (reverse2args <))');
		this.evaluate("(set not (lambda (x) (if x '() 'T)))");
		this.evaluate('(set and (lambda (x y) (if x y x)))');
		this.evaluate('(set or (lambda (x y) (if x x y)))');
		this.evaluate('(set mod (lambda (m n) (- m (* n (/ m n)))))');
		this.evaluate('(set gcd (lambda (m n) (if (= n 0) m (gcd n (mod m n)))))');
		//this.evaluate("(set atom? (lambda (x) (or (null? x) (or (number? x) (or (symbol? x) (string? x))))))"); // What about primop? and closure? ?
		this.evaluate('(set atom? (compose list? not))'); // Version 2
		//this.evaluate("(set equal (lambda (l1 l2) (if (atom? l1) (= l1 l2) (if (atom? l2) '() (if (equal (car l1) (car l2)) (equal (cdr l1) (cdr l2)) '())))))"); // Version 1
		this.evaluate(
			[
				'(set equal (lambda (l1 l2)',
				'(cond',
				'	((atom? l1) (= l1 l2))',
				"	((atom? l2) '())",
				'	((equal (car l1) (car l2)) (equal (cdr l1) (cdr l2)))',
				"	('T '())",
				')))'
			].join(' ')
		); // Version 2
		//this.evaluate("(set >= (lambda (x y) (not (< x y))))");
		this.evaluate('(set >= (compose2args < not))');
		//this.evaluate("(set <= (lambda (x y) (not (> x y))))");
		this.evaluate('(set <= (compose2args > not))');
		//this.evaluate("(set <> (lambda (x y) (not (= x y))))");
		this.evaluate('(set <> (compose2args = not))');
		this.evaluate("(set any (lambda (l) (if (null? l) '() (if (car l) 'T (any (cdr l))))))");
		this.evaluate(
			"(set all (lambda (l) (if (null? l) 'T (if (not (car l)) '() (all (cdr l))))))"
		);
		//this.evaluate("(set mapcar (lambda (f l) (if (null? l) '() (cons (f (car l)) (mapcar f (cdr l))))))"); // Original definition.
		this.evaluate('(set id (lambda (x) x))');
		this.evaluate("(set mapc (lambda (f) (combine f cons '())))"); // Second definition.
		this.evaluate('(set mapcar (lambda (f l) ((mapc f) l)))'); // Second definition.
		//this.evaluate("(set mapc (curry mapcar))");  // Original definition.  From page 101.
		this.evaluate("(set any2 (combine id or '()))");
		this.evaluate("(set all2 (combine id and 'T))");
		//this.evaluate("(set +1 (lambda (n) (+ n 1)))"); // Version 1
		this.evaluate('(set +1 ((curry +) 1))'); // Version 2
		//this.evaluate("(set append (lambda (l1 l2) (if (null? l1) l2 (cons (car l1) (append (cdr l1) l2)))))"); // Version 1
		this.evaluate('(set append (lambda (l1 l2) ((combine id cons l2) l1)))'); // Version 2
		this.evaluate(
			"(set reverse (lambda (l) (letrec ((rev-aux (lambda (l1 l2) (if (null? l1) l2 (rev-aux (cdr l1) (cons (car l1) l2)))))) (rev-aux l '()))))"
		);
		this.evaluate(
			'(set skip (lambda (n l) (if (or (null? l) (= n 0)) l (skip (- n 1) (cdr l)))))'
		);
		this.evaluate(
			"(set take (lambda (n l) (if (or (null? l) (= n 0)) '() (cons (car l) (take (- n 1) (cdr l))))))"
		);
		this.evaluate('(set abs (lambda (n) (if (< n 0) (- 0 n) n)))');
		//this.evaluate("(set cadr (lambda (l) (car (cdr l))))"); // Version 1
		this.evaluate('(set cadr (compose cdr car))'); // Version 2
		this.evaluate('(set length (lambda (l) (if (null? l) 0 (+1 (length (cdr l))))))'); // Adapted from page 29.
		/*
		this.evaluate(@"
(set find (lambda (pred lis) ; From page 104
(if (null? lis) '()
	(if (pred (car lis)) 'T (find pred (cdr lis))))))"); // Version 1
		 */
		this.evaluate(
			[
				'(set find (lambda (pred lis)',
				'(cond',
				"	((null? lis) '())",
				"	((pred (car lis)) 'T)",
				"	('T (find pred (cdr lis)))",
				')',
				'))'
			].join(' ')
		); // Version 2
		this.evaluate('(set nth (lambda (n l) (if (= n 0) (car l) (nth (- n 1) (cdr l)))))'); // Adapted from page 43.
		/* TODO:
		Evaluate("");
		 */
	}

	public override loadPreset(presetName: string): string {
		if (typeof this.tokenizer === 'undefined') {
			throw new Error('SchemeGlobalInfo.loadPreset() : this.tokenizer is undefined.');
		} else if (typeof this.parser === 'undefined') {
			throw new Error('SchemeGlobalInfo.loadPreset() : this.parser is undefined.');
		}

		// globalInfo.LoadPreset("assoc");
		// globalInfo.LoadPreset("select");
		// globalInfo.LoadPreset("flatten");

		switch (
			presetName // presetName.ToLower()
		) {
			case 'assoc':
				// Association list functions (adapted from page 32)
				this.evaluate('(set caar (compose car car))');
				this.evaluate('(set cadar (compose car cadr))');
				this.evaluate(
					[
						'(set assoc (lambda (x alist)',
						'	(cond',
						"		((null? alist) '())",
						'		((= x (caar alist)) (cadar alist))',
						"		('T (assoc x (cdr alist)))",
						'	)',
						'))'
					].join(' ')
				);
				this.evaluate(
					[
						'(set mkassoc (lambda (x y alist)',
						'	(cond',
						'		((null? alist) (list (list x y)))',
						'		((= x (caar alist)) (cons (list x y) (cdr alist)))',
						"		('T (cons (car alist) (mkassoc x y (cdr alist))))",
						'	)',
						'))'
					].join(' ')
				);
				// Additional function
				/*
		                    this.evaluate(@"
		(set assoc-contains-key (lambda (x alist)
		    (if (null? alist) '()
		        (if (= x (caar alist)) 'T
		            (assoc-contains-key x (cdr alist))))))");
		                     */
				this.evaluate(
					'(set assoc-contains-key (lambda (x alist) (find (compose car ((curry =) x)) alist)))'
				);
				// Adapted from page 55
				this.evaluate(
					[
						'(set rplac-assoc (lambda (x y alist)',
						'(cond',
						"((null? alist) '())",
						'((= x (caar alist)) (rplacd (car alist) (list y)))',
						'((null? (cdr alist)) (rplacd alist (list (list x y))))',
						"('T (rplac-assoc x y (cdr alist)))",
						')',
						'))'
					].join(' ')
				);
				break;

			case 'queue':
				// Queue functions (adapted from page 37)
				this.evaluate("(set empty-queue '())");
				this.evaluate('(set front car)');
				this.evaluate('(set rm-front cdr)');
				//this.evaluate("(set enqueue (lambda (t q) (if (null? q) (list t) (cons (car q) (enqueue t (cdr q))))))"); // Version 1
				this.evaluate('(set enqueue (lambda (t q) (append q (list t))))'); // Version 2; 2013/11/30
				this.evaluate('(set empty? null?)');
				break;

			case 'compose': // From page 104
				//this.evaluate("(set compose (lambda (f g) (lambda (x) (g (f x)))))");
				break;

			case 'set':
				// Scheme set functions; from pages 104-105
				this.evaluate("(set nullset '())");
				this.evaluate('(set member? (lambda (x s) (find ((curry equal) x) s)))');
				this.evaluate('(set addelt (lambda (x s) (if (member? x s) s (cons x s))))');
				this.evaluate('(set union (lambda (s1 s2) ((combine id addelt s1) s2)))');
				break;

			case 'select':
				this.evaluate(
					[
						'(set select (lambda (indices l)',
						'(letrec ((select* (lambda (n indices l)',
						'(cond',
						"((or (null? indices) (null? l)) '())",
						'((= n (car indices)) (cons (car l) (select* (+1 n) (cdr indices) (cdr l))))',
						"('T (select* (+1 n) indices (cdr l)))))))",
						'(select* 0 indices l))))'
					].join(' ')
				);
				break;

			case 'flatten':
				this.evaluate(
					[
						'(set flatten (lambda (tree)',
						"(if (null? tree) '()",
						'(if (atom? tree) (list tree)',
						'(append (flatten (car tree)) (flatten (cdr tree)))))))'
					].join(' ')
				);
				break;

			case 'sublist':
				this.evaluate(
					[
						'(set sublist (lambda (l start len)',
						'(cond',
						"((or (<= len 0) (null? l)) '())",
						'((> start 0) (sublist (cdr l) (- start 1) len))',
						"('T (cons (car l) (sublist (cdr l) 0 (- len 1)))))))"
					].join(' ')
				);
				this.evaluate(
					[
						'(set removesublist (lambda (l start len)',
						'(cond',
						'((or (<= len 0) (null? l)) l)',
						'((> start 0) (cons (car l) (removesublist (cdr l) (- start 1) len)))',
						"('T (removesublist (cdr l) 0 (- len 1))))))"
					].join(' ')
				);
				break;

			case 'substring':
				this.loadPreset('sublist');
				this.evaluate(
					'(set substring (lambda (str start len) (listtostring (sublist (stringtolist str) start len))))'
				);
				this.evaluate(
					'(set removesubstring (lambda (str start len) (listtostring (removesublist (stringtolist str) start len))))'
				);
				break;

			case 'stack':
				this.evaluate("(set empty-stack '())");
				this.evaluate('(set push cons)');
				this.evaluate('(set peek car)');
				this.evaluate('(set pop cdr)');
				this.evaluate('(set empty-stack? null?)');
				break;

			case 'filter':
				this.evaluate(
					[
						'(set filter (lambda (pred l)', // ; Returns only the elements of l for which pred is true.
						'(cond',
						"((null? l) '())",
						'((pred (car l)) (cons (car l) (filter pred (cdr l))))',
						"('T (filter pred (cdr l)))",
						')',
						'))'
					].join(' ')
				);
				this.evaluate(
					[
						'(set remove (lambda (x l)', // ; Returns a copy of l that has had all occurrences of x removed.
						'(filter (compose ((curry =) x) not) l)',
						'))'
					].join(' ')
				);
				break;

			case 'sort':
				this.evaluate(
					[
						'(set insertion-sort (lambda (lessthan)',
						'(letrec',
						'(',
						'(insert (lambda (x l)',
						'(cond',
						'((null? l) (list x))',
						'((lessthan x (car l)) (cons x l))',
						"('T (cons (car l) (insert x (cdr l))))",
						')',
						'))',
						')',
						"(combine id insert '())",
						')',
						'))'
					].join(' ')
				);
				this.evaluate(
					[
						'(set quicksort (lambda (lessthan)',
						'(letrec',
						'(',
						'(partition (lambda (pivot-element l lessthanlist notlessthanlist)',
						'(cond',
						'((null? l) (list lessthanlist notlessthanlist))',
						'((lessthan (car l) pivot-element) (partition pivot-element (cdr l) (cons (car l) lessthanlist) notlessthanlist))',
						"('T (partition pivot-element (cdr l) lessthanlist (cons (car l) notlessthanlist)))",
						')',
						'))',
						'(qs (lambda (l)',
						'(if (< (length l) 2) l',
						"(let ((partitioned-lists (partition (car l) (cdr l) '() '())))",
						'(append (qs (car partitioned-lists)) (cons (car l) (qs (cadr partitioned-lists))))',
						')',
						')',
						'))',
						')',
						'qs',
						')',
						'))'
					].join(' ')
				);
				this.evaluate(
					[
						'(set merge-sort (lambda (lessthan)',
						'(letrec',
						'(',
						'(merge (lambda (l1 l2 reversed-result)',
						'(cond',
						'((null? l1) (append (reverse reversed-result) l2))',
						'((null? l2) (append (reverse reversed-result) l1))',
						'((lessthan (car l1) (car l2)) (merge (cdr l1) l2 (cons (car l1) reversed-result)))',
						"('T (merge l1 (cdr l2) (cons (car l2) reversed-result)))",
						')',
						'))',
						'(cut-list (lambda (l)',
						'(let ((len (/ (length l) 2)))',
						'(list (take len l) (skip len l))',
						')',
						'))',
						'(ms (lambda (l)',
						'(if (< (length l) 2) l',
						'(let ((lists (cut-list l)))',
						"(merge (ms (car lists)) (ms (cadr lists)) '())",
						')',
						')',
						'))',
						')',
						'ms',
						')',
						'))'
					].join(' ')
				);

				break;

			default:
				throw new Error(`loadPreset() : Unknown preset name '${presetName}'.`);
		}

		return `The preset '${presetName}' has been successfully loaded.`;
	}

	public override loadPresets(): void {
		// Define commonly-used lambda expressions here.
		this.loadSASLSafePresets();

		// And now we can load any SASL-unsafe, Scheme-only presets below.
		/* TODO:
		Evaluate("");
		 */
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
		// return (value as IntegerLiteral) !== undefined; // No.

		return value instanceof IntegerLiteral;
	}

	public valueAsInteger(value: ISExpression): number {
		// TODO: 2019-12-22 : It looks like we need to combine ISExpression and INumber:

		// const valueAsNumber = value as INumber;
		// const valueAsInt = value as IntegerLiteral;

		/*
		// if (valueAsInt === undefined) {
		if (!(value instanceof IntegerLiteral)) {
			throw new ArgumentException('valueAsInteger() : The value is not an IntegerLiteral.', 'value');
		}

		// return valueAsInt.toInteger();

		return (value as IntegerLiteral).toInteger();
		 */

		if (value instanceof IntegerLiteral) {
			return (value as IntegerLiteral).toInteger();
		} else if (value instanceof FloatLiteral) {
			return (value as FloatLiteral).toInteger();
		} else {
			throw new ArgumentException(
				'valueAsInteger() : The value is neither an IntegerLiteral nor a FloatLiteral.',
				'value'
			);
		}
	}

	public integerAsValue(value: number): ISExpression {
		return new IntegerLiteral(value);
	}

	public valueIsNumber(value: ISExpression): boolean {
		// return (value as IntegerLiteral) !== undefined;

		// return (value as IntegerLiteral) !== undefined || (value as FloatLiteral) !== undefined;

		// return (value as INumber) !== undefined;

		// return value instanceof INumber;

		return value.isNumber();
	}

	public valueAsNumber(value: ISExpression): number {
		// return (value as INumber).toDouble();

		/*
		const i = value as IntegerLiteral;
		const f = value as FloatLiteral;

		if (i !== undefined) {
			return i.value;
		} else if (f !== undefined) {
			return f.value;
		} else {
			throw new ArgumentException('valueAsNumber() : The value is neither an IntegerLiteral nor a FloatLiteral.', 'value');
		}
		 */

		if (value instanceof IntegerLiteral) {
			return (value as IntegerLiteral).value;
		} else if (value instanceof FloatLiteral) {
			return (value as FloatLiteral).value;
		} else {
			throw new ArgumentException(
				'valueAsNumber() : The value is neither an IntegerLiteral nor a FloatLiteral.',
				'value'
			);
		}
	}

	public numberAsIntegerValue(value: number): ISExpression {
		// Convert to the language's native integer data type
		return new IntegerLiteral(value);
	}

	public numberAsFloatValue(value: number): ISExpression {
		// Convert to the language's native floating-point number data type
		return new FloatLiteral(value);
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

	// public evaluate(str: string): ISExpression {
	// 	if (typeof this.tokenizer === 'undefined') {
	// 		throw new Error('SchemeGlobalInfo.evaluate() : this.tokenizer is undefined.');
	// 	} else if (typeof this.parser === 'undefined') {
	// 		throw new Error('SchemeGlobalInfo.evaluate() : this.parser is undefined.');
	// 	}
	//
	// 	const parseResult = this.parser.parse(this.tokenizer.tokenize(str));
	// 	const expr = parseResult as IExpression<ISExpression>;
	//
	// 	return expr.evaluate(this.globalEnvironment, this);
	// }
	//
	// public evaluateToString(str: string): string {
	// 	return this.evaluate(str).toString();
	// }
}
