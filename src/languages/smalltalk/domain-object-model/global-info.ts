// tom-weatherhead/thaw-grammar/src/languages/smalltalk/domain-object-model/global-info.ts

// public class SmalltalkGlobalInfo : IGlobalInfoOps
// {
// 	private readonly ITokenizer Tokenizer;
// 	private readonly IParser Parser;
// 	public readonly ISmalltalkValue ZeroValue = new SmalltalkIntegerValue(0);
// 	private ISmalltalkValue FalseVal = null;
// 	private ISmalltalkValue TrueVal = null;
// 	public readonly SmalltalkEnvironmentFrame GlobalEnvironment = new SmalltalkEnvironmentFrame(null);
// 	public readonly Dictionary<string, SmalltalkFunctionDefinition> FunctionDefinitions = new Dictionary<string, SmalltalkFunctionDefinition>();
// 	public readonly Dictionary<string, SmalltalkClass> ClassDict = new Dictionary<string, SmalltalkClass>();
// 	public readonly SmalltalkUserValue ObjectInstance;  // Passed to Evaluate() by the interpreter; see Kamin pages 297-298.
// 	public readonly Random RandomNumberGenerator = new Random();
// 	private const string NilVariableName = "nil";
// 	public const string NilValueAsString = "nil";
// 	private const string FalseValueClassName = "FalseValue";
// 	private const string FalseVariableName = "false";
// 	public const string FalseValueAsString = "false"; //"0"; // Change this to "false"
// 	private const string TrueValueClassName = "TrueValue";
// 	private const string TrueVariableName = "true";
// 	public const string TrueValueAsString = "true"; //"1"; // Change this to "true"
// 	private readonly HashSet<string> LoadedPresets = new HashSet<string>();
//
// 	public SmalltalkGlobalInfo(ITokenizer t, IParser p)
// 	{
// 		Tokenizer = t;
// 		Parser = p;
//
// 		// These are temporary values for FalseVal and TrueVal; hopefully they are not used.
// 		//FalseVal = ZeroValue;
// 		//TrueVal = new SmalltalkIntegerValue(1);
//
// 		var objectClass = SmalltalkObjectClassKeeper.ObjectClass;
// 		var objectInstanceEnvFrame = new SmalltalkEnvironmentFrame(null);
//
// 		objectClass.AddFunction(t, p, string.Format("(define isNil () {0})", FalseVariableName));
// 		objectClass.AddFunction(t, p, string.Format("(define notNil () {0})", TrueVariableName));
//
// 		ClassDict[objectClass.ClassName] = objectClass;
// 		objectInstanceEnvFrame.Add(SmalltalkObjectClassKeeper.SelfVar, ZeroValue);
// 		ObjectInstance = new SmalltalkUserValue(objectClass, objectInstanceEnvFrame);
// 		ObjectInstance.Value.Dict[SmalltalkObjectClassKeeper.SelfVar] = ObjectInstance;
// 	}
//
// 	public void Clear()
// 	{
// 		GlobalEnvironment.Dict.Clear();
// 		FunctionDefinitions.Clear();
// 		ClassDict.Clear();
// 		LoadedPresets.Clear();
//
// 		ClassDict[SmalltalkObjectClassKeeper.ObjectClass.ClassName] = SmalltalkObjectClassKeeper.ObjectClass;
// 	}
//
// 	private void Evaluate(string input)
// 	{
// 		var expr = Parser.Parse(Tokenizer.Tokenize(input)) as ISmalltalkExpression;
//
// 		if (expr == null)
// 		{
// 			throw new Exception(string.Format("SmalltalkGlobalInfo.Evaluate() : Parse failed; input is: {0}", input));
// 		}
//
// 		expr.Evaluate(null, ObjectInstance, null, this);
// 	}
//
// 	public string LoadPreset(string presetName)
// 	{
// 		var presetNameToLower = presetName.ToLower();
//
// 		if (LoadedPresets.Contains(presetNameToLower))
// 		{
// 			return string.Format("The preset '{0}' has already been loaded.", presetName);
// 		}
//
// 		switch (presetNameToLower)
// 		{
// 			case "collection":
// 				// From Kamin page 283.
// 				const string collectionClass = @"
// (class Collection Object ()
// () ; abstract class
// (define first () #subclassResponsibility)
// (define next () #subclassResponsibility)
// (define add: (item) #subclassResponsibility)
// (define size ()
// 	(let ((tempitem (first self)) ; This has been modified to use 'let'.
// 		  (tempsize 0))
// 		(begin
// 			(while (notNil tempitem)
// 				(begin
// 					(set tempsize (+1 tempsize))
// 					(set tempitem (next self))))
// 			tempsize)))
// (define isEmpty () (isNil (first self)))
// (define includes: (item)
// 	(let ((tempitem (first self))
// 		  (found false))
// 		(begin
// 			(while (and (notNil tempitem) (not found))
// 				(if (= tempitem item)
// 					(set found true)
// 					(set tempitem (next self))))
// 			found)))
// ; The next three methods are described in Exercise 3 on page 345.
// (define asSet ()
// 	(let ((result (mkSet))
// 		  (tempitem (first self)))
// 		(begin
// 			(while (notNil tempitem)
// 				(begin
// 					(add: result tempitem)
// 					(set tempitem (next self))))
// 			result)))
// (define occurrencesOf: (item)
// 	(let ((tempitem (first self))
// 		  (count 0))
// 		(begin
// 			(while (notNil tempitem)
// 				(begin
// 					(if (= item tempitem)
// 						(set count (+1 count))
// 						0) ; The 0 is essentially a no-op.
// 					(set tempitem (next self))))
// 			count)))
// (define addAll: (collection)
// 	(let ((tempitem (first collection)))
// 		(begin
// 			(while (notNil tempitem)
// 				(begin
// 					(add: self tempitem)
// 					(set tempitem (next collection)))))))
// )";
//
// 				// From Kamin page 286.
// 				const string keyedCollectionClass = @"
// (class KeyedCollection Collection ()
// () ; abstract class
// (define at:put: (key value) #subclassResponsibility)
// (define currentKey () #subclassResponsibility)
// (define at: (key)
// 	(begin
// 		(set tempvalue (first self))
// 		(set found false)
// 		(while (and (notNil tempvalue) (not found))
// 			(if (= key (currentKey self))
// 				(set found true)
// 				(set tempvalue (next self))))
// 		tempvalue)) ; note: nil if key out of range
// (define includesKey: (key) (notNil (at: self key)))
// (define indexOf: (value)
// 	(begin
// 		(set tempvalue (first self))
// 		(set found false)
// 		(while (and (notNil tempvalue) (not found))
// 			(if (= value tempvalue)
// 				(set found true)
// 				(set tempvalue (next self))))
// 		(if (isNil tempvalue) nil (currentKey self))))
// )";
//
// 				// From Kamin page 289.
// 				const string sequenceableCollectionClass = @"
// (class SequenceableCollection KeyedCollection ()
// () ; abstract class
// (define firstKey () #subclassResponsibility)
// (define lastKey () #subclassResponsibility)
// (define last () (at: self (lastKey self)))
// (define at: (index)
// 	(begin
// 		(set iterations (- index (firstKey self)))
// 		(set result (first self))
// 		(while (> iterations 0)
// 			(begin
// 				(set result (next self))
// 				(set iterations (- iterations 1))))
// 		result))
// )";
//
// 				// From Kamin page 290.
// 				const string listClass = @"
// (class List SequenceableCollection ()
// (car cdr currentKey currentCell)
// (define car () car)
// (define cdr () cdr)
// (define init () (begin (set car nil) self)) ; super allows us to use a uniform init instead of initList et al.
// (define add: (item)
// 	(let ((temp (newEmptyCollection self))) ; See page 308.
// 		(begin
// 			(car: temp car)
// 			(cdr: temp cdr)
// 			(set cdr temp)
// 			(set car item))))
// (define newEmptyCollection () (init (new List))) ; See page 308.
// (define car: (x) (set car x))
// (define cdr: (x) (set cdr x))
// (define first ()
// 	(begin
// 		(set currentKey 1)
// 		(set currentCell self)
// 		car))
// (define next ()
// 	(if (isNil (car currentCell)) nil
// 		(begin
// 			(set currentKey (+1 currentKey))
// 			(set currentCell (cdr currentCell))
// 			(car currentCell))))
// (define firstKey () 1)
// (define lastKey () (size self))
// (define currentKey () currentKey)
// (define at:put: (n value)
// 	(if (= n 1) (set car value)
// 		(at:put: cdr (- n 1) value)))
// (define removeFirst ()
// 	(if (isEmpty self) self ; do nothing
// 		(begin
// 			(set car (car cdr))
// 			(set cdr (cdr cdr)))))
// (define zerolist (size)
// 	(while (> size 0)
// 		(begin
// 			(add: self 0)
// 			(set size (- size 1)))))
// )";
//
// 				// From Kamin page 283.
// 				const string setClass = @"
// (class Set Collection ()
// (members) ; list of elements
// (define init () (begin (set members (mkList)) self))
// (define first () (first members))
// (define next () (next members))
// (define add: (item)
// 	(if (includes: members item) self (add: members item)))
// )";
//
// 				// From Kamin page 286.
// 				const string associationClass = @"
// (class Association Object ()
// (fst snd)
// (define init (x y) (begin (set fst x) (set snd y) self))
// (define fst () fst)
// (define snd () snd)
// (define fst: (x) (set fst x))
// (define snd: (y) (set snd y))
// )";
//
// 				// From Kamin page 288.
// 				const string dictionaryClass = @"
// (class Dictionary KeyedCollection ()
// (table currentKey)
// (define init ()
// 	(begin (set table (mkList)) self))
// (define currentKey () currentKey)
// (define first ()
// 	(if (isEmpty table) nil
// 		(begin
// 			(set tempassoc (first table))
// 			(set currentKey (fst tempassoc))
// 			(snd tempassoc))))
// (define next ()
// 	(begin
// 		(set tempassoc (next table))
// 		(if (isNil tempassoc) nil
// 			(begin
// 				(set currentKey (fst tempassoc))
// 				(snd tempassoc)))))
// (define at:put: (key value)
// 	(begin
// 		(set tempassoc (associationAt: self key))
// 		(if (isNil tempassoc)
// 			(add: table (mkAssociation key value))
// 			(snd: tempassoc value))))
// (define associationAt: (key)
// 	(begin
// 		(set temptable table)
// 		(set found false)
// 		(while (not (or (isEmpty temptable) found))
// 			(if (= (fst (car temptable)) key)
// 				(set found true)
// 				(set temptable (cdr temptable))))
// 		(if found (car temptable) nil)))
// )";
//
// 				// From Kamin page 291.  (Not used by the Financial History example.)
// #if DEAD_CODE
// 				const string arrayClass = @"
// (class Array SequenceableCollection ()
// (elements lobound hibound currentKey)
// (define init (lo size)
// 	(begin
// 		(set lobound lo)
// 		(set hibound (- (+ lo size) 1))
// 		(set elements (new List))
// 		(zerolist elements size)
// 		self))
// (define size () (+1 (- hibound lobound)))
// (define firstKey () lobound)
// (define lastKey () hibound)
// (define currentKey () currentKey)
// (define first ()
// 	(begin
// 		(set currentKey lobound)
// 		(first elements)))
// (define next ()
// 	(if (= currentKey hibound) nil
// 		(begin
// 			(set currentKey (+1 currentKey))
// 			(next elements))))
// (define at:put: (n value)
// 	(if (or (< n lobound) (> n hibound)) nil ; Slightly modified condition
// 		(at:put: elements (+1 (- n lobound)) value)))
// )";
// #else
// 				// Re-implement the Array class using the built-in "array" value type.
// 				const string arrayClass = @"
// (class Array SequenceableCollection ()
// (elements lobound hibound currentKey)
// (define init (lo size)
// 	(begin
// 		(set lobound lo)
// 		(set hibound (- (+ lo size) 1))
// 		(set elements (newarray size))
// 		; (zerolist elements size)
// 		self))
// (define size () (arraylength elements))
// (define firstKey () lobound)
// (define lastKey () hibound)
// (define currentKey () currentKey)
// (define first ()
// 	(begin
// 		(set currentKey lobound)
// 		(at: self lobound)))
// (define next ()
// 	(if (= currentKey hibound) nil
// 		(begin
// 			(set currentKey (+1 currentKey))
// 			(at: self currentKey))))
// (define at:put: (n value)
// 	(if (or (< n lobound) (> n hibound)) nil ; Slightly modified condition
// 		(arrayset elements (+1 (- n lobound)) value)))
// (define at: (index)
// 	(if (or (< index lobound) (> index hibound)) nil ; Slightly modified condition
// 		(arrayget elements (+1 (- index lobound))))) ; ThAW 2014/02/01 : Override the 'at:' that is in SequenceableCollection.
// )";
// #endif
//
// 				// ThAW 2014/02/03 : There are some similarities between Stack and Queue.
// 				const string stackClass = @"
// (class Stack List () ()
// (define init () (init super))
// (define newEmptyCollection () (init (new Stack)))
// (define peek () car)
// (define push: (item) (add: self item))
// (define pop ()
// 	(if (isEmpty self) nil
// 		(let ((result car))
// 			(begin
// 				(removeFirst self)
// 				result))))
// )";
// 				// This Queue class is somewhat similar to the one on page 309.
// 				const string queueClass = @"
// (class Queue List () ()
// (define init () (init super))
// (define newEmptyCollection () (init (new Queue)))
// (define peek () car)
// (define enqueue: (item) ; Add the item to the end of the queue.
// 	(if (isEmpty self)
// 		(add: self item)
// 		(enqueue: cdr item)))
// (define dequeue ()
// 	(if (isEmpty self) nil
// 		(let ((result car))
// 			(begin
// 				(removeFirst self)
// 				result))))
// )";
// 				// See page 310.
// 				const string priorityQueueClass = @"
// (class PriorityQueue List () ()
// (define init () (init super))
// (define newEmptyCollection () (init (new PriorityQueue)))
// (define peek () car)
// (define enqueue: (pair) ; Insert the item at the appropriate place in the queue.
// 	(cond
// 		((isEmpty self) (add: self pair))
// 		; ThAW 2014/02/07 : I replaced these two lines...
// 		;((< (fst pair) (fst car)) (add: self pair))
// 		;(true (enqueue: cdr pair))))
// 		; ... with these two lines:
// 		((< (fst car) (fst pair)) (enqueue: cdr pair))
// 		(true (add: self pair))))
// (define dequeue ()
// 	(if (isEmpty self) nil
// 		(let ((result car))
// 			(begin
// 				(removeFirst self)
// 				result))))
// )";
//
// 				Evaluate(collectionClass);
// 				Evaluate(keyedCollectionClass);
// 				Evaluate(sequenceableCollectionClass);
// 				Evaluate(listClass);
// 				Evaluate("(define mkList () (init (new List)))");
// 				Evaluate(setClass);
// 				Evaluate("(define mkSet () (init (new Set)))");
// 				Evaluate(associationClass);
// 				Evaluate("(define mkAssociation (a b) (init (new Association) a b))");
// 				Evaluate(dictionaryClass);
// 				Evaluate("(define mkDictionary () (init (new Dictionary)))");
// 				Evaluate(arrayClass);
// 				Evaluate("(define mkArray (l s) (init (new Array) l s))");
// 				Evaluate(stackClass);
// 				Evaluate("(define mkStack () (init (new Stack)))");
// 				Evaluate(queueClass);
// 				Evaluate("(define mkQueue () (init (new Queue)))");
// 				Evaluate(priorityQueueClass);
// 				Evaluate("(define mkPriorityQueue () (init (new PriorityQueue)))");
// 				break;
//
// 			default:
// 				throw new Exception(string.Format("LoadPreset() : Unknown preset name '{0}'.", presetName));
// 		}
//
// 		LoadedPresets.Add(presetNameToLower);
// 		return string.Format("The preset '{0}' has been successfully loaded.", presetName);
// 	}

// 	public ISmalltalkValue FalseValue
// 	{
// 		get
// 		{
//
// 			if (FalseVal == null)
// 			{
// 				throw new Exception("FalseValue was used before being defined");
// 			}
//
// 			return FalseVal;
// 		}
// 	}
//
// 	public ISmalltalkValue TrueValue
// 	{
// 		get
// 		{
//
// 			if (TrueVal == null)
// 			{
// 				throw new Exception("TrueValue was used before being defined");
// 			}
//
// 			return TrueVal;
// 		}
// 	}
//
// 	public bool ValueIsFalse(ISmalltalkValue value)
// 	{
// 		//return value.Owner.Equals(ClassDict["FalseValue"]);
// 		return value.Owner.ClassName == FalseValueClassName;
// 	}
//
// 	public bool ValueIsInteger(ISmalltalkValue value)
// 	{
// 		return value is SmalltalkIntegerValue;
// 	}
//
// 	public int ValueAsInteger(ISmalltalkValue value)
// 	{
// #if DEAD_CODE
// 		if (!ValueIsInteger(value))
// 		{
// 			throw new ArgumentException("ValueAsInteger() : value is not an integer");
// 		}
//
// 		var prim = (SmalltalkIntegerValue)value;
//
// 		return prim.Value;
// #else
// 		var valueAsNumber = value as ISmalltalkNumber;
//
// 		if (valueAsNumber == null)
// 		{
// 			throw new ArgumentException("ValueAsInteger() : The value is not an ISmalltalkNumber.");
// 		}
//
// 		return valueAsNumber.ToInteger();
// #endif
// 	}
//
// 	public ISmalltalkValue IntegerAsValue(int value)
// 	{
// 		return new SmalltalkIntegerValue(value);
// 	}
//
// 	public bool SetScoping(bool dynamicScoping)
// 	{
// 		return false;
// 	}
//
// 	public bool SetDebug(bool debug)
// 	{
// 		return false;
// 	}
// }

import { IParser, ITokenizer } from 'thaw-interpreter-types';

// import { IGlobalInfoOps } from '../../../common/domain-object-model/iglobal-info-ops';

import { ArgumentException } from '../../../common/exceptions/argument-exception';

import {
	ISmalltalkClass,
	ISmalltalkEnvironmentFrame,
	ISmalltalkExpression,
	ISmalltalkFunctionDefinition,
	ISmalltalkGlobalInfo,
	ISmalltalkUserValue,
	ISmalltalkValue
} from './interfaces/iexpression';

import { falseVar, nilClass, nilVar, objectClass, trueVar } from './bootstrap';

import { SmalltalkEnvironmentFrame } from './environment-frame';

import { SmalltalkIntegerValue } from './integer';

import { falseValue, nilInstance, objectInstance, trueValue } from './object-instance';

// import { SmalltalkVariable } from './variable';

export class SmalltalkGlobalInfo implements /* IGlobalInfoOps, */ ISmalltalkGlobalInfo {
	// private readonly zeroValueForAccessor = new SmalltalkIntegerValue(0);
	// private readonly falseValueForAccessor = new SmalltalkIntegerValue(0);
	// private readonly falseValueForAccessor = falseValue;
	// private readonly trueValueForAccessor = new SmalltalkIntegerValue(1);
	// private readonly trueValueForAccessor = trueValue;
	public readonly globalEnvironment = new SmalltalkEnvironmentFrame();
	public readonly functionDefinitions = new Map<string, ISmalltalkFunctionDefinition>();
	public readonly classDict = new Map<string, ISmalltalkClass>();
	public readonly objectInstance: ISmalltalkUserValue; // Passed to Evaluate() by the interpreter; see Kamin pages 297-298.

	constructor() {
		// } = {} // 	tokenizer?: ITokenizer; // 	parser?: IParser; // options: {
		// These are temporary values for FalseVal and TrueVal; hopefully they are not used.
		//FalseVal = ZeroValue;
		//TrueVal = new SmalltalkIntegerValue(1);

		// if (typeof options.parser !== 'undefined' && typeof options.tokenizer !== 'undefined') {
		// 	// new SmalltalkFunctionDefinition('isNil', [], new SmalltalkVariable('false'));
		// 	objectClass.addFunction(
		// 		options.tokenizer,
		// 		options.parser,
		// 		`(define isNil () ${falseVariableName})`
		// 	);
		//
		// 	// new SmalltalkFunctionDefinition('notNil', [], new SmalltalkVariable('true'));
		// 	objectClass.addFunction(
		// 		options.tokenizer,
		// 		options.parser,
		// 		`(define notNil () ${trueVariableName})`
		// 	);
		// }

		this.classDict.set(nilClass.className, nilClass);
		this.classDict.set(objectClass.className, objectClass);

		this.objectInstance = objectInstance;

		this.globalEnvironment.add(nilVar, nilInstance);
		this.globalEnvironment.add(falseVar, falseValue);
		this.globalEnvironment.add(trueVar, trueValue);
	}

	// public get zeroValue(): ISmalltalkValue {
	// 	return this.zeroValueForAccessor;
	// }

	public get falseValue(): ISmalltalkValue {
		// return this.falseValueForAccessor;

		return falseValue;
	}

	public get trueValue(): ISmalltalkValue {
		// return this.trueValueForAccessor;

		return trueValue;
	}

	public valueIsFalse(value: ISmalltalkValue): boolean {
		return value.toInteger() === 0;
	}

	public valueIsInteger(value: ISmalltalkValue): boolean {
		return value.isInteger;
	}

	public valueAsInteger(value: ISmalltalkValue): number {
		const valueAsNumber = value.toInteger();

		if (valueAsNumber === undefined) {
			throw new ArgumentException(
				'valueAsInteger() : The value is not an IntegerLiteral.',
				'valueAsNumber'
			);
		}

		return valueAsNumber;
	}

	public integerAsValue(value: number): ISmalltalkValue {
		return new SmalltalkIntegerValue(value);
	}

	public loadPresets(tokenizer: ITokenizer, parser: IParser): void {
		// expr.Evaluate(null, ObjectInstance, null, this); ->

		// const f = (str: string) => (parser.parse(tokenizer.tokenize(str)) as ISmalltalkExpression).evaluate(this.globalEnvironment, this.objectInstance, undefined, this);

		const f = (str: string) =>
			(parser.parse(tokenizer.tokenize(str)) as ISmalltalkExpression).evaluate(
				undefined,
				this.objectInstance,
				undefined,
				this
			);

		f('(define +1 (x) (+ x 1))');
	}

	// More code to put into loadPresets() :

	// GlobalEnvironment.Add(new SmalltalkVariable("e" /*, 0, 0 */), new SmalltalkFloatValue(Math.E));
	// GlobalEnvironment.Add(new SmalltalkVariable("pi" /*, 0, 0 */), new SmalltalkFloatValue(Math.PI));

	// 		Evaluate(string.Format(@"
	// (class {0} Object ()
	// (stringValue) ; stringValue is used as the value of the object of this class when it is converted to a string.
	// (define init () (begin (set stringValue '{1}') self))
	// (define if (trueBlock falseBlock) falseBlock)
	// (define and (x) {2})
	// (define or (x) x)
	// (define xor (x) x)
	// (define not () {3})
	// )", FalseValueClassName, FalseValueAsString, FalseVariableName, TrueVariableName));
	// 		Evaluate(string.Format(@"
	// (class {0} Object ()
	// (stringValue) ; stringValue is used as the value of the object of this class when it is converted to a string.
	// (define init () (begin (set stringValue '{1}') self))
	// (define if (trueBlock falseBlock) trueBlock)
	// (define and (x) x)
	// (define or (x) {2})
	// (define xor (x) (not x))
	// (define not () {3})
	// )", TrueValueClassName, TrueValueAsString, TrueVariableName, FalseVariableName));
	// 		Evaluate(string.Format("(set {0} (init (new {1})))", FalseVariableName, FalseValueClassName));
	// 		Evaluate(string.Format("(set {0} (init (new {1})))", TrueVariableName, TrueValueClassName));
	// 		FalseVal = GlobalEnvironment.Dict[new SmalltalkVariable(FalseVariableName)];
	// 		TrueVal = GlobalEnvironment.Dict[new SmalltalkVariable(TrueVariableName)];
	//
	// 		Evaluate(string.Format(@"
	// (class UndefinedObject Object ()
	// (stringValue) ; stringValue (#nil) is used as the value of the object of this class when it is converted to a string.
	// (define init () (begin (set stringValue '{0}') self))
	// (define isNil () {1})
	// (define notNil () {2})
	// )", NilValueAsString, TrueVariableName, FalseVariableName));
	// 		Evaluate("(set nil (init (new UndefinedObject)))");

	// Evaluate("(define > (x y) (< y x))");
	//Evaluate("(define and (x y) (if x y x))");
	//Evaluate("(define or (x y) (if x x y))");
	//Evaluate(string.Format("(define not (x) (if x {0} {1}))", FalseVariableName, TrueVariableName));
	// Evaluate("(define <> (x y) (not (= x y)))");
	// Evaluate("(define <= (x y) (not (> x y)))");
	// Evaluate("(define >= (x y) (not (< x y)))");
	// Evaluate("(define mod (m n) (- m (* n (/ m n))))");
	// Evaluate("(define gcd (m n) (if (= n 0) m (gcd n (mod m n))))");
	// Evaluate("(define abs (n) (if (< n 0) (- 0 n) n))");

	public evaluate(
		expr: ISmalltalkExpression,
		options: {
			localEnvironment?: ISmalltalkEnvironmentFrame;
			c?: ISmalltalkClass;
		} = {}
	): ISmalltalkValue {
		return expr.evaluate(
			options.localEnvironment, // undefined, // Or this.globalEnvironment ?
			this.objectInstance,
			options.c, // undefined,
			this
		);
	}
}
