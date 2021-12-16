// tom-weatherhead/thaw-grammar/src/languages/prolog/domain-object-model/prolog-global-info.ts

import { createSet, IImmutableSet } from 'thaw-common-utilities.ts';

import { IParser, ITokenizer } from 'thaw-interpreter-types';

import { GlobalInfoBase } from '../../../common/domain-object-model/global-info-base';

import { CutBacktrackException } from './cut-backtrack-exception';
import { CutDetector } from './cut-detector';
import { PrologClause } from './prolog-clause';
import { PrologFunctorExpression } from './prolog-functor-expression';
import { PrologGoal } from './prolog-goal';
import { PrologIntegerLiteral } from './prolog-integer-literal';
import { PrologModule } from './prolog-module';
import { createSubstitution } from './prolog-substitution';
import { createVariable } from './prolog-variable';
import { StringIntKey } from './string-int-key';
import { createGoalFromFunctorExpression, findBindingVariablesInSubstitution } from '../utilities';

import { IPrologExpression } from './interfaces/iprolog-expression';
import { ISubstitution } from './interfaces/isubstitution';
import { IVariable, isIVariable } from './interfaces/ivariable';

enum SolutionCollectionMode {
	None,
	FindAll,
	BagOfOrSetOf
}

export class PrologGlobalInfo extends GlobalInfoBase<IPrologExpression> /* implements IGlobalInfoOps, IParser */ {
	public static readonly ClauseAdded = 'Clause added.';
	public static readonly ClauseAlreadyExists =
		'An identical clause is already in the clause list.';
	public static readonly IsomorphicClauseAlreadyExists =
		'An isomorphic clause is already in the clause list.';
	public static readonly IsomorphicOrMoreGeneralClauseAlreadyExists =
		'An isomorphic or more general clause is already in the clause list.';
	public static readonly OperatorAdded = 'Operator added.';
	public static readonly InvalidCommand = 'Invalid command.';
	public static readonly Satisfied = 'Satisfied';
	public static readonly NotSatisfied = 'Not satisfied';
	// //public readonly List<PrologClause> ClauseList = new List<PrologClause>();
	// public readonly gs: LanguageSelector;
	// public readonly tokenizer: ITokenizer;
	// public readonly IParser parser;
	private variableRenameNum = 0;
	private allMode = false; // Determines how many solutions we will search for.  false means "first" mode; true means "all" mode.
	// private readonly StringBuilder sbOutput = new StringBuilder();
	// private readonly Random random = new Random();
	// private readonly HashSet<string> LoadedPresets = new HashSet<string>();
	private solutionCollectionMode = SolutionCollectionMode.None;
	private numSolutionsFound = 0;
	// private IPrologExpression findAll_Expression = null;
	// private List<IPrologExpression> findAll_ResultList = null;
	// private List<PrologVariable> caretListVariables = null;
	// private Dictionary<ExpressionListAsKey, List<IPrologExpression>> dictSolutions = null;
	// public string PathToDefaultDirectory = null;
	// private readonly Dictionary<StringIntKey, Func<PrologGoal, PrologSubstitution>> dictBuiltInPredicates
	//         = new Dictionary<StringIntKey, Func<PrologGoal, PrologSubstitution>>();
	// 	// #if SUPPORT_USER_DEFINED_OPERATORS
	// private readonly List<PrologOperator> Operators = new List<PrologOperator>();
	// 	// #endif
	// public IInterpreterFileLoader FileLoader = null;
	private readonly DefaultModule = new PrologModule();
	// private readonly dictModules = new Map<string, PrologModule>(); // The keys are file paths.
	private guidNumber = 0;

	// constructor(gs: LanguageSelector, t: ITokenizer) {
	// constructor() {
	// 	super();
	//
	// 	// this.gs = gs;
	// 	// this.tokenizer = t;
	// }

	constructor(
		options: {
			parser?: IParser;
			tokenizer?: ITokenizer;
		} = {}
	) {
		super(options);
	}

	//     public PrologGlobalInfo(LanguageSelector gs, ITokenizer t, IParser p)
	//     {
	//         this.gs = gs;
	//         tokenizer = t;
	//         parser = p;

	//         if (gs == LanguageSelector.Prolog)
	//         {
	//             dictBuiltInPredicates[new StringIntKey("plus", 3)] = KaminPlus3;
	//             dictBuiltInPredicates[new StringIntKey("minus", 3)] = KaminMinus3;
	//             dictBuiltInPredicates[new StringIntKey("less", 2)] = LessThan2;
	//             dictBuiltInPredicates[new StringIntKey("not-equal", 2)] = NotEquals2;
	//         }

	//         dictBuiltInPredicates[new StringIntKey("is", 2)] = Is2;
	//         dictBuiltInPredicates[new StringIntKey("<", 2)] = LessThan2;
	//         dictBuiltInPredicates[new StringIntKey(">", 2)] = GreaterThan2;
	//         dictBuiltInPredicates[new StringIntKey("=<", 2)] = EqualOrLessThan2;
	//         dictBuiltInPredicates[new StringIntKey(">=", 2)] = GreaterThanOrEqual2;
	//         dictBuiltInPredicates[new StringIntKey("=:=", 2)] = ArithmeticEqual2;
	//         dictBuiltInPredicates[new StringIntKey(@"=\=", 2)] = ArithmeticNotEqual2;
	//         dictBuiltInPredicates[new StringIntKey("=", 2)] = Unifiable2;
	//         dictBuiltInPredicates[new StringIntKey(@"\=", 2)] = NotUnifiable2;
	//         dictBuiltInPredicates[new StringIntKey("==", 2)] = Equals2;
	//         dictBuiltInPredicates[new StringIntKey(@"\==", 2)] = NotEquals2;
	//         dictBuiltInPredicates[new StringIntKey("open", 3)] = Open3;
	//         dictBuiltInPredicates[new StringIntKey("close", 1)] = Close1;
	//         dictBuiltInPredicates[new StringIntKey("at_end_of_stream", 1)] = AtEndOfStream1;
	//         dictBuiltInPredicates[new StringIntKey("read", 2)] = Read2;
	//         dictBuiltInPredicates[new StringIntKey("get_code", 2)] = GetCode2;
	//         dictBuiltInPredicates[new StringIntKey("write", 1)] = Write1; // See http://www.csupomona.edu/~jrfisher/www/prolog_tutorial/2_3.html
	//         dictBuiltInPredicates[new StringIntKey("write", 2)] = Write2;
	//         dictBuiltInPredicates[new StringIntKey("tab", 1)] = Tab1;
	//         dictBuiltInPredicates[new StringIntKey("tab", 2)] = Tab2;
	//         dictBuiltInPredicates[new StringIntKey("nl", 0)] = NL0;
	//         dictBuiltInPredicates[new StringIntKey("nl", 1)] = NL1;
	//         dictBuiltInPredicates[new StringIntKey("atom", 1)] = Atom1;
	//         dictBuiltInPredicates[new StringIntKey("integer", 1)] = Integer1;
	//         dictBuiltInPredicates[new StringIntKey("float", 1)] = Float1;
	//         dictBuiltInPredicates[new StringIntKey("number", 1)] = Number1;
	//         dictBuiltInPredicates[new StringIntKey("atomic", 1)] = Atomic1;
	//         // var: See usage in http://www.csupomona.edu/~jrfisher/www/prolog_tutorial/5_3.html
	//         // See also http://www.csupomona.edu/~jrfisher/www/prolog_tutorial/4.html#4.8
	//         dictBuiltInPredicates[new StringIntKey("var", 1)] = Var1;
	//         dictBuiltInPredicates[new StringIntKey("nonvar", 1)] = NonVar1;
	//         dictBuiltInPredicates[new StringIntKey("ground", 1)] = Ground1; // See http://www.csupomona.edu/~jrfisher/www/prolog_tutorial/4.html#4.8
	//         dictBuiltInPredicates[new StringIntKey("random", 2)] = Random2;
	//         dictBuiltInPredicates[new StringIntKey("fail", 0)] = Fail0;
	//         // listing/0 and listing/1: See http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlse4
	//         dictBuiltInPredicates[new StringIntKey("listing", 0)] = Listing0;
	//         dictBuiltInPredicates[new StringIntKey("listing", 1)] = Listing1;
	//         dictBuiltInPredicates[new StringIntKey("arg", 3)] = Arg3; // See http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlse39
	//         dictBuiltInPredicates[new StringIntKey("=..", 2)] = Univ2; // See http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlse39
	//         dictBuiltInPredicates[new StringIntKey("atom_codes", 2)] = AtomicCodes;     // See http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlse39
	//         dictBuiltInPredicates[new StringIntKey("number_codes", 2)] = AtomicCodes;   // See http://www.complang.tuwien.ac.at/SWI-Prolog/Manual/manipatom.html
	//         dictBuiltInPredicates[new StringIntKey("name", 2)] = AtomicCodes;
	//         dictBuiltInPredicates[new StringIntKey("atom_chars", 2)] = AtomicChars;
	//         dictBuiltInPredicates[new StringIntKey("number_chars", 2)] = AtomicChars;   // See http://www.complang.tuwien.ac.at/SWI-Prolog/Manual/manipatom.html
	//         dictBuiltInPredicates[new StringIntKey("char_code", 2)] = CharCode;         // See http://www.complang.tuwien.ac.at/SWI-Prolog/Manual/manipatom.html
	//         dictBuiltInPredicates[new StringIntKey("atom_number", 2)] = AtomNumber;     // See http://www.complang.tuwien.ac.at/SWI-Prolog/Manual/manipatom.html
	//         dictBuiltInPredicates[new StringIntKey("atom_length", 2)] = AtomLength;     // See http://www.complang.tuwien.ac.at/SWI-Prolog/Manual/manipatom.html
	//         //dictBuiltInPredicates[new StringIntKey("findall", 3)] = FindAll3;           // See http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlse49
	//         dictBuiltInPredicates[new StringIntKey("assert", 1)] = AssertA1;
	//         dictBuiltInPredicates[new StringIntKey("asserta", 1)] = AssertA1;
	//         dictBuiltInPredicates[new StringIntKey("assertz", 1)] = AssertZ1;

	// 		// #if SUPPORT_USER_DEFINED_OPERATORS
	//         CreateBuiltInOperators();
	// 		// #endif
	//     }

	public override toString(): string {
		return 'PrologGlobalInfo.toString()';
	}

	public get falseValue(): IPrologExpression {
		return new PrologIntegerLiteral(0);
	}

	public get trueValue(): IPrologExpression {
		return new PrologIntegerLiteral(1);
	}

	public valueIsFalse(value: IPrologExpression): boolean {
		// return value === this.falseValue;

		return this.valueIsInteger(value) && this.valueAsInteger(value) === 0;

		// Or: return this.valueIsInteger(value) && this.valueAsInteger(value) === this.valueAsInteger(this.falseValue);
	}

	public valueIsInteger(value: IPrologExpression): boolean {
		const pn = value.EvaluateToNumber();

		if (typeof pn === 'undefined') {
			return false;
		}

		const n = pn.ToDouble();

		return !Number.isNaN(n) && Math.floor(n) === n;
	}

	public valueAsInteger(value: IPrologExpression): number {
		// Should we return Number.NaN if value is not a (safe) integer?
		const pn = value.EvaluateToNumber();

		if (typeof pn === 'undefined') {
			throw new Error('PrologGlobalInfo.valueAsInteger() error 1');
		}

		const n = pn.ToDouble();

		if (Number.isNaN(n) || Math.floor(n) !== n) {
			throw new Error('PrologGlobalInfo.valueAsInteger() error 2');
		}

		return n;
	}

	public integerAsValue(value: number): IPrologExpression {
		return new PrologIntegerLiteral(value);
	}

	// 	// #if SUPPORT_USER_DEFINED_OPERATORS
	//     private void CreateBuiltInOperators()
	//     {
	//         // See http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlse40
	//         /*
	//         :-  op(  1200,  xfx,  [  :-,  -->  ]).
	//         :-  op(  1200,    fx,  [  :-,  ?-  ]).
	//         :-  op(  1100,  xfy,  [  ;  ]).
	//         :-  op(  1000,  xfy,  [  ’,’  ]).
	//         :-  op(    700,  xfx,  [  =,  is,  =..,  ==,  \==,  =:=,  =\=,  <,  >,  =<,  >=  ]).
	//         :-  op(    500,  yfx,  [  +,  -]).
	//         :-  op(    500,    fx,  [  +,  -  ]).
	//         :-  op(    300,  xfx,  [  mod  ]).
	//         :-  op(    200,  xfy,  [  ^  ]).
	//          */
	//         // See also http://www.swi-prolog.org/pldoc/man?section=operators
	//         Operators.Add(new PrologOperator(1200, OperatorType.xfx, ":-"));
	//         Operators.Add(new PrologOperator(1200, OperatorType.xfx, "-->"));
	//         Operators.Add(new PrologOperator(1200, OperatorType.fx, ":-"));
	//         Operators.Add(new PrologOperator(1200, OperatorType.fx, "?-"));
	//         Operators.Add(new PrologOperator(1100, OperatorType.xfy, ";"));
	//         Operators.Add(new PrologOperator(1100, OperatorType.xfy, "|"));
	//         Operators.Add(new PrologOperator(1050, OperatorType.xfy, "->"));
	//         Operators.Add(new PrologOperator(1000, OperatorType.xfy, ","));
	//         Operators.Add(new PrologOperator(900, OperatorType.fy, @"\+"));
	//         Operators.Add(new PrologOperator(700, OperatorType.xfx, "="));
	//         Operators.Add(new PrologOperator(700, OperatorType.xfx, "is"));
	//         Operators.Add(new PrologOperator(700, OperatorType.xfx, "=.."));
	//         Operators.Add(new PrologOperator(700, OperatorType.xfx, "=="));
	//         Operators.Add(new PrologOperator(700, OperatorType.xfx, @"\=="));
	//         Operators.Add(new PrologOperator(700, OperatorType.xfx, "=:="));
	//         Operators.Add(new PrologOperator(700, OperatorType.xfx, @"=\="));
	//         Operators.Add(new PrologOperator(700, OperatorType.xfx, "<"));
	//         Operators.Add(new PrologOperator(700, OperatorType.xfx, ">"));
	//         Operators.Add(new PrologOperator(700, OperatorType.xfx, "=<"));
	//         Operators.Add(new PrologOperator(700, OperatorType.xfx, ">="));
	//         Operators.Add(new PrologOperator(600, OperatorType.xfy, ":"));
	//         Operators.Add(new PrologOperator(500, OperatorType.yfx, "+"));
	//         Operators.Add(new PrologOperator(500, OperatorType.yfx, "-"));
	//         //Operators.Add(new PrologOperator(500, OperatorType.fx, "+"));
	//         //Operators.Add(new PrologOperator(500, OperatorType.fx, "-"));
	//         //Operators.Add(new PrologOperator(500, OperatorType.fx, "?")); // Do we use this?
	//         Operators.Add(new PrologOperator(400, OperatorType.yfx, "*"));
	//         Operators.Add(new PrologOperator(400, OperatorType.yfx, "/"));
	//         Operators.Add(new PrologOperator(400, OperatorType.yfx, "mod"));    // According to SWI-Prolog.
	//         //Operators.Add(new PrologOperator(300, OperatorType.xfx, "mod"));
	//         Operators.Add(new PrologOperator(200, OperatorType.xfy, "^"));
	//         Operators.Add(new PrologOperator(200, OperatorType.fy, "+"));       // According to SWI-Prolog.
	//         Operators.Add(new PrologOperator(200, OperatorType.fy, "-"));       // According to SWI-Prolog.
	//         //Operators.Add(new PrologOperator(0, OperatorType.fx, "("));
	//     }
	// 	// #endif

	public Clear(): void {
		// ClauseList.Clear();
		this.variableRenameNum = 0;
		this.FindFirstSolution();
		// LoadedPresets.Clear();
		// // #if SUPPORT_USER_DEFINED_OPERATORS
		// Operators.Clear();
		// CreateBuiltInOperators();
		// // #endif
		this.DefaultModule.clear();
		// dictModules.Clear();
	}

	public FindFirstSolution(): void {
		this.allMode = false;
	}

	public FindAllSolutions(): void {
		this.allMode = true;
	}

	// public LoadPreset(presetName: string): string {
	// 	// if (LoadedPresets.Contains(presetName)) {
	// 	if (presetName in this.LoadedPresets) {
	// 		// return string.Format("The preset '{0}' has already been loaded.", presetName);
	// 		return `The preset '${presetName}' has already been loaded.`;
	// 	}

	// 	switch (presetName) {
	// 		// case "<=":

	// 		// 	if (gs == LanguageSelector.Prolog)
	// 		// 	{
	// 		// 	ProcessInputString("(infer (<= X X))");
	// 		// 	ProcessInputString("(infer (<= X Y) from (less X Y))");
	// 		// 	}
	// 		// 	else
	// 		// 	{
	// 		// 	ProcessInputString("'<='(X, Y) :- X =< Y.");
	// 		// 	}

	// 		// 	break;

	// 		// case "addtoend":

	// 		// 	if (gs == LanguageSelector.Prolog)
	// 		// 	{
	// 		// 	// (addtoend L X M) means that M is the list obtained by adding X to the end of L.
	// 		// 	ProcessInputString("(infer (addtoend nil X (cons X nil)))");
	// 		// 	ProcessInputString("(infer (addtoend (cons Y L) X (cons Y M)) from (addtoend L X M))");
	// 		// 	}
	// 		// 	else
	// 		// 	{
	// 		// 	ProcessInputString("addtoend([], X, [X]).");
	// 		// 	ProcessInputString("addtoend([Y | L], X, [Y | M]) :- addtoend(L, X, M).");
	// 		// 	}

	// 		// 	break;

	// 		case 'append':
	// 			//case "append2": // The preset name "append2" is deprecated.

	// 			// if (gs == LanguageSelector.Prolog)
	// 			// {
	// 			// // (append L M N) means that N is the list obtained by appending M onto the end of L.
	// 			// ProcessInputString("(infer (append nil L L))");
	// 			// ProcessInputString("(infer (append (cons X L) M (cons X N)) from (append L M N))");
	// 			// }
	// 			// else
	// 			// {
	// 			this.ProcessInputString('append([], L, L).');
	// 			this.ProcessInputString(
	// 				'append([X | Y], L, [X | Z]) :- append(Y, L, Z).'
	// 			);
	// 			// }

	// 			break;

	// 		case 'member':
	// 			//case "member2": // The preset name "member2" is deprecated.

	// 			// if (gs == LanguageSelector.Prolog)
	// 			// {
	// 			// ProcessInputString("(infer (member X (cons X L)))");
	// 			// ProcessInputString("(infer (member X (cons Y M)) from (member X M))");
	// 			// }
	// 			// else
	// 			// {
	// 			this.ProcessInputString('member(X, [X | _]).');
	// 			this.ProcessInputString('member(X, [_ | T]) :- member(X, T).');
	// 			// }

	// 			break;

	// 		case 'permutation':
	// 			this.LoadPreset('append');
	// 			this.ProcessInputString('permutation([], []).');
	// 			this.ProcessInputString(
	// 				'permutation(L, [H | T]) :- append(V, [H | U], L), append(V, U, W), permutation(W, T).'
	// 			);
	// 			break;

	// 		case 'rev': // Reverse a list.
	// 			this.ProcessInputString(
	// 				'accRev([H | T], A, R):-  accRev(T, [H | A], R).'
	// 			);
	// 			this.ProcessInputString('accRev([], A, A).');
	// 			this.ProcessInputString('rev(L, R) :- accRev(L, [], R).');
	// 			break;

	// 		// case "succ":
	// 		// 	ProcessInputString("intToSucc(0, 0).");
	// 		// 	ProcessInputString("intToSucc(N, succ(L)) :- N > 0, M is N - 1, intToSucc(M, L).");
	// 		// 	ProcessInputString("succToInt(0, 0).");
	// 		// 	ProcessInputString("succToInt(succ(L), N) :- succToInt(L, M), N is M + 1.");
	// 		// 	break;

	// 		// case "atom_concat":
	// 		// 	LoadPreset("append");
	// 		// 	// We want to use a cut in one of these clauses to avoid repeated results in the case where A1, A2, and A3 are all atomic.
	// 		// 	// We can use a cut in this clause because it can produce at most one result, even in "all" mode:
	// 		// 	ProcessInputString("atom_concat(A1, A2, A3) :- atomic(A1), atomic(A2), atom_chars(A1, L1), atom_chars(A2, L2), append(L1, L2, L3), atom_chars(A3, L3), !.");
	// 		// 	// We don't use a cut in ths clause because it can produce multiple results in "all" mode:
	// 		// 	ProcessInputString("atom_concat(A1, A2, A3) :- atomic(A3), atom_chars(A3, L3), append(L1, L2, L3), atom_chars(A1, L1), atom_chars(A2, L2).");
	// 		// 	break;

	// 		// case "concat_atom":
	// 		// 	LoadPreset("atom_concat");
	// 		// 	ProcessInputString("concat_atomAcc(Acc, [], Acc).");
	// 		// 	ProcessInputString("concat_atomAcc(Acc, [H | T], Result) :- atom_concat(Acc, H, Acc2), concat_atomAcc(Acc2, T, Result).");
	// 		// 	// We can use a cut in this clause to avoid reporting results using both this clause and the next clause when in "all" mode.
	// 		// 	// This clause is not satisfied when there are variables in the first parameter.
	// 		// 	ProcessInputString("concat_atom([H | T], Result) :- concat_atomAcc(H, T, Result), !.");
	// 		// 	// We don't use a cut in ths clause because it can produce multiple results in "all" mode:
	// 		// 	ProcessInputString("concat_atom([X, Y], Result) :- atom_concat(X, Y, Result).");
	// 		// 	break;

	// 		default:
	// 			throw new Error(
	// 				`LoadPreset() : Unknown preset name '${presetName}'.`
	// 			);
	// 	}

	// 	this.LoadedPresets.push(presetName);

	// 	return `The preset '${presetName}' has been successfully loaded.`;
	// }

	//     public void LoadPresets()
	//     {
	//         //ProcessInputString("");
	//     }

	//     private PrologNameExpression<PrologFunctor> CreateAtom(PrologFunctor f)
	//     {
	//         return new PrologNameExpression<PrologFunctor>(gs, f);
	//     }

	//     private PrologNameExpression<PrologFunctor> CreateAtom(string name)
	//     {

	//         if (string.IsNullOrEmpty(name))
	//         {
	//             throw new Exception("CreateAtom() : The atom name is null or empty.");
	//         }

	//         return CreateAtom(new PrologFunctor(name));
	//     }

	public GetNextUniqueVariable(): IVariable {
		++this.variableRenameNum;

		return createVariable(`Var${this.variableRenameNum}`);
	}

	private GetVariablesFromGoalList(goalListParam: PrologGoal[]): IImmutableSet<IVariable> {
		const result = createSet<IVariable>();

		// console.log('goalListParam is', typeof goalListParam, goalListParam);

		for (const goal of goalListParam) {
			result.unionInPlace(goal.FindBindingVariables());
		}

		return result;
	}

	public GetListOfBindingVariablesFromGoalList(goalListParam: PrologGoal[]): IVariable[] {
		let result: IVariable[] = [];

		for (const goal of goalListParam) {
			result = result.concat(goal.GetListOfBindingVariables());
		}

		return result;
	}

	//     private PrologSubstitution ApplyAssert1(PrologGoal goal, bool asserta)
	//     {
	//         var clause = CreateClause_General(goal.ExpressionList[0]);

	//         if (clause == null)
	//         {
	//             return null;
	//         }

	//         if (asserta)
	//         {
	//             DefaultModule.ClauseList.Insert(0, clause);
	//         }
	//         else
	//         {
	//             DefaultModule.ClauseList.Add(clause);
	//         }

	//         return new PrologSubstitution();
	//     }

	//     private PrologSubstitution AssertA1(PrologGoal goal)
	//     {
	//         return ApplyAssert1(goal, true);
	//     }

	//     private PrologSubstitution AssertZ1(PrologGoal goal)
	//     {
	//         return ApplyAssert1(goal, false);
	//     }

	//     // Question: Should retract be able to remove multiple clauses when retracting a clause that uses non-binding variables?
	//     // See the definition of "undo" at the bottom of http://www.csupomona.edu/~jrfisher/www/prolog_tutorial/2_17.html
	//     // Answer: No, we provide retractall (further below) for removing multiple clauses without binding any variables.

	//     private PrologSubstitution Retract1(PrologGoal goal, PrologSubstitution oldSubstitution, HashSet<PrologVariable> parentVariablesToAvoid)
	//     {
	//         var clause = CreateClause_General(goal.ExpressionList[0]);

	//         if (clause == null)
	//         {
	//             return null;
	//         }

	//         var variablesToAvoid = clause.FindBindingVariables();

	//         variablesToAvoid.unionInPlace(parentVariablesToAvoid);
	//         variablesToAvoid.unionInPlace(oldSubstitution.FindBindingVariables());

	//         for (var clauseNum = 0; clauseNum < DefaultModule.ClauseList.Count; ++clauseNum)
	//         {
	//             var newClause = DefaultModule.ClauseList[clauseNum].RenameVariables(variablesToAvoid, this);
	//             var unifier = newClause.Unify(clause);

	//             if (unifier != null)
	//             {
	//                 DefaultModule.ClauseList.RemoveAt(clauseNum);

	//                 return oldSubstitution.Compose(unifier);
	//             }
	//         }

	//         // No clause was retracted.
	//         // TODO: Should we really return null if nothing was retracted, or should we say that "retract" successfully did nothing?
	//         return null;
	//     }

	//     private PrologSubstitution RetractAll1(PrologGoal goal, PrologSubstitution oldSubstitution, HashSet<PrologVariable> parentVariablesToAvoid)
	//     {
	//         var clause = CreateClause_General(goal.ExpressionList[0]);

	//         if (clause == null)
	//         {
	//             return null;
	//         }

	//         var variablesToAvoid = clause.FindBindingVariables();

	//         variablesToAvoid.unionInPlace(parentVariablesToAvoid);
	//         variablesToAvoid.unionInPlace(oldSubstitution.FindBindingVariables());

	//         var clausesToRemove = DefaultModule.ClauseList
	//             .Where(cl => clause.RenameVariables(variablesToAvoid, this).Unify(cl) != null)
	//             .ToList();

	//         if (clausesToRemove.Count == 0)
	//         {
	//             // No clause was retracted.
	//             // TODO: Should we really return null if nothing was retracted, or should we say that "retractall" successfully did nothing?
	//             return null;
	//         }

	//         for each (var cl in clausesToRemove)
	//         {
	//             DefaultModule.ClauseList.Remove(cl);
	//         }

	//         return new PrologSubstitution();
	//     }

	//     // This function is used by Kamin's Prolog only.

	//     private PrologSubstitution KaminApplyBuiltInArithmeticOperator(
	//         PrologGoal goal,
	//         Func<int, int, int> arithmeticOperator)
	//     {
	//         var intlit1 = goal.ExpressionList[0] as PrologIntegerLiteral;
	//         var intlit2 = goal.ExpressionList[1] as PrologIntegerLiteral;

	//         if (intlit1 == null || intlit2 == null)
	//         {
	//             return null;
	//         }

	//         var sum = arithmeticOperator(intlit1.Value, intlit2.Value);

	//         return goal.ExpressionList[2].Unify(new PrologIntegerLiteral(sum));
	//     }

	// private applyBuiltInComparisonOperator(
	//     goal: PrologGoal,
	//     // Func<int, int, bool> intComparisonOperator,
	//     // Func<double, double, bool> floatComparisonOperator
	// 	comparisonOperator: (x: number, y: number) => boolean
	// ): ISubstitution | undefined {
	//     const lhsEvalated = goal.ExpressionList[0].EvaluateToNumber();
	//     const rhsEvalated = goal.ExpressionList[1].EvaluateToNumber();
	//
	//     if (typeof lhsEvalated === 'undefined' || typeof rhsEvalated === 'undefined') {
	//         return undefined;
	//     }
	//
	//     // let comparisonResult: boolean;
	// 	//
	//     // if (lhsEvalated is PrologIntegerLiteral && rhsEvalated is PrologIntegerLiteral)
	//     // {
	//     //     comparisonResult = intComparisonOperator(lhsEvalated.ToInteger(), rhsEvalated.ToInteger());
	//     // }
	//     // else
	//     // {
	//     //     comparisonResult = floatComparisonOperator(lhsEvalated.ToDouble(), rhsEvalated.ToDouble());
	//     // }
	//
	//     if (comparisonOperator(lhsEvalated.ToDouble(), rhsEvalated.ToDouble())) {
	//         return createSubstitution();
	//     }
	//
	//     return undefined;
	// }
	//
	// private greaterThan2(goal: PrologGoal): ISubstitution | undefined {
	//     return this.applyBuiltInComparisonOperator(goal, (x, y) => x > y);
	// }

	//     private PrologSubstitution GoalDisjunction2(
	//         PrologGoal goal,
	//         List<PrologGoal> goalList,
	//         List<CutDetector> cutDetectorList,
	//         int goalNum,
	//         PrologSubstitution oldSubstitution,
	//         HashSet<PrologVariable> parentVariablesToAvoid,
	//         List<PrologVariable> variablesInQuery,
	//         List<PrologModule> listOfCurrentModules)
	//     {
	//         var goal1 = ExpressionToGoal(goal.ExpressionList[0]);
	//         var goal2 = ExpressionToGoal(goal.ExpressionList[1]);
	//         var nextGoalNum = goalNum + 1;
	//         var cutDetector = cutDetectorList[goalNum];
	//         var currentModule = listOfCurrentModules[goalNum];
	//         PrologSubstitution localSubstitution;

	//         if (goal1 == null)
	//         {
	//             return null;
	//         }

	//         goalList.Insert(nextGoalNum, goal1);
	//         cutDetectorList.Insert(nextGoalNum, cutDetector);
	//         listOfCurrentModules.Insert(nextGoalNum, currentModule);

	//         try
	//         {
	//             localSubstitution = ProveGoalList(goalList, cutDetectorList, nextGoalNum, oldSubstitution, parentVariablesToAvoid, variablesInQuery,
	//                 listOfCurrentModules);
	//         }
	//         finally // In case a CutBacktrackException is thrown.
	//         {
	//             goalList.RemoveAt(nextGoalNum);
	//             cutDetectorList.RemoveAt(nextGoalNum);
	//             listOfCurrentModules.RemoveAt(nextGoalNum);
	//         }

	//         if (localSubstitution != null)
	//         {
	//             return localSubstitution;
	//         }

	//         if (goal2 == null)
	//         {
	//             return null;
	//         }

	//         goalList.Insert(nextGoalNum, goal2);
	//         cutDetectorList.Insert(nextGoalNum, cutDetector);
	//         listOfCurrentModules.Insert(nextGoalNum, currentModule);

	//         try
	//         {
	//             localSubstitution = ProveGoalList(goalList, cutDetectorList, nextGoalNum, oldSubstitution, parentVariablesToAvoid, variablesInQuery,
	//                 listOfCurrentModules);
	//         }
	//         finally // In case a CutBacktrackException is thrown.
	//         {
	//             goalList.RemoveAt(nextGoalNum);
	//             cutDetectorList.RemoveAt(nextGoalNum);
	//             listOfCurrentModules.RemoveAt(nextGoalNum);
	//         }

	//         return localSubstitution;
	//     }

	//     private PrologSubstitution IfThenElse3(
	//         //PrologGoal goal,
	//         IPrologExpression ifPart,
	//         IPrologExpression thenPart,
	//         IPrologExpression elsePart,
	//         List<PrologGoal> goalList,
	//         List<CutDetector> cutDetectorList,
	//         int goalNum,
	//         PrologSubstitution oldSubstitution,
	//         HashSet<PrologVariable> parentVariablesToAvoid,
	//         List<PrologVariable> variablesInQuery,
	//         List<PrologModule> listOfCurrentModules)
	//     {
	//         var conditionGoal = ExpressionToGoal(ifPart);

	//         if (conditionGoal == null)
	//         {
	//             return null;
	//         }

	//         var nextGoalNum = goalNum + 1;
	//         IPrologExpression chosenGoalAsExpression;
	//         var cutDetector = cutDetectorList[goalNum];
	//         var currentModule = listOfCurrentModules[goalNum];
	//         var cachedAllMode = allMode;
	//         var cachedSolutionCollectionMode = solutionCollectionMode;
	//         var tempGoalList = new List<PrologGoal>() { conditionGoal };
	//         // goalIfThenElse.ConditionGoal had better not be a cut;
	//         // it would not make much sense if it was, since it would always be satisfied, and it would do nothing.
	//         var tempCutDetectorList = new List<CutDetector>() { cutDetector };
	//         var tempListOfCurrentModules = new List<PrologModule>() { currentModule };
	//         PrologSubstitution localSubstitution;
	//         PrologSubstitution result;

	//         allMode = false;
	//         solutionCollectionMode = SolutionCollectionMode.None;

	//         try
	//         {
	//             localSubstitution = ProveGoalList(tempGoalList, tempCutDetectorList, 0, oldSubstitution, parentVariablesToAvoid, null, tempListOfCurrentModules);
	//         }
	//         finally
	//         {
	//             allMode = cachedAllMode;
	//             solutionCollectionMode = cachedSolutionCollectionMode;
	//         }

	//         if (localSubstitution != null)
	//         {
	//             chosenGoalAsExpression = thenPart;
	//             //localSubstitution = oldSubstitution.Compose(localSubstitution);
	//         }
	//         else
	//         {
	//             chosenGoalAsExpression = elsePart;
	//             localSubstitution = oldSubstitution;
	//         }

	//         var chosenGoal = ExpressionToGoal(chosenGoalAsExpression);

	//         if (chosenGoal == null)
	//         {
	//             return null;
	//         }

	//         goalList.Insert(nextGoalNum, chosenGoal);
	//         cutDetectorList.Insert(nextGoalNum, cutDetector);
	//         listOfCurrentModules.Insert(nextGoalNum, currentModule);

	//         try
	//         {
	//             result = ProveGoalList(goalList, cutDetectorList, nextGoalNum, localSubstitution, parentVariablesToAvoid, variablesInQuery, listOfCurrentModules);
	//         }
	//         finally
	//         {
	//             goalList.RemoveAt(nextGoalNum);
	//             cutDetectorList.RemoveAt(nextGoalNum);
	//             listOfCurrentModules.RemoveAt(nextGoalNum);
	//         }

	//         return result;
	//     }

	//     private PrologSubstitution Functor3(PrologGoal goal, HashSet<PrologVariable> parentVariablesToAvoid)
	//     {
	//         var firstArgAsFunctorExpression = goal.ExpressionList[0] as PrologNameExpression<PrologFunctor>;
	//         var firstArgAsVariable = goal.ExpressionList[0] as PrologVariable;
	//         var secondArgAsFunctorExpression = goal.ExpressionList[1] as PrologNameExpression<PrologFunctor>;
	//         var secondArgAsVariable = goal.ExpressionList[1] as PrologVariable;
	//         var thirdArgAsInteger = goal.ExpressionList[2] as PrologIntegerLiteral;
	//         var thirdArgAsVariable = goal.ExpressionList[2] as PrologVariable;
	//         var functorSubstitution = new PrologSubstitution();

	//         if (firstArgAsFunctorExpression != null)
	//         {
	//             var functorName = firstArgAsFunctorExpression.Name;
	//             var functorArity = firstArgAsFunctorExpression.ExpressionList.Count;

	//             if (secondArgAsFunctorExpression != null)
	//             {

	//                 if (secondArgAsFunctorExpression.Name != functorName || secondArgAsFunctorExpression.ExpressionList.Count != 0)
	//                 {
	//                     return null;
	//                 }
	//             }
	//             else if (secondArgAsVariable != null)
	//             {
	//                 //functorSubstitution.SubstitutionList[secondArgAsVariable] = CreateAtom(firstArgAsFunctorExpression.Name);
	//                 functorSubstitution = secondArgAsVariable.Unify(CreateAtom(firstArgAsFunctorExpression.Name));
	//             }
	//             else
	//             {
	//                 return null;
	//             }

	//             if (thirdArgAsVariable != null && functorSubstitution.SubstitutionList.Count > 0)
	//             {
	//                 var newThirdArg = thirdArgAsVariable.ApplySubstitution(functorSubstitution);

	//                 thirdArgAsInteger = newThirdArg as PrologIntegerLiteral;
	//                 thirdArgAsVariable = newThirdArg as PrologVariable;
	//             }

	//             if (thirdArgAsInteger != null)
	//             {

	//                 if (thirdArgAsInteger.Value != functorArity)
	//                 {
	//                     return null;
	//                 }
	//             }
	//             else if (thirdArgAsVariable != null)
	//             {
	//                 //var sub = new PrologSubstitution(thirdArgAsVariable, new PrologIntegerLiteral(functorArity));
	//                 // Use Unify() because thirdArgAsVariable could be a non-binding variable.
	//                 var sub = thirdArgAsVariable.Unify(new PrologIntegerLiteral(functorArity));

	//                 functorSubstitution = functorSubstitution.Compose(sub);
	//             }
	//             else
	//             {
	//                 return null;
	//             }
	//         }
	//         else if (firstArgAsVariable != null)
	//         {

	//             if (secondArgAsFunctorExpression != null && secondArgAsFunctorExpression.ExpressionList.Count == 0 &&
	//                 thirdArgAsInteger != null && thirdArgAsInteger.Value >= 0)
	//             {
	//                 var variablesToAvoid = goal.FindBindingVariables();

	//                 variablesToAvoid.unionInPlace(parentVariablesToAvoid);

	//                 var exprList = new List<IPrologExpression>();

	//                 for (var i = 0; i < thirdArgAsInteger.Value; ++i)
	//                 {
	//                     // TODO: This code is similar to code in ProveGoalList().  Factor out the common code.
	//                     PrologVariable v;

	//                     do
	//                     {
	//                         v = GetNextUniqueVariable();
	//                     }
	//                     while (variablesToAvoid.Contains(v));

	//                     exprList.Add(v);
	//                 }

	//                 functorSubstitution = firstArgAsVariable.Unify(new PrologNameExpression<PrologFunctor>(
	//                     gs,
	//                     secondArgAsFunctorExpression.Name,
	//                     exprList));
	//             }
	//             else
	//             {
	//                 return null;
	//             }
	//         }
	//         else
	//         {
	//             // The first argument is neither a functor expression nor a variable.
	//             // The second argument must be either a variable or firstArg.ToString().
	//             // The third argument must be either a variable or the integer zero.
	//             var firstArgAsString = goal.ExpressionList[0].ToString();

	//             if (secondArgAsFunctorExpression != null && secondArgAsFunctorExpression.Name == firstArgAsString &&
	//                 secondArgAsFunctorExpression.ExpressionList.Count == 0)
	//             {
	//             }
	//             else if (secondArgAsVariable != null)
	//             {
	//                 functorSubstitution = secondArgAsVariable.Unify(CreateAtom(firstArgAsString));
	//             }
	//             else
	//             {
	//                 return null;
	//             }

	//             if (thirdArgAsVariable != null && functorSubstitution.SubstitutionList.Count > 0)
	//             {
	//                 var newThirdArg = thirdArgAsVariable.ApplySubstitution(functorSubstitution);

	//                 thirdArgAsInteger = newThirdArg as PrologIntegerLiteral;
	//                 thirdArgAsVariable = newThirdArg as PrologVariable;
	//             }

	//             if (thirdArgAsInteger != null && thirdArgAsInteger.Value == 0)
	//             {
	//             }
	//             else if (thirdArgAsVariable != null)
	//             {
	//                 //var sub = new PrologSubstitution(thirdArgAsVariable, new PrologIntegerLiteral(0));
	//                 // Use Unify() because thirdArgAsVariable could be a non-binding variable.
	//                 var sub = thirdArgAsVariable.Unify(new PrologIntegerLiteral(0));

	//                 functorSubstitution = functorSubstitution.Compose(sub);
	//             }
	//             else
	//             {
	//                 return null;
	//             }
	//         }

	//         return functorSubstitution;
	//     }

	//     private void Print(PrologGoal unsubstitutedGoal, PrologGoal goal)
	//     {

	//         if (sbOutput.Length > 0)
	//         {
	//             sbOutput.AppendLine();
	//         }

	// #if OLD_PRINT
	//         sbOutput.Append(string.Join(" ", goal.ExpressionList));
	// #else
	//         var resultList = new List<string>();

	//         for (var i = 0; i < goal.ExpressionList.Count; ++i)
	//         {
	//             resultList.Add(string.Format("{0} = {1}", unsubstitutedGoal.ExpressionList[i], goal.ExpressionList[i]));
	//         }

	//         sbOutput.Append(string.Join(", ", resultList));

	//         if (allMode)
	//         {
	//             sbOutput.Append(";");
	//         }
	// #endif
	//     }

	private AutomaticPrint(variablesInQuery: IVariable[], substitution: ISubstitution): void {
		if (variablesInQuery.length === 0) {
			return;
		}

		const substitutionsForBindingVariables = createSubstitution();

		for (const v of variablesInQuery) {
			const key = v.toString();
			const value = substitution.SubstitutionList.get(key);

			if (typeof value !== 'undefined') {
				substitutionsForBindingVariables.SubstitutionList.set(key, value);
			}
		}

		this.printDirect(`Satisfying substitution is: ${substitutionsForBindingVariables}`);
	}

	// private Is2(goal: PrologGoal): PrologSubstitution | undefined {
	// 	const rhsEvaluated = goal.ExpressionList[1].EvaluateToNumber();

	// 	if (typeof rhsEvaluated === 'undefined') {
	// 		return undefined;
	// 	}

	// 	// if (goal.ExpressionList[0] is IPrologNumber) {
	// 	if (
	// 		goal.ExpressionList[0] instanceof PrologIntegerLiteral ||
	// 		goal.ExpressionList[0] instanceof PrologFloatLiteral
	// 	) {
	// 		// if (goal.ExpressionList[0].Equals(rhsEvaluated)) { // Remember that the int 1 does not equal the double 1.0 according to this code.
	// 		if (goal.ExpressionList[0].EvaluateToNumber() === rhsEvaluated) {
	// 			return new PrologSubstitution();
	// 		}
	// 	} else if (goal.ExpressionList[0] instanceof PrologVariable) {
	// 		//var newSubstitution = new PrologSubstitution((PrologVariable)goal.ExpressionList[0], rhsEvaluated);
	// 		// Use Unify() because goal.ExpressionList[0] could be a non-binding variable.

	// 		return goal.ExpressionList[0].Unify(rhsEvaluated);
	// 	}

	// 	return undefined;
	// }

	private Unifiable2(goal: PrologGoal): ISubstitution | undefined {
		return goal.ExpressionList[0].Unify(goal.ExpressionList[1]);
	}

	private NotUnifiable2(goal: PrologGoal): ISubstitution | undefined {
		const s = goal.ExpressionList[0].Unify(goal.ExpressionList[1]);

		if (typeof s !== 'undefined') {
			return undefined;
		}

		return createSubstitution();
	}

	private Equals2(goal: PrologGoal): ISubstitution | undefined {
		if (goal.ExpressionList[0].equals(goal.ExpressionList[1])) {
			return createSubstitution();
		}

		return undefined;
	}

	private NotEquals2(goal: PrologGoal): ISubstitution | undefined {
		if (goal.ExpressionList[0].equals(goal.ExpressionList[1])) {
			return undefined;
		}

		return createSubstitution();
	}

	//     private PrologSubstitution Tab1(PrologGoal goal) // See http://www.swi-prolog.org/pldoc/man?predicate=tab/1
	//     {
	//         var firstArgAsIntegerLiteral = goal.ExpressionList[0] as PrologIntegerLiteral;

	//         if (firstArgAsIntegerLiteral == null || firstArgAsIntegerLiteral.Value <= 0)
	//         {
	//             return null;
	//         }

	//         for (var i = 0; i < firstArgAsIntegerLiteral.Value; ++i)
	//         {
	//             sbOutput.Append(' ');
	//         }

	//         return new PrologSubstitution();
	//     }

	//     private PrologSubstitution Tab2(PrologGoal goal)
	//     {
	//         var firstArgAsFileWriter = goal.ExpressionList[0] as PrologFileWriter;
	//         var secondArgAsIntegerLiteral = goal.ExpressionList[1] as PrologIntegerLiteral;

	//         if (firstArgAsFileWriter != null && secondArgAsIntegerLiteral != null && //secondArgAsIntegerLiteral.Value > 0 &&
	//             firstArgAsFileWriter.Tab(secondArgAsIntegerLiteral.Value))
	//         {
	//             return new PrologSubstitution();
	//         }
	//         else
	//         {
	//             return null;
	//         }
	//     }

	//     private PrologSubstitution NL0(PrologGoal goal)
	//     {
	//         sbOutput.AppendLine();
	//         return new PrologSubstitution();
	//     }

	//     private PrologSubstitution NL1(PrologGoal goal)
	//     {
	//         var firstArgAsFileWriter = goal.ExpressionList[0] as PrologFileWriter;

	//         if (firstArgAsFileWriter != null && firstArgAsFileWriter.NewLine())
	//         {
	//             return new PrologSubstitution();
	//         }
	//         else
	//         {
	//             return null;
	//         }
	//     }

	//     private PrologSubstitution Atom1(PrologGoal goal)
	//     {
	//         var firstAndOnlyArgAsFunctorExpression = goal.ExpressionList[0] as PrologNameExpression<PrologFunctor>;

	//         if (firstAndOnlyArgAsFunctorExpression != null && firstAndOnlyArgAsFunctorExpression.ExpressionList.Count == 0)
	//         {
	//             return new PrologSubstitution();
	//         }
	//         else
	//         {
	//             return null;
	//         }
	//     }

	// private Integer1(goal: PrologGoal): PrologSubstitution | undefined {
	// 	if (goal.ExpressionList[0] instanceof PrologIntegerLiteral) {
	// 		return new PrologSubstitution();
	// 	}

	// 	return undefined;
	// }

	//     private PrologSubstitution Float1(PrologGoal goal)
	//     {

	//         if (goal.ExpressionList[0] is PrologFloatLiteral)
	//         {
	//             return new PrologSubstitution();
	//         }
	//         else
	//         {
	//             return null;
	//         }
	//     }

	//     private PrologSubstitution Number1(PrologGoal goal)
	//     {

	//         if (goal.ExpressionList[0] is IPrologNumber)
	//         {
	//             return new PrologSubstitution();
	//         }
	//         else
	//         {
	//             return null;
	//         }
	//     }

	//     private PrologSubstitution Atomic1(PrologGoal goal)
	//     {
	//         var firstAndOnlyArgAsFunctorExpression = goal.ExpressionList[0] as PrologNameExpression<PrologFunctor>;
	//         var firstAndOnlyArgIsAtom = firstAndOnlyArgAsFunctorExpression != null && firstAndOnlyArgAsFunctorExpression.ExpressionList.Count == 0;

	//         if (firstAndOnlyArgIsAtom || goal.ExpressionList[0] is IPrologNumber)
	//         {
	//             return new PrologSubstitution();
	//         }
	//         else
	//         {
	//             return null;
	//         }
	//     }

	// private Var1(goal: PrologGoal): PrologSubstitution | undefined {
	// 	if (goal.ExpressionList[0] instanceof PrologVariable) {
	// 		return new PrologSubstitution();
	// 	}

	// 	return undefined;
	// }

	// private NonVar1(goal: PrologGoal): PrologSubstitution | undefined {
	// 	if (goal.ExpressionList[0] instanceof PrologVariable) {
	// 		return undefined;
	// 	}

	// 	return new PrologSubstitution();
	// }

	//     private PrologSubstitution Ground1(PrologGoal goal)
	//     {

	//         if (goal.ExpressionList[0].IsGround)
	//         {
	//             return new PrologSubstitution();
	//         }
	//         else
	//         {
	//             return null;
	//         }
	//     }

	//     private PrologSubstitution Random2(PrologGoal goal)
	//     {
	//         var firstArgAsIntegerLiteral = goal.ExpressionList[0] as PrologIntegerLiteral;

	//         if (firstArgAsIntegerLiteral == null || firstArgAsIntegerLiteral.Value <= 0 || !(goal.ExpressionList[1] is PrologVariable))
	//         {
	//             return null;
	//         }

	//         var randomNumber = random.Next(firstArgAsIntegerLiteral.Value);

	//         // Use Unify() because goal.ExpressionList[1] could be a non-binding variable.
	//         return goal.ExpressionList[1].Unify(new PrologIntegerLiteral(randomNumber));
	//     }

	//     private PrologSubstitution Fail0(PrologGoal goal)
	//     {
	//         return null;
	//     }

	// eslint-disable-next-line @typescript-eslint/no-unused-vars
	// private Fail0(goal: PrologGoal): undefined {
	// 	return undefined;
	// }

	//     // "listing" and "listing(targetName)"; see http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlse4

	//     private PrologSubstitution Listing0(PrologGoal goal)
	//     {

	//         for each (var moduleName in dictModules.Keys)
	//         {
	//             sbOutput.AppendLine(string.Format("Module '{0}' :", moduleName));
	//             dictModules[moduleName].ClauseList.For Each(clause => sbOutput.AppendLine(clause.ToString()));
	//             sbOutput.AppendLine();
	//         }

	//         sbOutput.AppendLine("Default module:");
	//         DefaultModule.ClauseList.For Each(clause => sbOutput.AppendLine(clause.ToString()));
	//         return new PrologSubstitution();
	//     }

	//     private PrologSubstitution Listing1(PrologGoal goal)
	//     {
	//         string targetName = null;
	//         var functorExpression = goal.ExpressionList[0] as PrologNameExpression<PrologFunctor>;
	//         var variable = goal.ExpressionList[0] as PrologVariable;

	//         if (functorExpression != null && functorExpression.ExpressionList.Count == 0)
	//         {
	//             targetName = functorExpression.Name; // The name probably does not begin with a capital letter or an underscore, unless it was single quoted.
	//         }
	//         else if (variable != null)
	//         {
	//             targetName = variable.Name; // The name begins with a capital letter or an underscore.
	//         }

	//         if (targetName == null)
	//         {
	//             return null;
	//         }

	//         // TODO? Should we include clauses that contain mentions of targetName on their RHSs?

	//         for each (var moduleName in dictModules.Keys)
	//         {
	//             sbOutput.AppendLine(string.Format("Module '{0}' :", moduleName));
	//             dictModules[moduleName].ClauseList
	//                 .Where(clause => clause.Lhs.Name == targetName)
	//                 .ToList()
	//                 .For Each(clause => sbOutput.AppendLine(clause.ToString()));
	//             sbOutput.AppendLine();
	//         }

	//         sbOutput.AppendLine("Default module:");
	//         DefaultModule.ClauseList
	//             .Where(clause => clause.Lhs.Name == targetName)
	//             .ToList()
	//             .For Each(clause => sbOutput.AppendLine(clause.ToString()));
	//         return new PrologSubstitution();
	//     }

	//     private PrologSubstitution Arg3(PrologGoal goal)
	//     {
	//         var firstArgAsInteger = goal.ExpressionList[0] as PrologIntegerLiteral;
	//         var secondArgAsFunctorExpression = goal.ExpressionList[1] as PrologNameExpression<PrologFunctor>;

	//         if (firstArgAsInteger == null || firstArgAsInteger.Value <= 0 ||
	//             secondArgAsFunctorExpression == null || secondArgAsFunctorExpression.ExpressionList.Count < firstArgAsInteger.Value)
	//         {
	//             return null;
	//         }

	//         return secondArgAsFunctorExpression.ExpressionList[firstArgAsInteger.Value - 1].Unify(goal.ExpressionList[2]);
	//     }

	//     private PrologSubstitution Univ2(PrologGoal goal)
	//     {
	//         var firstArgAsFunctorExpression = goal.ExpressionList[0] as PrologNameExpression<PrologFunctor>;
	//         var firstArgAsVariable = goal.ExpressionList[0] as PrologVariable;
	//         var secondArgAsFunctorExpression = goal.ExpressionList[1] as PrologNameExpression<PrologFunctor>;
	//         var secondArgAsVariable = goal.ExpressionList[1] as PrologVariable;

	//         if (firstArgAsFunctorExpression != null)
	//         {
	//             var f = CreateAtom(firstArgAsFunctorExpression.Name);
	//             var list = new PrologNameExpression<PrologFunctor>(LanguageSelector.Prolog2, new PrologFunctor("."),
	//                 new List<IPrologExpression>() { f, CSharpListToPrologList(firstArgAsFunctorExpression.ExpressionList) });

	//             return list.Unify(goal.ExpressionList[1]);
	//         }
	//         else if (firstArgAsVariable != null)
	//         {
	//             var list = PrologListToCSharpList(goal.ExpressionList[1]);

	//             if (list == null || list.Count == 0)
	//             {
	//                 return null;
	//             }

	//             var f = list[0] as PrologNameExpression<PrologFunctor>;

	//             if (f == null || f.ExpressionList.Count != 0)
	//             {
	//                 return null;
	//             }

	//             // Unify instead of explicitly creating the substitution, because list may contain firstArgAsVariable,
	//             // or firstArgAsVariable may be non-binding.
	//             return firstArgAsVariable.Unify(new PrologNameExpression<PrologFunctor>(LanguageSelector.Prolog2, f.Name, list.Skip(1).ToList()));
	//         }
	//         else if (goal.ExpressionList[0] is IPrologNumber)
	//         {
	//             // Note: This case is not described in http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlse39
	//             var f = CreateAtom(goal.ExpressionList[0].ToString());
	//             var list = CSharpListToPrologList(new List<IPrologExpression>() { f });

	//             return list.Unify(goal.ExpressionList[1]);
	//         }

	//         return null;
	//     }

	//     private PrologSubstitution FindAll3(PrologGoal goal, HashSet<PrologVariable> parentVariablesToAvoid, PrologModule currentModule)
	//     {
	//         var secondArgAsFunctorExpression = goal.ExpressionList[1] as PrologNameExpression<PrologFunctor>;

	//         if (secondArgAsFunctorExpression == null)
	//         {
	//             return null;
	//         }

	//         var tempGoalList = new List<PrologGoal>() { secondArgAsFunctorExpression.ToGoal() }; // oldSubstitution has already been applied.
	//         var tempCutDetectorList = new List<CutDetector>() { null };
	//         var tempListOfCurrentModules = new List<PrologModule>() { currentModule };
	//         var cachedSolutionCollectionMode = solutionCollectionMode;
	//         var cachedFindAll_Expression = findAll_Expression;
	//         var cachedFindAll_ResultList = findAll_ResultList;
	//         var cachedAllMode = allMode;

	//         solutionCollectionMode = SolutionCollectionMode.FindAll;
	//         findAll_Expression = goal.ExpressionList[0];
	//         findAll_ResultList = new List<IPrologExpression>();
	//         allMode = true;

	//         try
	//         {
	//             ProveGoalList(tempGoalList, tempCutDetectorList, 0, new PrologSubstitution(), parentVariablesToAvoid, null, tempListOfCurrentModules);
	//             return goal.ExpressionList[2].Unify(CSharpListToPrologList(findAll_ResultList));
	//         }
	//         finally
	//         {
	//             solutionCollectionMode = cachedSolutionCollectionMode;
	//             findAll_Expression = cachedFindAll_Expression;
	//             findAll_ResultList = cachedFindAll_ResultList;
	//             allMode = cachedAllMode;
	//         }
	//     }

	private doIntegerArithmetic(op: string, n1: number, n2: number): number {
		switch (op) {
			case 'add':
			case '+':
				return n1 + n2;
			case 'sub':
			case '-':
				return n1 - n2;
			case 'mult':
			case '*':
				return n1 * n2;
			case 'div':
			case '/':
				if (n2 === 0) {
					throw new Error('doIntegerArithmetic() : Division by zero');
				}

				return Math.floor(n1 / n2);
			case 'mod':
			case '%':
				if (n2 === 0) {
					throw new Error('doIntegerArithmetic() : Modulus by zero');
				}

				return n1 % n2;
			default:
				throw new Error(`doIntegerArithmetic() : Unsupported operator '${op}'`);
		}
	}

	private doIntegerComparison(op: string, n1: number, n2: number): boolean {
		switch (op) {
			case 'lt':
			case '<':
				return n1 < n2;
			case 'le':
			case '=<':
				return n1 <= n2;
			case 'gt':
			case '>':
				return n1 > n2;
			case 'ge':
			case '>=':
				return n1 >= n2;
			case 'eq':
			case '=:=': // Prolog's arithmetic equals comparison operator
				return n1 === n2;
			case 'ne':
			case '=\\=': // Prolog's arithmetic not-equal comparison operator
				return n1 !== n2;
			default:
				throw new Error(`doIntegerComparison() : Unsupported operator '${op}'`);
		}
	}

	private createSubstitutionForArithmeticOperation(
		goal: PrologGoal,
		vi: number,
		n1i: number,
		n2i: number,
		op: string
	): ISubstitution | undefined {
		// TODO? : Return { match: boolean; substitution: ISubstitution | undefined; }
		// Then create an array or iterator or generator:
		// [() => this.createSubstitutionForArithmeticOperation(goal, a, b, c, op), ...]
		// and use .find() to find the first one (if any) with match === true
		const v = goal.ExpressionList[vi] as IVariable;
		const n1 = goal.ExpressionList[n1i] as PrologIntegerLiteral;
		const n2 = goal.ExpressionList[n2i] as PrologIntegerLiteral;

		let result = 0;

		if ((op === '/' || op === 'div') && n2.Value === 0) {
			if (n1.Value !== 0) {
				return undefined;
			}

			// We will define 0 / 0 to be 0, so that 0 * N = 0 => N = 0.
		} else {
			result = this.doIntegerArithmetic(op, n1.Value, n2.Value);
		}

		return createSubstitution(v.Name, new PrologIntegerLiteral(result));
	}

	private ProveGoalList(
		goalList: PrologGoal[],
		cutDetectorList: CutDetector[],
		goalNum: number,
		oldSubstitution: ISubstitution,
		parentVariablesToAvoid: IImmutableSet<IVariable>,
		variablesInQuery: IVariable[], // Print these variables and their values automatically upon success if there is no print() goal at the end
		listOfCurrentModules: PrologModule[]
	): ISubstitution | undefined {
		if (goalNum >= goalList.length) {
			// The goal list has been satisfied.

			// console.log(
			// 	`ProveGoalList() : The goal list of length ${goalList.length} has been satisfied`
			// );
			// console.log(
			// 	'ProveGoalList() : Found solution:',
			// 	oldSubstitution.toString()
			// );
			// this.printDirect(
			// 	`ProveGoalList() : Found solution: ${oldSubstitution}`
			// );

			// **** Begin automatic printing ****
			// const lastGoal =
			// 	goalList.length > 0 ? goalList[goalList.length - 1] : undefined;

			// if (
			// 	typeof lastGoal !== 'undefined' &&
			// 	lastGoal.Name !== 'print'
			// ) {
			// 	// Don't do automatic printing if the last goal was a print() goal.
			this.AutomaticPrint(variablesInQuery, oldSubstitution);
			this.numSolutionsFound++;
			// }

			// **** End automatic printing ****

			// To continue searching for other solutions (i.e. if we are operating in "all" mode rather than "first" mode), return null.

			if (this.allMode) {
				return undefined;
			}

			return oldSubstitution;
		}

		// #if SUBSTITUTION_KEY_COUNT_LIMIT
		if (oldSubstitution.SubstitutionList.size > 100) {
			throw new Error(
				'ProveGoalList() : **** Aborting because the substitution is too long. ****'
			);
		}
		// #endif

		// console.log('ProveGoalList() : The goal list is:');

		// for (let i = 0; i < goalList.length; i++) {
		// 	console.log(
		// 		`${i === goalNum ? '-> ' : ''}${i + 1} : ${goalList[i]}`
		// 	);
		// }

		// console.log(
		// 	`ProveGoalList() : Proving goal ${goalNum + 1} of ${
		// 		goalList.length
		// 	}...`
		// );

		const unsubstitutedGoal = goalList[goalNum];
		const currentModule = listOfCurrentModules[goalNum];
		const nextGoalNum = goalNum + 1;

		// console.log('unsubstitutedGoal is', unsubstitutedGoal.toString());
		// console.log('oldSubstitution is', oldSubstitution.toString());

		if (unsubstitutedGoal.isCut) {
			// The "cut" goal always succeeds.
			// console.log('ProveGoal: Detected a cut.');

			// 2014/03/07
			const cutDetector = cutDetectorList[goalNum];
			const cutSubstitution = this.ProveGoalList(
				goalList,
				cutDetectorList,
				nextGoalNum,
				oldSubstitution,
				parentVariablesToAvoid,
				variablesInQuery,
				listOfCurrentModules
			);

			if (typeof cutSubstitution === 'undefined' && typeof cutDetector !== 'undefined') {
				// We may not backtrack through a cut.
				throw new CutBacktrackException(cutDetector.guid);
			}

			return cutSubstitution;
		}

		const goal = unsubstitutedGoal.ApplySubstitution(oldSubstitution) as PrologGoal;

		// console.log(
		// 	'ProveGoalList() : 1) substituted goal is',
		// 	goal.toString()
		// );

		const numArgsInGoal = goal.ExpressionList.length;
		// const functionKey = new StringIntKey(goal.Name, numArgsInGoal);

		// console.log('Goal signature is', functionKey.toString());

		// if (dictBuiltInPredicates.ContainsKey(functionKey)) {
		// 	var unifier = dictBuiltInPredicates[functionKey](goal);

		// 	if (unifier == null) {
		// 		return null;
		// 	}

		// 	return ProveGoalList(goalList, cutDetectorList, nextGoalNum, oldSubstitution.Compose(unifier),
		// 	parentVariablesToAvoid, variablesInQuery, listOfCurrentModules);
		// }

		// if (gs === LanguageSelector.Prolog) // Built-in predicates that are used by Kamin's Prolog only.
		// {

		// switch (goal.Name)
		// {
		// case "print": // This can take any number of arguments.
		// Print(unsubstitutedGoal, goal);
		// return ProveGoalList(goalList, cutDetectorList, nextGoalNum, oldSubstitution, parentVariablesToAvoid, variablesInQuery,
		// listOfCurrentModules);

		// default:
		// break;
		// }
		// }

		// Built-in predicates:

		// TODO: Replace the next line with:
		// switch (`${goal.Name}/${numArgsInGoal}`) { ... }
		switch (goal.Name) {
			case 'fail':
				if (numArgsInGoal === 0) {
					return undefined;
				}

				break;

			// TODO: Remove 'add', 'sub', ... 'lt', 'gt', etc. Use the symbolic names.
			case 'add':
			case '+':
			case 'sub':
			case '-':
			case 'mult':
			case '*':
			case 'div':
			// case '/':
			case 'mod':
				// case '%':
				if (
					numArgsInGoal === 3 &&
					goal.ExpressionList[0] instanceof PrologIntegerLiteral &&
					goal.ExpressionList[1] instanceof PrologIntegerLiteral
				) {
					const n1 = goal.ExpressionList[0] as PrologIntegerLiteral;
					const n2 = goal.ExpressionList[1] as PrologIntegerLiteral;

					if (goal.ExpressionList[2] instanceof PrologIntegerLiteral) {
						const n3 = goal.ExpressionList[2] as PrologIntegerLiteral;
						const result = this.doIntegerArithmetic(goal.Name, n1.Value, n2.Value);

						if (n3.Value === result) {
							return this.ProveGoalList(
								goalList,
								cutDetectorList,
								nextGoalNum,
								oldSubstitution,
								parentVariablesToAvoid,
								variablesInQuery,
								listOfCurrentModules
							);
						} else {
							return undefined;
						}
					} else if (isIVariable(goal.ExpressionList[2])) {
						const v = goal.ExpressionList[2] as IVariable;
						const addSubstitution = createSubstitution(
							v.Name,
							new PrologIntegerLiteral(
								this.doIntegerArithmetic(goal.Name, n1.Value, n2.Value)
							)
						);

						return this.ProveGoalList(
							goalList,
							cutDetectorList,
							nextGoalNum,
							oldSubstitution.compose(addSubstitution),
							parentVariablesToAvoid,
							variablesInQuery,
							listOfCurrentModules
						);
					}
				}
				break;

			case 'lt': // '<':
			case '<':
			case 'le': // '<=':
			case '=<':
			case 'gt': // '>':
			case '>':
			case 'ge': // '>=':
			case '>=':
			case 'eq': // '=':
			case '=:=': // Prolog's arithmetic equals comparison operator
			case 'ne': // '!=':
			case '=\\=': // Prolog's arithmetic not-equal comparison operator
				if (
					numArgsInGoal === 2 &&
					goal.ExpressionList[0] instanceof PrologIntegerLiteral &&
					goal.ExpressionList[1] instanceof PrologIntegerLiteral
				) {
					const n1 = goal.ExpressionList[0] as PrologIntegerLiteral;
					const n2 = goal.ExpressionList[1] as PrologIntegerLiteral;

					if (!this.doIntegerComparison(goal.Name, n1.Value, n2.Value)) {
						return undefined;
					}

					return this.ProveGoalList(
						goalList,
						cutDetectorList,
						nextGoalNum,
						oldSubstitution,
						parentVariablesToAvoid,
						variablesInQuery,
						listOfCurrentModules
					);
				}
				break;

			case 'not':
			case '\\+':
				if (numArgsInGoal === 1) {
					const fe = goal.ExpressionList[0] as PrologFunctorExpression;
					const innerGoal = createGoalFromFunctorExpression(fe);

					const tempGoalList = [innerGoal];
					// This next line prevents us from adding "not" to the built-in predicates dictionary:
					const tempCutDetectorList = [cutDetectorList[goalNum]];
					const tempListOfCurrentModules = [listOfCurrentModules[goalNum]];
					const cachedAllMode = this.allMode;
					const cachedSolutionCollectionMode = this.solutionCollectionMode;
					let localSubstitution: ISubstitution | undefined;

					this.allMode = false;
					this.solutionCollectionMode = SolutionCollectionMode.None;

					try {
						// We don't need to use parentVariablesToAvoid here, since we don't propagate localSubstitution.
						localSubstitution = this.ProveGoalList(
							tempGoalList,
							tempCutDetectorList,
							0,
							createSubstitution(),
							innerGoal.FindBindingVariables(),
							[],
							tempListOfCurrentModules
						);
					} finally {
						this.allMode = cachedAllMode;
						this.solutionCollectionMode = cachedSolutionCollectionMode;
					}

					if (typeof localSubstitution !== 'undefined') {
						return undefined;
					}

					return this.ProveGoalList(
						goalList,
						cutDetectorList,
						nextGoalNum,
						oldSubstitution,
						parentVariablesToAvoid,
						variablesInQuery,
						listOfCurrentModules
					);
				}

				break;

			case 'is':
				if (numArgsInGoal === 2) {
					const e0 = goal.ExpressionList[0];
					const n1 = goal.ExpressionList[1].EvaluateToNumber();

					if (typeof n1 === 'undefined') {
						console.error(
							`Goal is/2 : '${goal.ExpressionList[1]}' does not evaluate to a number.`
						);

						return undefined;
					} else if (e0 instanceof PrologIntegerLiteral) {
						if (e0.EvaluateToNumber() !== n1) {
							return undefined;
						}

						this.ProveGoalList(
							goalList,
							cutDetectorList,
							nextGoalNum,
							oldSubstitution,
							parentVariablesToAvoid,
							variablesInQuery,
							listOfCurrentModules
						);
					} else if (isIVariable(e0)) {
						// We can say e0.Name because we already know that
						// e0 instanceof PrologVariable is true.
						return this.ProveGoalList(
							goalList,
							cutDetectorList,
							nextGoalNum,
							oldSubstitution.compose(createSubstitution((e0 as IVariable).Name, n1)),
							parentVariablesToAvoid,
							variablesInQuery,
							listOfCurrentModules
						);
					} else {
						console.error(
							`Goal is/3 : '${goal.ExpressionList[0]}' is not an IntegerLiteral or a Variable.`
						);

						return undefined;
					}
				}

				break;

			// case "functor":
			// 	// See http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlse39

			// 	if (numArgsInGoal == 3) // functor/3
			// 	{
			// 	var functorSubstitution = Functor3(goal, parentVariablesToAvoid);

			// 	if (functorSubstitution == null)
			// 	{
			// 	return null;
			// 	}

			// 	return ProveGoalList(goalList, cutDetectorList, nextGoalNum, oldSubstitution.Compose(functorSubstitution),
			// 	parentVariablesToAvoid, variablesInQuery, listOfCurrentModules);
			// 	}

			// 	break;

			case '=':
				if (numArgsInGoal === 2) {
					return this.Unifiable2(goal);
				}

				break;

			case '\\=':
				if (numArgsInGoal === 2) {
					return this.NotUnifiable2(goal);
				}

				break;

			case '==':
				if (numArgsInGoal === 2) {
					return this.Equals2(goal);
				}

				break;

			case '\\==':
				if (numArgsInGoal === 2) {
					return this.NotEquals2(goal);
				}

				break;

			// case "findall": // See http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlse49

			// 	if (numArgsInGoal == 3) // findall/3
			// 	{
			// 	var unifier = FindAll3(goal, parentVariablesToAvoid, currentModule);

			// 	if (unifier == null)
			// 	{
			// 	return null;
			// 	}

			// 	return ProveGoalList(goalList, cutDetectorList, nextGoalNum, oldSubstitution.Compose(unifier),
			// 	parentVariablesToAvoid, variablesInQuery, listOfCurrentModules);
			// 	}

			// 	break;

			// case "goal_disjunction":
			// case ";":

			// 	if (numArgsInGoal == 2)
			// 	{
			// 	return GoalDisjunction2(goal, goalList, cutDetectorList, goalNum, oldSubstitution, parentVariablesToAvoid, variablesInQuery,
			// 	listOfCurrentModules);
			// 	}

			// 	break;

			// case "if_then_else":

			// 	if (numArgsInGoal == 3)
			// 	{
			// 	return IfThenElse3(goal.ExpressionList[0], goal.ExpressionList[1], goal.ExpressionList[2],
			// 	goalList, cutDetectorList, goalNum, oldSubstitution, parentVariablesToAvoid, variablesInQuery, listOfCurrentModules);
			// 	}

			// 	break;

			// case "->":

			// 	if (numArgsInGoal == 2)
			// 	{
			// 	var thenElseGoal = ExpressionToGoal(goal.ExpressionList[1]);

			// 	if (thenElseGoal != null && thenElseGoal.Name == ":" && thenElseGoal.ExpressionList.length == 2)
			// 	{
			// 	return IfThenElse3(goal.ExpressionList[0], thenElseGoal.ExpressionList[0], thenElseGoal.ExpressionList[1],
			// 	goalList, cutDetectorList, goalNum, oldSubstitution, parentVariablesToAvoid, variablesInQuery, listOfCurrentModules);
			// 	}
			// 	}

			// 	break;

			// case "retract":

			// 	if (numArgsInGoal == 1)
			// 	{
			// 	var unifier = Retract1(goal, oldSubstitution, parentVariablesToAvoid);

			// 	if (unifier == null)
			// 	{
			// 	return null;
			// 	}

			// 	return ProveGoalList(goalList, cutDetectorList, nextGoalNum, oldSubstitution.Compose(unifier),
			// 	parentVariablesToAvoid, variablesInQuery, listOfCurrentModules);
			// 	}

			// 	break;

			// case "retractall":

			// 	if (numArgsInGoal == 1)
			// 	{
			// 	var unifier = RetractAll1(goal, oldSubstitution, parentVariablesToAvoid);

			// 	if (unifier == null)
			// 	{
			// 	return null;
			// 	}

			// 	return ProveGoalList(goalList, cutDetectorList, nextGoalNum, oldSubstitution.Compose(unifier),
			// 	parentVariablesToAvoid, variablesInQuery, listOfCurrentModules);
			// 	}

			// 	break;

			default:
				break;
		}

		// **** BEGIN Special case: E.g. add(2, N, 5) ****

		if (
			goal.Name === 'add' &&
			numArgsInGoal === 3 &&
			goal.ExpressionList[0] instanceof PrologIntegerLiteral &&
			isIVariable(goal.ExpressionList[1]) &&
			goal.ExpressionList[2] instanceof PrologIntegerLiteral
		) {
			// If 2 + N === 5 then N === 5 - 2

			// const n0 = goal.ExpressionList[0] as PrologIntegerLiteral;
			// const v1 = goal.ExpressionList[1] as IVariable;
			// const n2 = goal.ExpressionList[2] as PrologIntegerLiteral;
			//
			// const addSubstitution = createSubstitution(
			// 	v1.Name,
			// 	new PrologIntegerLiteral(this.doIntegerArithmetic('sub', n2.Value, n0.Value))
			// );

			// const addSubstitution = this.createSubstitutionForArithmeticOperation(goal.ExpressionList[1] as IVariable, goal.ExpressionList[2] as PrologIntegerLiteral, goal.ExpressionList[0] as PrologIntegerLiteral, '-');

			const addSubstitution = this.createSubstitutionForArithmeticOperation(
				goal,
				1,
				2,
				0,
				'-'
			);

			if (typeof addSubstitution === 'undefined') {
				return undefined;
			}

			return this.ProveGoalList(
				goalList,
				cutDetectorList,
				nextGoalNum,
				oldSubstitution.compose(addSubstitution),
				parentVariablesToAvoid,
				variablesInQuery,
				listOfCurrentModules
			);
		} else if (
			goal.Name === 'add' &&
			numArgsInGoal === 3 &&
			isIVariable(goal.ExpressionList[0]) &&
			goal.ExpressionList[1] instanceof PrologIntegerLiteral &&
			goal.ExpressionList[2] instanceof PrologIntegerLiteral
		) {
			// If N + 2 === 5 then N === 5 - 2
			const v0 = goal.ExpressionList[0] as IVariable;
			const n1 = goal.ExpressionList[1] as PrologIntegerLiteral;
			const n2 = goal.ExpressionList[2] as PrologIntegerLiteral;

			const addSubstitution = createSubstitution(
				v0.Name,
				new PrologIntegerLiteral(this.doIntegerArithmetic('sub', n2.Value, n1.Value))
			);

			return this.ProveGoalList(
				goalList,
				cutDetectorList,
				nextGoalNum,
				oldSubstitution.compose(addSubstitution),
				parentVariablesToAvoid,
				variablesInQuery,
				listOfCurrentModules
			);
		} else if (
			goal.Name === 'sub' &&
			numArgsInGoal === 3 &&
			goal.ExpressionList[0] instanceof PrologIntegerLiteral &&
			isIVariable(goal.ExpressionList[1]) &&
			goal.ExpressionList[2] instanceof PrologIntegerLiteral
		) {
			// If 5 - N === 2 then N === 5 - 2
			const n0 = goal.ExpressionList[0] as PrologIntegerLiteral;
			const v1 = goal.ExpressionList[1] as IVariable;
			const n2 = goal.ExpressionList[2] as PrologIntegerLiteral;

			const addSubstitution = createSubstitution(
				v1.Name,
				new PrologIntegerLiteral(this.doIntegerArithmetic('sub', n0.Value, n2.Value))
			);

			return this.ProveGoalList(
				goalList,
				cutDetectorList,
				nextGoalNum,
				oldSubstitution.compose(addSubstitution),
				parentVariablesToAvoid,
				variablesInQuery,
				listOfCurrentModules
			);
		} else if (
			goal.Name === 'sub' &&
			numArgsInGoal === 3 &&
			isIVariable(goal.ExpressionList[0]) &&
			goal.ExpressionList[1] instanceof PrologIntegerLiteral &&
			goal.ExpressionList[2] instanceof PrologIntegerLiteral
		) {
			// If N - 2 === 3 then N === 2 + 3
			const v0 = goal.ExpressionList[0] as IVariable;
			const n1 = goal.ExpressionList[1] as PrologIntegerLiteral;
			const n2 = goal.ExpressionList[2] as PrologIntegerLiteral;

			const addSubstitution = createSubstitution(
				v0.Name,
				new PrologIntegerLiteral(this.doIntegerArithmetic('add', n1.Value, n2.Value))
			);

			return this.ProveGoalList(
				goalList,
				cutDetectorList,
				nextGoalNum,
				oldSubstitution.compose(addSubstitution),
				parentVariablesToAvoid,
				variablesInQuery,
				listOfCurrentModules
			);
		} else if (
			goal.Name === 'mult' &&
			numArgsInGoal === 3 &&
			goal.ExpressionList[0] instanceof PrologIntegerLiteral &&
			isIVariable(goal.ExpressionList[1]) &&
			goal.ExpressionList[2] instanceof PrologIntegerLiteral
		) {
			// If 7 * N === 91 then N === 91 / 7 === 13
			const n0 = goal.ExpressionList[0] as PrologIntegerLiteral;
			const v1 = goal.ExpressionList[1] as IVariable;
			const n2 = goal.ExpressionList[2] as PrologIntegerLiteral;

			// Handle unsolvable cases like 1 * N === 0
			// Also handle solvable cases like 0 * N === 0
			let solution = 0;

			if (n0.Value === 0) {
				if (n2.Value !== 0) {
					return undefined;
				}
			} else {
				solution = n2.Value / n0.Value;
			}

			const addSubstitution = createSubstitution(v1.Name, new PrologIntegerLiteral(solution));

			return this.ProveGoalList(
				goalList,
				cutDetectorList,
				nextGoalNum,
				oldSubstitution.compose(addSubstitution),
				parentVariablesToAvoid,
				variablesInQuery,
				listOfCurrentModules
			);
		} else if (
			goal.Name === 'mult' &&
			numArgsInGoal === 3 &&
			isIVariable(goal.ExpressionList[0]) &&
			goal.ExpressionList[1] instanceof PrologIntegerLiteral &&
			goal.ExpressionList[2] instanceof PrologIntegerLiteral
		) {
			// If N * 13 === 91 then N === 91 / 13 === 7
			const v0 = goal.ExpressionList[0] as IVariable;
			const n1 = goal.ExpressionList[1] as PrologIntegerLiteral;
			const n2 = goal.ExpressionList[2] as PrologIntegerLiteral;

			// Handle unsolvable cases like N * 1 === 0
			// Also handle solvable cases like N * 0 === 0
			let solution = 0;

			if (n1.Value === 0) {
				if (n2.Value !== 0) {
					return undefined;
				}
			} else {
				solution = n2.Value / n1.Value;
			}

			const addSubstitution = createSubstitution(v0.Name, new PrologIntegerLiteral(solution));

			return this.ProveGoalList(
				goalList,
				cutDetectorList,
				nextGoalNum,
				oldSubstitution.compose(addSubstitution),
				parentVariablesToAvoid,
				variablesInQuery,
				listOfCurrentModules
			);
			// TODO: Support this div case: 91 / N === 7
		} else if (
			goal.Name === 'div' &&
			numArgsInGoal === 3 &&
			isIVariable(goal.ExpressionList[0]) &&
			goal.ExpressionList[1] instanceof PrologIntegerLiteral &&
			goal.ExpressionList[2] instanceof PrologIntegerLiteral
		) {
			// If N / 13 === 7 then N === 13 * 7 === 91
			const v0 = goal.ExpressionList[0] as IVariable;
			const n1 = goal.ExpressionList[1] as PrologIntegerLiteral;
			const n2 = goal.ExpressionList[2] as PrologIntegerLiteral;

			const addSubstitution = createSubstitution(
				v0.Name,
				new PrologIntegerLiteral(this.doIntegerArithmetic('mult', n1.Value, n2.Value))
			);

			return this.ProveGoalList(
				goalList,
				cutDetectorList,
				nextGoalNum,
				oldSubstitution.compose(addSubstitution),
				parentVariablesToAvoid,
				variablesInQuery,
				listOfCurrentModules
			);
		}

		// **** END Special case: E.g. add(2, N, 5) ****

		let resultSubstitution = this.ProveGoalListUsingModule(
			goal,
			goalList,
			cutDetectorList,
			nextGoalNum,
			oldSubstitution,
			parentVariablesToAvoid,
			variablesInQuery,
			currentModule,
			listOfCurrentModules
		);

		if (typeof resultSubstitution !== 'undefined') {
			// console.log(
			// 	'ProveGoalList() : 1) Returning resultSubstitution:',
			// 	resultSubstitution.toString()
			// );

			return resultSubstitution;
		}

		const goalSignature = new StringIntKey(goal.Name, goal.ExpressionList.length).toString();

		for (const key of currentModule.ImportList.keys()) {
			const v = currentModule.ImportList.get(key);

			if (key === goalSignature && typeof v !== 'undefined') {
				resultSubstitution = this.ProveGoalListUsingModule(
					goal,
					goalList,
					cutDetectorList,
					nextGoalNum,
					oldSubstitution,
					parentVariablesToAvoid,
					variablesInQuery,
					v,
					listOfCurrentModules
				);

				if (typeof resultSubstitution !== 'undefined') {
					// console.log(
					// 	'ProveGoalList() : 2) Returning resultSubstitution:',
					// 	resultSubstitution.toString()
					// );

					return resultSubstitution;
				}
			}
		}

		// console.log(
		// 	'ProveGoalList() : No resultSubstitution; returning undefined'
		// );

		return undefined;
	}

	private ProveGoalListUsingModule(
		goal: PrologGoal,
		goalList: PrologGoal[],
		cutDetectorList: CutDetector[],
		nextGoalNum: number,
		oldSubstitution: ISubstitution,
		parentVariablesToAvoid: IImmutableSet<IVariable>,
		variablesInQuery: IVariable[], // Print these variables and their values automatically upon success if there is no print() goal at the end
		currentModule: PrologModule,
		listOfCurrentModules: PrologModule[]
	): ISubstitution | undefined {
		// console.log('ProveGoalListUsingModule() : currentModule name is', currentModule.Name);

		const variablesToAvoid = goal
			.FindBindingVariables()
			.union(parentVariablesToAvoid)
			// variablesToAvoid.unionInPlace(oldSubstitution.FindBindingVariables());
			.union(findBindingVariablesInSubstitution(oldSubstitution));

		// console.log(
		// 	`ProveGoalListUsingModule() : The variables to avoid for goal ${goal} are:`
		// );

		// for (const bv of variablesToAvoid) {
		// 	console.log(`  ${bv}`);
		// }

		// Iterate over a copy of the ClauseList to protect against InvalidOperationExceptions due to assert*/retract*.
		const clauseListCopy = currentModule.ClauseList.slice(0);

		// console.log(
		// 	'typeof clauseListCopy is:',
		// 	typeof clauseListCopy,
		// 	clauseListCopy.constructor.name
		// );

		// console.log('ProveGoalListUsingModule() : goal is:', goal.toString());
		// console.log(
		// 	`ProveGoalListUsingModule() : Module contains ${currentModule.ClauseList.length} clause(s).`
		// );

		for (const clause of clauseListCopy) {
			// console.log(
			// 	'typeof clause is:',
			// 	typeof clause,
			// 	clause.constructor.name
			// );
			// console.log(
			// 	'ProveGoalListUsingModule() : clause is:',
			// 	clause.toString()
			// );

			const newClause = clause.RenameVariables(variablesToAvoid, this);

			// console.log(
			// 	'ProveGoalListUsingModule() : clause with renamed variables is:',
			// 	newClause.toString()
			// );

			// #if CONSOLE_WRITELINE
			// Console.WriteLine("ProveGoal: Trying to unify goal {0} with Lhs of clause {1}", goal, newClause);
			// #endif
			// console.log(
			// 	`ProveGoalListUsingModule() : Attempting to unify ${goal} with the clause LHS ${newClause.Lhs}...`
			// );

			const unifier = newClause.Lhs.Unify(goal);

			if (typeof unifier === 'undefined') {
				// console.log('ProveGoalListUsingModule() : Unification failed.');

				continue;
			}

			// console.log(
			// 	`ProveGoalListUsingModule() : goal ${goal} unifies with Lhs of clause ${newClause}`
			// );
			// console.log(`ProveGoalListUsingModule() : unifier is: ${unifier}`);

			// //Console.WriteLine("ProveGoal: Composing unifier with substitution: {0}", oldSubstitution);

			let localSubstitution: ISubstitution | undefined = oldSubstitution.compose(unifier);

			// console.log(
			// 	`ProveGoalListUsingModule() : The composition of substitutions ${oldSubstitution} and ${unifier} is ${localSubstitution}`
			// );

			// See the program F2.16.txt for a test of the cut.
			const newVariablesToAvoid = this.GetVariablesFromGoalList(newClause.Rhs).union(
				variablesToAvoid
			);

			// ThAW 2014/03/06 : We want to support cuts in goal disjunctions and if/then/else constructs.
			const cutDetector = new CutDetector(this.getNextGuid());

			const goalListLengthBeforeSplice = goalList.length;

			// console.log('goalList length before splice:', goalList.length);
			// goalList.InsertRange(nextGoalNum, newClause.Rhs);
			goalList.splice(nextGoalNum, 0, ...newClause.Rhs);
			// console.log('goalList length after splice:', goalList.length);

			if (goalList.length !== goalListLengthBeforeSplice + newClause.Rhs.length) {
				throw new Error('BOOM: goalListLengthBeforeSplice');
			}

			// Insert as many copies of the cutDetector reference as we have subgoals in newClause.Rhs .
			// cutDetectorList.InsertRange(nextGoalNum, newClause.Rhs.Select(g => cutDetector));

			// listOfCurrentModules.InsertRange(
			// 	nextGoalNum,
			// 	newClause.Rhs.Select((g) => currentModule)
			// );

			for (let i = 0; i < newClause.Rhs.length; i++) {
				cutDetectorList.splice(nextGoalNum, 0, cutDetector);
				listOfCurrentModules.splice(nextGoalNum, 0, currentModule);
			}

			try {
				localSubstitution = this.ProveGoalList(
					goalList,
					cutDetectorList,
					nextGoalNum,
					localSubstitution,
					newVariablesToAvoid,
					variablesInQuery,
					listOfCurrentModules
				);
			} catch (
				error // 2014/03/07
			) {
				// Clean up the lists before we return or re-throw.
				//goalList.RemoveRange(nextGoalNum, newClause.Rhs.Count);
				//cutDetectorList.RemoveRange(nextGoalNum, newClause.Rhs.Count);

				if (!(error instanceof CutBacktrackException) || error.guid !== cutDetector.guid) {
					throw error;
				}

				return undefined;
			} finally {
				// console.log(
				// 	'goalList length before undo of splice:',
				// 	goalList.length
				// );
				// goalList.RemoveRange(nextGoalNum, newClause.Rhs.Count);
				goalList.splice(nextGoalNum, newClause.Rhs.length);
				// console.log(
				// 	'goalList length after undo of splice:',
				// 	goalList.length
				// );

				// cutDetectorList.RemoveRange(nextGoalNum, newClause.Rhs.Count);
				cutDetectorList.splice(nextGoalNum, newClause.Rhs.length);

				// listOfCurrentModules.RemoveRange(
				// 	nextGoalNum,
				// 	newClause.Rhs.Count
				// );
				listOfCurrentModules.splice(nextGoalNum, newClause.Rhs.length);
			}

			if (goalList.length !== goalListLengthBeforeSplice) {
				throw new Error('BOOM: goalListLength after Splice');
			}

			if (typeof localSubstitution !== 'undefined') {
				return localSubstitution;
			}
		}

		// #if CONSOLE_WRITELINE
		// Console.WriteLine("ProveGoal: *** Could not prove goal {0}", goal);
		// Console.WriteLine("ProveGoal: oldSubstitution is: {0}", oldSubstitution);
		// #endif

		return undefined;
	}

	//     private bool ClauseIsIsomorphicToMemberOfClauseList(PrologClause clause, PrologModule currentModule)
	//     {
	//         var variablesToAvoid = clause.FindBindingVariables();

	//         return currentModule.ClauseList.Any(otherClause => clause.IsIsomorphicTo(otherClause, variablesToAvoid, this));
	//     }

	//     private bool ClauseIsNoMoreGeneralThanMemberOfClauseList(PrologClause clause, PrologModule currentModule)
	//     {
	//         var variablesToAvoid = clause.FindBindingVariables();

	//         for each (var otherClause in currentModule.ClauseList)
	//         {
	//             var renamedClause = otherClause.RenameVariables(variablesToAvoid, this);
	//             var unifier = renamedClause.Unify(clause);

	//             if (unifier != null && renamedClause.ApplySubstitution(unifier).Equals(clause))
	//             {
	//                 return true;
	//             }
	//         }

	//         return false;
	//     }

	public ProcessInput(parseResult: PrologClause | PrologGoal[], currentModuleName = ''): string {
		// #if SUPPORT_USER_DEFINED_OPERATORS
		const inputTypeName = parseResult.constructor.name;

		// console.log('ProcessInput() : Type of parseResult is:', inputTypeName);

		// const inputAsFunctorExpression =
		// 	parseResult as PrologNameExpression<PrologFunctor>;

		// if (typeof inputAsFunctorExpression !== 'undefined' && inputAsFunctorExpression.Name == ':-' && inputAsFunctorExpression.ExpressionList.length === 1) {
		// if (inputTypeName === PrologNameExpression<PrologFunctor>.name && inputAsFunctorExpression.Name === ':-' && inputAsFunctorExpression.ExpressionList.length === 1) {
		// 	return ProcessCommand(inputAsFunctorExpression.ExpressionList[0], currentModuleName);
		// }
		// #endif

		const clause = parseResult as PrologClause;
		const goalList = parseResult as PrologGoal[];

		// console.log(
		// 	'ProcessInput() : parseResult as PrologClause is:',
		// 	clause.toString()
		// );
		// console.log(
		// 	'ProcessInput() : parseResult as PrologGoal[] is:',
		// 	goalList
		// );
		// console.log(
		// 	'ProcessInput() : typeof parseResult.length is:',
		// 	typeof goalList.length
		// );

		this.clearPrintedText();

		// if (parseResult is PrologClause) {
		// if (inputTypeName === PrologClause.name) {
		// if (typeof goalList.length === 'undefined') {
		if (clause instanceof PrologClause) {
			// var clause = (PrologClause)parseResult;
			const currentModule = this.FindModule(currentModuleName);

			// TODO: To search for a clause in the current module's clause
			// list, first convert the clause to a string.

			// if (typeof currentModule.ClauseList.find(clause) !== 'undefined') {
			// 	return PrologGlobalInfo.ClauseAlreadyExists;
			// 	// } else if (
			// 	// 	this.ClauseIsIsomorphicToMemberOfClauseList(
			// 	// 		clause,
			// 	// 		currentModule
			// 	// 	)
			// 	// ) {
			// 	// 	return PrologGlobalInfo.IsomorphicClauseAlreadyExists;
			// 	// } else if (
			// 	// 	this.ClauseIsNoMoreGeneralThanMemberOfClauseList(
			// 	// 		clause,
			// 	// 		currentModule
			// 	// 	)
			// 	// ) {
			// 	// 	return PrologGlobalInfo.IsomorphicOrMoreGeneralClauseAlreadyExists;
			// }

			//Console.WriteLine("Adding clause '{0}' to module '{1}'.", clause, currentModuleName);
			// console.log(`Adding clause '${clause}' to module '${currentModuleName}'.`);
			currentModule.ClauseList.push(clause);

			return PrologGlobalInfo.ClauseAdded;
		} else if (
			goalList instanceof Array &&
			goalList.length > 0 &&
			// goalList[0] instanceof PrologGoal
			goalList.every((g: unknown) => g instanceof PrologGoal)
		) {
			// We are performing a query.
			this.numSolutionsFound = 0;

			// var goalList = new List<PrologGoal>((List<PrologGoal>)parseResult);
			const cutDetectorList: CutDetector[] = [];
			const listOfCurrentModules: PrologModule[] = [];
			const cutDetector = new CutDetector(this.getNextGuid());
			let substitution: ISubstitution | undefined;

			// sbOutput.Clear();
			// goalList.For Each(g => cutDetectorList.Add(cutDetector));
			// goalList.for Each((g: PrologGoal) =>
			// 	listOfCurrentModules.push(this.DefaultModule)
			// );

			// console.log(
			// 	`1) goalList of length ${goalList.length} is:`,
			// 	goalList.join(', ')
			// );

			for (let i = 0; i < goalList.length; i++) {
				cutDetectorList.push(cutDetector);
				listOfCurrentModules.push(this.DefaultModule);
			}

			try {
				substitution = this.ProveGoalList(
					goalList,
					cutDetectorList,
					0,
					createSubstitution(),
					this.GetVariablesFromGoalList(goalList),
					this.GetListOfBindingVariablesFromGoalList(goalList),
					listOfCurrentModules
				);
			} catch (error) {
				if (!(error instanceof CutBacktrackException) || error.guid !== cutDetector.guid) {
					throw error;
				}
			}

			// if (sbOutput.Length > 0) {
			// 	sbOutput.AppendLine();
			// }

			// sbOutput.Append(substitution != null ? Satisfied : NotSatisfied);

			// return sbOutput.ToString();

			// if (typeof substitution !== 'undefined') {
			// 	// console.log(
			// 	// 	'Satisfying substitution is:',
			// 	// 	substitution.toString()
			// 	// );

			// 	// console.log(
			// 	// 	`2) goalList of length ${goalList.length} is:`,
			// 	// 	goalList.join(', ')
			// 	// );

			// 	// Create a subset of 'substitution' that contains only the values for the binding variables in goalList.
			// 	const setOfBindingVariables = new Set<PrologVariable>();

			// 	for (const goal of goalList) {
			// 		setOfBindingVariables.unionInPlace(
			// 			goal.FindBindingVariables()
			// 		);
			// 	}

			// 	// console.log(
			// 	// 	'setOfBindingVariables:',
			// 	// 	setOfBindingVariables.toArray().join(', ')
			// 	// );

			// 	// const substitutionsForBindingVariables =
			// 	// 	new PrologSubstitution();

			// 	// for (const v of setOfBindingVariables) {
			// 	// 	const key = v.toString();
			// 	// 	const value = substitution.SubstitutionList.get(key);

			// 	// 	if (typeof value !== 'undefined') {
			// 	// 		substitutionsForBindingVariables.SubstitutionList.set(
			// 	// 			key,
			// 	// 			value
			// 	// 		);
			// 	// 	}
			// 	// }

			// 	// return (
			// 	// 	this.getPrintedText() +
			// 	// 	`Satisfying substitution is: ${substitutionsForBindingVariables.toString()}\n${
			// 	// 		PrologGlobalInfo.Satisfied
			// 	// 	}`
			// 	// );

			// 	this.AutomaticPrint(setOfBindingVariables, substitution);
			// }

			let satisfied: boolean;

			if (this.allMode) {
				this.printDirect(`Number of solutions found: ${this.numSolutionsFound}`);
				satisfied = this.numSolutionsFound > 0;
				// return (
				// 	this.getPrintedText() + `\n${PrologGlobalInfo.NotSatisfied}`
				// );
			} else {
				satisfied = typeof substitution !== 'undefined';
			}

			this.printDirect(
				satisfied ? PrologGlobalInfo.Satisfied : PrologGlobalInfo.NotSatisfied
			);

			return this.getPrintedText();
		} else if (typeof parseResult === 'undefined') {
			throw new Error('PrologGlobalInfo.ProcessInput() : parseResult is undefined');
		} else {
			throw new Error(
				`PrologGlobalInfo.ProcessInput() : parseResult is of unrecognized type ${inputTypeName}`
			);
		}
	}

	// eslint-disable-next-line @typescript-eslint/no-unused-vars
	private FindModule(filePath: string): PrologModule {
		// if (dictModules.ContainsKey(filePath)) {
		// 	return dictModules[filePath];
		// }

		return this.DefaultModule;
	}

	public override evaluateToString(str: string): string {
		if (typeof this.tokenizer === 'undefined') {
			throw new Error('PrologGlobalInfo.evaluateToString() : this.tokenizer is undefined.');
		} else if (typeof this.parser === 'undefined') {
			throw new Error('PrologGlobalInfo.evaluateToString() : this.parser is undefined.');
		}

		const parseResult = this.parser.parse(this.tokenizer.tokenize(str));
		const expr = parseResult as PrologClause | PrologGoal[];

		this.clearPrintedText();

		const evaluationResultAsString = this.ProcessInput(expr);

		return this.getPrintedText() + evaluationResultAsString;
	}

	private getNextGuid(): string {
		return `guid-${this.guidNumber++}`;
	}
}
