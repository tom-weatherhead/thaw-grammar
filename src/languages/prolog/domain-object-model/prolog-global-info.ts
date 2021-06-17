// tom-weatherhead/thaw-grammar/src/languages/prolog/domain-object-model/prolog-global-info.ts

import { Set } from 'thaw-common-utilities.ts';

import { LanguageSelector } from 'thaw-lexical-analyzer';

import { GlobalInfoBase } from '../../../common/domain-object-model/global-info-base';

import { IPrologExpression } from './iprolog-expression';
import { PrologClause } from './prolog-clause';
import { PrologFunctor } from './prolog-functor';
import { PrologGoal } from './prolog-goal';
import { PrologIntegerLiteral } from './prolog-integer-literal';

import { PrologModule } from './prolog-module';
import { PrologNameExpression } from './prolog-name-expression';
import { PrologPredicate } from './prolog-predicate';
import { PrologSubstitution } from './prolog-substitution';
import { PrologVariable } from './prolog-variable';
import { StringIntKey } from './string-int-key';

export function setToArray<T>(s: Set<T>): T[] {
	const result: T[] = [];

	s.getIterator().forEach((t: T) => {
		result.push(t);
	});

	return result;
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
	//     //public readonly List<PrologClause> ClauseList = new List<PrologClause>();
	//     public readonly LanguageSelector gs;
	//     public readonly ITokenizer tokenizer;
	//     public readonly IParser parser;
	private variableRenameNum = 0;
	//     private bool allMode = false;    // Determines how many solutions we will search for.  false means "first" mode; true means "all" mode.
	//     private readonly StringBuilder sbOutput = new StringBuilder();
	//     private readonly Random random = new Random();
	//     private readonly HashSet<string> LoadedPresets = new HashSet<string>();
	//     private SolutionCollectionMode solutionCollectionMode = SolutionCollectionMode.None;
	//     private IPrologExpression findAll_Expression = null;
	//     private List<IPrologExpression> findAll_ResultList = null;
	//     private List<PrologVariable> caretListVariables = null;
	//     private Dictionary<ExpressionListAsKey, List<IPrologExpression>> dictSolutions = null;
	//     public string PathToDefaultDirectory = null;
	//     private readonly Dictionary<StringIntKey, Func<PrologGoal, PrologSubstitution>> dictBuiltInPredicates
	//         = new Dictionary<StringIntKey, Func<PrologGoal, PrologSubstitution>>();
	// 	// #if SUPPORT_USER_DEFINED_OPERATORS
	//     private readonly List<PrologOperator> Operators = new List<PrologOperator>();
	// 	// #endif
	//     public IInterpreterFileLoader FileLoader = null;
	private readonly DefaultModule = new PrologModule();
	private readonly dictModules = new Map<string, PrologModule>(); // The keys are file paths.

	constructor() {
		super();
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

	public toString(): string {
		return 'PrologGlobalInfo.toString()';
	}

	public get falseValue(): IPrologExpression {
		return new PrologIntegerLiteral(0);
	}

	public get trueValue(): IPrologExpression {
		return new PrologIntegerLiteral(1);
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
		//ClauseList.Clear();
		this.variableRenameNum = 0;
		//       FindFirstSolution();
		//       LoadedPresets.Clear();
		// // #if SUPPORT_USER_DEFINED_OPERATORS
		//       Operators.Clear();
		//       CreateBuiltInOperators();
		// // #endif
		//       DefaultModule.Clear();
		//       dictModules.Clear();
	}

	//     public void FindFirstSolution()
	//     {
	//         allMode = false;
	//     }

	//     public void FindAllSolutions()
	//     {
	//         allMode = true;
	//     }

	//     public string LoadPreset(string presetName)
	//     {

	//         if (LoadedPresets.Contains(presetName))
	//         {
	//             return string.Format("The preset '{0}' has already been loaded.", presetName);
	//         }

	//         switch (presetName)
	//         {
	//             case "<=":

	//                 if (gs == LanguageSelector.Prolog)
	//                 {
	//                     ProcessInputString("(infer (<= X X))");
	//                     ProcessInputString("(infer (<= X Y) from (less X Y))");
	//                 }
	//                 else
	//                 {
	//                     ProcessInputString("'<='(X, Y) :- X =< Y.");
	//                 }

	//                 break;

	//             case "addtoend":

	//                 if (gs == LanguageSelector.Prolog)
	//                 {
	//                     // (addtoend L X M) means that M is the list obtained by adding X to the end of L.
	//                     ProcessInputString("(infer (addtoend nil X (cons X nil)))");
	//                     ProcessInputString("(infer (addtoend (cons Y L) X (cons Y M)) from (addtoend L X M))");
	//                 }
	//                 else
	//                 {
	//                     ProcessInputString("addtoend([], X, [X]).");
	//                     ProcessInputString("addtoend([Y | L], X, [Y | M]) :- addtoend(L, X, M).");
	//                 }

	//                 break;

	//             case "append":
	//             //case "append2": // The preset name "append2" is deprecated.

	//                 if (gs == LanguageSelector.Prolog)
	//                 {
	//                     // (append L M N) means that N is the list obtained by appending M onto the end of L.
	//                     ProcessInputString("(infer (append nil L L))");
	//                     ProcessInputString("(infer (append (cons X L) M (cons X N)) from (append L M N))");
	//                 }
	//                 else
	//                 {
	//                     ProcessInputString("append([], L, L).");
	//                     ProcessInputString("append([X | Y], L, [X | Z]) :- append(Y, L, Z).");
	//                 }

	//                 break;

	//             case "member":
	//             //case "member2": // The preset name "member2" is deprecated.

	//                 if (gs == LanguageSelector.Prolog)
	//                 {
	//                     ProcessInputString("(infer (member X (cons X L)))");
	//                     ProcessInputString("(infer (member X (cons Y M)) from (member X M))");
	//                 }
	//                 else
	//                 {
	//                     ProcessInputString("member(X, [X | _]).");
	//                     ProcessInputString("member(X, [_ | T]) :- member(X, T).");
	//                 }

	//                 break;

	//             case "permutation":
	//                 LoadPreset("append");
	//                 ProcessInputString("permutation([], []).");
	//                 ProcessInputString("permutation(L, [H | T]) :- append(V, [H | U], L), append(V, U, W), permutation(W, T).");
	//                 break;

	//             case "rev": // Reverse a list.
	//                 ProcessInputString("accRev([H | T], A, R):-  accRev(T, [H | A], R).");
	//                 ProcessInputString("accRev([], A, A).");
	//                 ProcessInputString("rev(L, R) :- accRev(L, [], R).");
	//                 break;

	//             case "succ":
	//                 ProcessInputString("intToSucc(0, 0).");
	//                 ProcessInputString("intToSucc(N, succ(L)) :- N > 0, M is N - 1, intToSucc(M, L).");
	//                 ProcessInputString("succToInt(0, 0).");
	//                 ProcessInputString("succToInt(succ(L), N) :- succToInt(L, M), N is M + 1.");
	//                 break;

	//             case "atom_concat":
	//                 LoadPreset("append");
	//                 // We want to use a cut in one of these clauses to avoid repeated results in the case where A1, A2, and A3 are all atomic.
	//                 // We can use a cut in this clause because it can produce at most one result, even in "all" mode:
	//                 ProcessInputString("atom_concat(A1, A2, A3) :- atomic(A1), atomic(A2), atom_chars(A1, L1), atom_chars(A2, L2), append(L1, L2, L3), atom_chars(A3, L3), !.");
	//                 // We don't use a cut in ths clause because it can produce multiple results in "all" mode:
	//                 ProcessInputString("atom_concat(A1, A2, A3) :- atomic(A3), atom_chars(A3, L3), append(L1, L2, L3), atom_chars(A1, L1), atom_chars(A2, L2).");
	//                 break;

	//             case "concat_atom":
	//                 LoadPreset("atom_concat");
	//                 ProcessInputString("concat_atomAcc(Acc, [], Acc).");
	//                 ProcessInputString("concat_atomAcc(Acc, [H | T], Result) :- atom_concat(Acc, H, Acc2), concat_atomAcc(Acc2, T, Result).");
	//                 // We can use a cut in this clause to avoid reporting results using both this clause and the next clause when in "all" mode.
	//                 // This clause is not satisfied when there are variables in the first parameter.
	//                 ProcessInputString("concat_atom([H | T], Result) :- concat_atomAcc(H, T, Result), !.");
	//                 // We don't use a cut in ths clause because it can produce multiple results in "all" mode:
	//                 ProcessInputString("concat_atom([X, Y], Result) :- atom_concat(X, Y, Result).");
	//                 break;

	//             default:
	//                 throw new Exception(string.Format("LoadPreset() : Unknown preset name '{0}'.", presetName));
	//         }

	//         LoadedPresets.Add(presetName);
	//         return string.Format("The preset '{0}' has been successfully loaded.", presetName);
	//     }

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

	public GetNextUniqueVariable(): PrologVariable {
		++this.variableRenameNum;

		return new PrologVariable(`Var${this.variableRenameNum}`);
	}

	private GetVariablesFromGoalList(
		goalListParam: PrologGoal[]
	): Set<PrologVariable> {
		const result = new Set<PrologVariable>();

		// console.log('goalListParam is', typeof goalListParam, goalListParam);

		for (const goal of goalListParam) {
			result.unionInPlace(goal.FindBindingVariables());
		}

		return result;
	}

	public GetListOfBindingVariablesFromGoalList(
		goalListParam: PrologGoal[]
	): PrologVariable[] {
		let result: PrologVariable[] = [];

		for (const goal of goalListParam) {
			result = result.concat(goal.GetListOfBindingVariables());
		}

		return result;
	}

	public static ConvertToFunctorExpression(
		obj: unknown
	): PrologNameExpression<PrologFunctor> {
		let functorExpression: PrologNameExpression<PrologFunctor>;
		const a = obj as PrologNameExpression<PrologFunctor>;
		const b = obj as PrologFunctor;
		const c = obj as PrologVariable;
		const d = obj as string;

		if (typeof a !== 'undefined') {
			functorExpression = a;
		} else if (typeof b !== 'undefined') {
			functorExpression = new PrologNameExpression<PrologFunctor>(
				LanguageSelector.Prolog2,
				b
			);
		} else if (typeof c !== 'undefined') {
			functorExpression = new PrologNameExpression<PrologFunctor>(
				LanguageSelector.Prolog2,
				new PrologFunctor(c.Name)
			);
		} else if (typeof d !== 'undefined') {
			functorExpression = new PrologNameExpression<PrologFunctor>(
				LanguageSelector.Prolog2,
				new PrologFunctor(d)
			);
		} else {
			// functorExpression = undefined;
			throw new Error(
				'ConvertToFunctorExpression() : obj is an unsupported type'
			);
		}

		return functorExpression;
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

	//         foreach (var cl in clausesToRemove)
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

	//     private PrologSubstitution ApplyBuiltInComparisonOperator(
	//         PrologGoal goal,
	//         Func<int, int, bool> intComparisonOperator,
	//         Func<double, double, bool> floatComparisonOperator)
	//     {
	//         var lhsEvalated = goal.ExpressionList[0].EvaluateToNumber();
	//         var rhsEvalated = goal.ExpressionList[1].EvaluateToNumber();

	//         if (lhsEvalated == null || rhsEvalated == null)
	//         {
	//             return null;
	//         }

	//         bool comparisonResult;

	//         if (lhsEvalated is PrologIntegerLiteral && rhsEvalated is PrologIntegerLiteral)
	//         {
	//             comparisonResult = intComparisonOperator(lhsEvalated.ToInteger(), rhsEvalated.ToInteger());
	//         }
	//         else
	//         {
	//             comparisonResult = floatComparisonOperator(lhsEvalated.ToDouble(), rhsEvalated.ToDouble());
	//         }

	//         if (comparisonResult)
	//         {
	//             return new PrologSubstitution();
	//         }

	//         return null;
	//     }

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
	//             var functorName = firstArgAsFunctorExpression.Name.Name;
	//             var functorArity = firstArgAsFunctorExpression.ExpressionList.Count;

	//             if (secondArgAsFunctorExpression != null)
	//             {

	//                 if (secondArgAsFunctorExpression.Name.Name != functorName || secondArgAsFunctorExpression.ExpressionList.Count != 0)
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

	//             if (secondArgAsFunctorExpression != null && secondArgAsFunctorExpression.Name.Name == firstArgAsString &&
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

	public static PrologListToCSharpList(
		expr: IPrologExpression
	): IPrologExpression[] {
		const functorExpression = expr as PrologNameExpression<PrologFunctor>;

		if (typeof functorExpression === 'undefined') {
			// return null;
			throw new Error(
				'PrologListToCSharpList() : functorExpression is undefined'
			);
		} else if (
			functorExpression.Name.Name === '[]' &&
			functorExpression.ExpressionList.length === 0
		) {
			return [];
		} else if (
			functorExpression.Name.Name === '.' &&
			functorExpression.ExpressionList.length === 2
		) {
			const result = this.PrologListToCSharpList(
				functorExpression.ExpressionList[1]
			);

			if (typeof result === 'undefined') {
				// return null;
				throw new Error('PrologListToCSharpList() : case 2');
			}

			result.unshift(functorExpression.ExpressionList[0]);

			return result;
		} else {
			// return null;
			throw new Error('PrologListToCSharpList() : case 3');
		}
	}

	//     // This is used only in Prolog2 (standard Prolog notation):

	//     public static PrologNameExpression<PrologFunctor> CSharpListToPrologList(List<IPrologExpression> exprList, int i = 0)
	//     {

	//         if (i >= exprList.Count)
	//         {
	//             return new PrologNameExpression<PrologFunctor>(LanguageSelector.Prolog2, new PrologFunctor("[]"), null);
	//         }
	//         else
	//         {
	//             return new PrologNameExpression<PrologFunctor>(LanguageSelector.Prolog2, new PrologFunctor("."),
	//                 new List<IPrologExpression>() { exprList[i], CSharpListToPrologList(exprList, i + 1) });
	//         }
	//     }

	public static ExpressionToGoal(
		expr: IPrologExpression
	): PrologGoal | undefined {
		//var fe = expr as PrologNameExpression<PrologFunctor>;
		const fe = this.ConvertToFunctorExpression(expr);

		if (typeof fe === 'undefined') {
			return undefined;
		}

		// return fe.ToGoal();
		return new PrologGoal(
			fe.gs,
			new PrologPredicate(fe.Name.Name),
			fe.ExpressionList
		);
	}

	public static CSharpListToGoalList(
		exprList: IPrologExpression[]
	): PrologGoal[] | undefined {
		const goalList: PrologGoal[] = [];

		for (const e of exprList) {
			const goal = this.ExpressionToGoal(e);

			if (typeof goal === 'undefined') {
				//throw new Exception(string.Format("CSharpListToGoalList() : '{0}' is not a functor expression.", e));
				return undefined;
			}

			goalList.push(goal);
		}

		return goalList;
	}

	//     public static List<PrologGoal> PrologListToGoalList(IPrologExpression expr)
	//     {
	//         return CSharpListToGoalList(PrologListToCSharpList(expr));
	//     }

	//     public static PrologNameExpression<PrologFunctor> CreateClauseAsFunctorExpression(IPrologExpression lhs, IPrologExpression rhs)
	//     {
	//         return new PrologNameExpression<PrologFunctor>(LanguageSelector.Prolog2, new PrologFunctor("clause"),
	//             new List<IPrologExpression>() { lhs, rhs });
	//     }

	public static CreateClause(expr: IPrologExpression): PrologClause {
		const fe = expr as PrologNameExpression<PrologFunctor>;

		if (
			typeof fe === 'undefined' ||
			(fe.Name.Name !== 'clause' && fe.Name.Name !== ':-') ||
			fe.ExpressionList.length !== 2
		) {
			//Console.WriteLine("CreateClause 1: null");
			// return undefined;
			throw new Error('PrologGlobalInfo.CreateClause() : case 1');
		}

		const lhs = this.ExpressionToGoal(fe.ExpressionList[0]);
		// #if SUPPORT_USER_DEFINED_OPERATORS
		const rhs = this.CSharpListToGoalList(
			this.ExpressionToCSharpList(fe.ExpressionList[1])
		);
		// #else
		// List<PrologGoal> rhs;

		// if (fe.Name.Name == "clause")
		// {
		//     rhs = PrologListToGoalList(fe.ExpressionList[1]);
		// }
		// else
		// {
		//     rhs = CSharpListToGoalList(CommaSeparatedListToCSharpList(fe.ExpressionList[1]));

		//     if (rhs == null)
		//     {
		//         Console.WriteLine("CreateClause 3: rhs is null");
		//     }
		// }
		// #endif

		if (typeof lhs === 'undefined' || typeof rhs === 'undefined') {
			//Console.WriteLine("CreateClause 2: null");
			// return undefined;
			throw new Error('PrologGlobalInfo.CreateClause() : case 2');
		}

		return new PrologClause(lhs, rhs);
	}

	//     private PrologClause CreateClause_General(IPrologExpression expr)
	//     {
	//         var clause = CreateClause(expr);

	//         if (clause != null)
	//         {
	//             return clause;
	//         }

	//         var fe = ConvertToFunctorExpression(expr);

	//         if (fe == null)
	//         {
	//             return null;
	//         }

	//         return new PrologClause(fe.ToGoal(), new List<PrologGoal>());
	//     }

	//     public static string PrologCodeListToCSharpString(IPrologExpression expr)
	//     {
	//         var csharpList = PrologListToCSharpList(expr);

	//         if (csharpList == null)
	//         {
	//             return null;
	//         }

	//         var sb = new StringBuilder();

	//         foreach (var value in csharpList)
	//         {
	//             var intValue = value as PrologIntegerLiteral;

	//             if (intValue == null || intValue.Value < 0)
	//             {
	//                 return null;
	//             }

	//             sb.Append((char)intValue.Value);
	//         }

	//         return sb.ToString();
	//     }

	//     public static PrologNameExpression<PrologFunctor> CSharpStringToPrologCodeList(string str)
	//     {
	//         var listOfIntCodes = new List<IPrologExpression>();

	//         for (var i = 0; i < str.Length; ++i)
	//         {
	//             listOfIntCodes.Add(new PrologIntegerLiteral((int)str[i]));
	//         }

	//         return PrologGlobalInfo.CSharpListToPrologList(listOfIntCodes);
	//     }

	//     public static string PrologCharListToCSharpString(IPrologExpression expr)
	//     {
	//         var csharpList = PrologListToCSharpList(expr);

	//         if (csharpList == null)
	//         {
	//             return null;
	//         }

	//         var sb = new StringBuilder();

	//         foreach (var value in csharpList)
	//         {
	//             var functorExpr = value as PrologNameExpression<PrologFunctor>;

	//             if (functorExpr == null || functorExpr.Name.Name.Length != 1 || functorExpr.ExpressionList.Count != 0)
	//             {
	//                 return null;
	//             }

	//             sb.Append(functorExpr.Name.Name);
	//         }

	//         return sb.ToString();
	//     }

	//     public static PrologNameExpression<PrologFunctor> CSharpStringToPrologCharList(string str)
	//     {
	//         var listOfChars = new List<IPrologExpression>();

	//         for (var i = 0; i < str.Length; ++i)
	//         {
	//             listOfChars.Add(new PrologNameExpression<PrologFunctor>(LanguageSelector.Prolog2, new PrologFunctor(str.Substring(i, 1)), null));
	//         }

	//         return PrologGlobalInfo.CSharpListToPrologList(listOfChars);
	//     }

	//     private PrologSubstitution AtomicCodes(PrologGoal goal)
	//     {
	//         var goalName = goal.Name.Name;
	//         var firstArg = goal.ExpressionList[0];
	//         var firstArgAsFunctorExpression = firstArg as PrologNameExpression<PrologFunctor>;
	//         var firstArgIsAtom = firstArgAsFunctorExpression != null && firstArgAsFunctorExpression.ExpressionList.Count == 0;
	//         var secondArg = goal.ExpressionList[1];
	//         var secondArgAsCSharpString = PrologCodeListToCSharpString(secondArg);

	//         if (secondArgAsCSharpString == string.Empty)
	//         {
	//             return null;
	//         }
	//         else if (secondArgAsCSharpString != null)
	//         {
	//             var canGenerateNumber = goalName == "number_codes" || goalName == "name";
	//             var canGenerateAtom = goalName == "atom_codes" || goalName == "name";
	//             int secondArgAsCSharpInt;
	//             double secondArgAsCSharpDouble;

	//             if (canGenerateNumber && int.TryParse(secondArgAsCSharpString, out secondArgAsCSharpInt))
	//             {
	//                 // Use Unify() because firstArgAsVariable could be a non-binding variable.
	//                 return firstArg.Unify(new PrologIntegerLiteral(secondArgAsCSharpInt));
	//             }
	//             else if (canGenerateNumber && double.TryParse(secondArgAsCSharpString, out secondArgAsCSharpDouble))
	//             {
	//                 // Use Unify() because firstArgAsVariable could be a non-binding variable.
	//                 return firstArg.Unify(new PrologFloatLiteral(secondArgAsCSharpDouble));
	//             }
	//             else if (canGenerateAtom)
	//             {
	//                 // Use Unify() because firstArgAsVariable could be a non-binding variable.
	//                 return firstArg.Unify(CreateAtom(secondArgAsCSharpString));
	//             }
	//         }
	//         else if (firstArgIsAtom || (firstArg is IPrologNumber))
	//         {
	//             var firstArgAsCodesList = CSharpStringToPrologCodeList(firstArg.ToString());

	//             return secondArg.Unify(firstArgAsCodesList);
	//         }

	//         return null;
	//     }

	//     private PrologSubstitution AtomicChars(PrologGoal goal)
	//     {
	//         var goalName = goal.Name.Name;
	//         var firstArg = goal.ExpressionList[0];
	//         var firstArgAsFunctorExpression = firstArg as PrologNameExpression<PrologFunctor>;
	//         var firstArgIsAtom = firstArgAsFunctorExpression != null && firstArgAsFunctorExpression.ExpressionList.Count == 0;
	//         var secondArg = goal.ExpressionList[1];
	//         var secondArgAsCSharpString = PrologCharListToCSharpString(secondArg);

	//         if (secondArgAsCSharpString == string.Empty)
	//         {
	//             return null;
	//         }
	//         else if (secondArgAsCSharpString != null)
	//         {
	//             var canGenerateNumber = goalName == "number_chars";
	//             var canGenerateAtom = goalName == "atom_chars";
	//             int secondArgAsCSharpInt;
	//             double secondArgAsCSharpDouble;

	//             if (canGenerateNumber && int.TryParse(secondArgAsCSharpString, out secondArgAsCSharpInt))
	//             {
	//                 // Use Unify() because firstArgAsVariable could be a non-binding variable.
	//                 return firstArg.Unify(new PrologIntegerLiteral(secondArgAsCSharpInt));
	//             }
	//             else if (canGenerateNumber && double.TryParse(secondArgAsCSharpString, out secondArgAsCSharpDouble))
	//             {
	//                 // Use Unify() because firstArgAsVariable could be a non-binding variable.
	//                 return firstArg.Unify(new PrologFloatLiteral(secondArgAsCSharpDouble));
	//             }
	//             else if (canGenerateAtom)
	//             {
	//                 // Use Unify() because firstArgAsVariable could be a non-binding variable.
	//                 return firstArg.Unify(CreateAtom(secondArgAsCSharpString));
	//             }
	//         }
	//         else if (firstArgIsAtom || (firstArg is IPrologNumber))
	//         {
	//             var firstArgAsCodesList = CSharpStringToPrologCharList(firstArg.ToString());

	//             return secondArg.Unify(firstArgAsCodesList);
	//         }

	//         return null;
	//     }

	//     private PrologSubstitution CharCode(PrologGoal goal)
	//     {
	//         var goalName = goal.Name.Name;
	//         var firstArg = goal.ExpressionList[0];
	//         var firstArgAsFunctorExpression = firstArg as PrologNameExpression<PrologFunctor>;
	//         var firstArgIsChar = firstArgAsFunctorExpression != null &&
	//             firstArgAsFunctorExpression.ExpressionList.Count == 0 &&
	//             firstArgAsFunctorExpression.Name.Name.Length == 1;
	//         var firstArgAsVariable = firstArg as PrologVariable;
	//         var secondArg = goal.ExpressionList[1];
	//         var secondArgAsInteger = secondArg as PrologIntegerLiteral;

	//         if (firstArgIsChar)
	//         {
	//             var firstArgCharCode = new PrologIntegerLiteral((int)firstArgAsFunctorExpression.Name.Name[0]);

	//             return firstArgCharCode.Unify(secondArg);
	//         }
	//         else if (firstArgAsVariable != null && secondArgAsInteger != null && secondArgAsInteger.Value >= 0)
	//         {
	//             var c = (char)secondArgAsInteger.Value;

	//             // Use Unify() because firstArgAsVariable could be a non-binding variable.
	//             return firstArgAsVariable.Unify(CreateAtom(c.ToString()));
	//         }

	//         return null;
	//     }

	//     private PrologSubstitution AtomNumber(PrologGoal goal)
	//     {
	//         var firstArg = goal.ExpressionList[0];
	//         var secondArg = goal.ExpressionList[1];
	//         var firstArgAsFunctorExpression = firstArg as PrologNameExpression<PrologFunctor>;
	//         var firstArgIsAtom = firstArgAsFunctorExpression != null && firstArgAsFunctorExpression.ExpressionList.Count == 0;
	//         var firstArgAsVariable = firstArg as PrologVariable;
	//         var secondArgAsNumber = secondArg as IPrologNumber;

	//         if (firstArgIsAtom)
	//         {
	//             var firstArgAsCSharpString = firstArg.ToString();
	//             int firstArgAsCSharpInt;
	//             double firstArgAsCSharpDouble;

	//             if (int.TryParse(firstArgAsCSharpString, out firstArgAsCSharpInt))
	//             {
	//                 // Use Unify() because secondArg could be a non-binding variable.
	//                 return secondArg.Unify(new PrologIntegerLiteral(firstArgAsCSharpInt));
	//             }
	//             else if (double.TryParse(firstArgAsCSharpString, out firstArgAsCSharpDouble))
	//             {
	//                 // Use Unify() because secondArg could be a non-binding variable.
	//                 return secondArg.Unify(new PrologFloatLiteral(firstArgAsCSharpDouble));
	//             }
	//         }
	//         else if (firstArgAsVariable != null && secondArgAsNumber != null)
	//         {
	//             // Use Unify() because firstArgAsVariable could be a non-binding variable.
	//             return firstArgAsVariable.Unify(CreateAtom(secondArgAsNumber.ToString()));
	//         }

	//         return null;
	//     }

	//     private PrologSubstitution AtomLength(PrologGoal goal)
	//     {
	//         var firstArg = goal.ExpressionList[0];
	//         var secondArg = goal.ExpressionList[1];
	//         // firstArg can be an atom or a string.
	//         var firstArgAsFunctorExpression = firstArg as PrologNameExpression<PrologFunctor>;
	//         var firstArgIsAtom = firstArgAsFunctorExpression != null && firstArgAsFunctorExpression.ExpressionList.Count == 0;
	//         var firstArgAsCSharpString = PrologCodeListToCSharpString(firstArg);

	//         if (firstArgAsCSharpString != null)
	//         {
	//             // firstArg may be the empty list, in which case we want to yield 0 (the length of "") rather than 2 (the length of "[]").
	//         }
	//         else if (firstArgIsAtom)
	//         {
	//             firstArgAsCSharpString = firstArgAsFunctorExpression.Name.Name;
	//         }
	//         else
	//         {
	//             return null;
	//         }

	//         // firstArgAsCSharpString may be string.Empty
	//         return secondArg.Unify(new PrologIntegerLiteral(firstArgAsCSharpString.Length));
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

	//     private void AutomaticPrint(List<PrologVariable> variablesInQuery, PrologSubstitution substitution)
	//     {

	//         if (variablesInQuery == null)
	//         {
	//             return;
	//         }

	//         var resultList = new List<string>();

	//         foreach (var v in variablesInQuery)
	//         {
	//             var value = v.ApplySubstitution(substitution);

	//             if (!value.Equals(v)) // Avoid printing identity results such as "X = X".
	//             {
	//                 resultList.Add(string.Format("{0} = {1}", v, value));
	//             }
	//         }

	//         if (resultList.Count == 0)
	//         {
	//             return;
	//         }

	//         if (sbOutput.Length > 0)
	//         {
	//             sbOutput.AppendLine();
	//         }

	//         sbOutput.Append(string.Join(", ", resultList));

	//         if (allMode)
	//         {
	//             sbOutput.Append(";");
	//         }
	//     }

	//     private PrologSubstitution Is2(PrologGoal goal)
	//     {
	//         var rhsEvaluated = goal.ExpressionList[1].EvaluateToNumber();

	//         if (rhsEvaluated == null)
	//         {
	//             return null;
	//         }

	//         if (goal.ExpressionList[0] is IPrologNumber)
	//         {

	//             if (goal.ExpressionList[0].Equals(rhsEvaluated)) // Remember that the int 1 does not equal the double 1.0 according to this code.
	//             {
	//                 return new PrologSubstitution();
	//             }
	//         }
	//         else if (goal.ExpressionList[0] is PrologVariable)
	//         {
	//             //var newSubstitution = new PrologSubstitution((PrologVariable)goal.ExpressionList[0], rhsEvaluated);
	//             // Use Unify() because goal.ExpressionList[0] could be a non-binding variable.
	//             return goal.ExpressionList[0].Unify(rhsEvaluated);
	//         }

	//         return null;
	//     }

	//     private PrologSubstitution Unifiable2(PrologGoal goal)
	//     {
	//         return goal.ExpressionList[0].Unify(goal.ExpressionList[1]);
	//     }

	//     private PrologSubstitution NotUnifiable2(PrologGoal goal)
	//     {

	//         if (goal.ExpressionList[0].Unify(goal.ExpressionList[1]) != null)
	//         {
	//             return null;
	//         }

	//         return new PrologSubstitution();
	//     }

	//     private PrologSubstitution Equals2(PrologGoal goal)
	//     {

	//         if (goal.ExpressionList[0].Equals(goal.ExpressionList[1]))
	//         {
	//             return new PrologSubstitution();
	//         }

	//         return null;
	//     }

	//     private PrologSubstitution NotEquals2(PrologGoal goal)
	//     {

	//         if (goal.ExpressionList[0].Equals(goal.ExpressionList[1]))
	//         {
	//             return null;
	//         }

	//         return new PrologSubstitution();
	//     }

	//     private PrologSubstitution Open3(PrologGoal goal)
	//     {
	//         var firstArgAsFunctorExpression = goal.ExpressionList[0] as PrologNameExpression<PrologFunctor>;
	//         var secondArgAsFunctorExpression = goal.ExpressionList[1] as PrologNameExpression<PrologFunctor>;

	//         if (firstArgAsFunctorExpression == null || secondArgAsFunctorExpression == null ||
	//             firstArgAsFunctorExpression.ExpressionList.Count > 0 || secondArgAsFunctorExpression.ExpressionList.Count > 0)
	//         {
	//             //sbOutput.AppendLine("open error 1.");
	//             return null;
	//         }

	//         switch (secondArgAsFunctorExpression.Name.Name)
	//         {
	//             case "read":
	//                 var fileReader = new PrologFileReader(firstArgAsFunctorExpression.Name.Name, this);

	//                 if (fileReader.IsNull)
	//                 {
	//                     //sbOutput.AppendLine("open error 2.");
	//                     break;
	//                 }

	//                 return goal.ExpressionList[2].Unify(fileReader);

	//             case "write":
	//             case "append":
	//                 var fileWriter = new PrologFileWriter(firstArgAsFunctorExpression.Name.Name,
	//                     secondArgAsFunctorExpression.Name.Name == "append", this);

	//                 if (fileWriter.IsNull)
	//                 {
	//                     break;
	//                 }

	//                 return goal.ExpressionList[2].Unify(fileWriter);

	//             default:
	//                 break;
	//         }

	//         return null;
	//     }

	//     private PrologSubstitution Close1(PrologGoal goal)
	//     {
	//         var firstArgAsFileReader = goal.ExpressionList[0] as PrologFileReader;
	//         var firstArgAsFileWriter = goal.ExpressionList[0] as PrologFileWriter;

	//         if (firstArgAsFileReader != null)
	//         {
	//             firstArgAsFileReader.Close();
	//             return new PrologSubstitution();
	//         }
	//         else if (firstArgAsFileWriter != null)
	//         {
	//             firstArgAsFileWriter.Close();
	//             return new PrologSubstitution();
	//         }
	//         else
	//         {
	//             return null;
	//         }
	//     }

	//     private PrologSubstitution AtEndOfStream1(PrologGoal goal)
	//     {
	//         var firstArgAsFileReader = goal.ExpressionList[0] as PrologFileReader;

	//         if (firstArgAsFileReader != null && firstArgAsFileReader.IsEOF)
	//         {
	//             return new PrologSubstitution();
	//         }
	//         else
	//         {
	//             return null;
	//         }
	//     }

	//     private PrologSubstitution Read2(PrologGoal goal)
	//     {
	//         var firstArgAsFileReader = goal.ExpressionList[0] as PrologFileReader;

	//         if (firstArgAsFileReader == null)
	//         {
	//             //sbOutput.AppendLine("read error 1.");
	//             return null;
	//         }

	//         var readAtom = firstArgAsFileReader.Read( /* sbOutput */ );

	//         if (readAtom == null)
	//         {
	//             //sbOutput.AppendLine("read error 2.");
	//             return null;
	//         }

	//         return goal.ExpressionList[1].Unify(readAtom);
	//     }

	//     private PrologSubstitution GetCode2(PrologGoal goal)
	//     {
	//         var firstArgAsFileReader = goal.ExpressionList[0] as PrologFileReader;

	//         if (firstArgAsFileReader == null)
	//         {
	//             return null;
	//         }

	//         var readCharacterCode = new PrologIntegerLiteral(firstArgAsFileReader.GetCode());

	//         return goal.ExpressionList[1].Unify(readCharacterCode);
	//     }

	//     private PrologSubstitution Write1(PrologGoal goal)
	//     {

	//         if (goal.ExpressionList[0] is PrologVariable)
	//         {
	//             return null;
	//         }
	//         else
	//         {
	//             sbOutput.Append(goal.ExpressionList[0].ToString());
	//             return new PrologSubstitution();
	//         }
	//     }

	//     private PrologSubstitution Write2(PrologGoal goal)
	//     {
	//         var firstArgAsFileWriter = goal.ExpressionList[0] as PrologFileWriter;

	//         if (firstArgAsFileWriter != null && !(goal.ExpressionList[1] is PrologVariable) &&
	//             firstArgAsFileWriter.Write(goal.ExpressionList[1]))
	//         {
	//             return new PrologSubstitution();
	//         }
	//         else
	//         {
	//             return null;
	//         }
	//     }

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

	//     private PrologSubstitution Integer1(PrologGoal goal)
	//     {

	//         if (goal.ExpressionList[0] is PrologIntegerLiteral)
	//         {
	//             return new PrologSubstitution();
	//         }
	//         else
	//         {
	//             return null;
	//         }
	//     }

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

	//     private PrologSubstitution Var1(PrologGoal goal)
	//     {

	//         if (goal.ExpressionList[0] is PrologVariable)
	//         {
	//             return new PrologSubstitution();
	//         }
	//         else
	//         {
	//             return null;
	//         }
	//     }

	//     private PrologSubstitution NonVar1(PrologGoal goal)
	//     {

	//         if (goal.ExpressionList[0] is PrologVariable)
	//         {
	//             return null;
	//         }
	//         else
	//         {
	//             return new PrologSubstitution();
	//         }
	//     }

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

	//     // "listing" and "listing(targetName)"; see http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlse4

	//     private PrologSubstitution Listing0(PrologGoal goal)
	//     {

	//         foreach (var moduleName in dictModules.Keys)
	//         {
	//             sbOutput.AppendLine(string.Format("Module '{0}' :", moduleName));
	//             dictModules[moduleName].ClauseList.ForEach(clause => sbOutput.AppendLine(clause.ToString()));
	//             sbOutput.AppendLine();
	//         }

	//         sbOutput.AppendLine("Default module:");
	//         DefaultModule.ClauseList.ForEach(clause => sbOutput.AppendLine(clause.ToString()));
	//         return new PrologSubstitution();
	//     }

	//     private PrologSubstitution Listing1(PrologGoal goal)
	//     {
	//         string targetName = null;
	//         var functorExpression = goal.ExpressionList[0] as PrologNameExpression<PrologFunctor>;
	//         var variable = goal.ExpressionList[0] as PrologVariable;

	//         if (functorExpression != null && functorExpression.ExpressionList.Count == 0)
	//         {
	//             targetName = functorExpression.Name.Name; // The name probably does not begin with a capital letter or an underscore, unless it was single quoted.
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

	//         foreach (var moduleName in dictModules.Keys)
	//         {
	//             sbOutput.AppendLine(string.Format("Module '{0}' :", moduleName));
	//             dictModules[moduleName].ClauseList
	//                 .Where(clause => clause.Lhs.Name.Name == targetName)
	//                 .ToList()
	//                 .ForEach(clause => sbOutput.AppendLine(clause.ToString()));
	//             sbOutput.AppendLine();
	//         }

	//         sbOutput.AppendLine("Default module:");
	//         DefaultModule.ClauseList
	//             .Where(clause => clause.Lhs.Name.Name == targetName)
	//             .ToList()
	//             .ForEach(clause => sbOutput.AppendLine(clause.ToString()));
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

	//     private PrologSubstitution KaminPlus3(PrologGoal goal)
	//     {
	//         return KaminApplyBuiltInArithmeticOperator(goal, (x, y) => x + y);
	//     }

	//     private PrologSubstitution KaminMinus3(PrologGoal goal)
	//     {
	//         return KaminApplyBuiltInArithmeticOperator(goal, (x, y) => x - y);
	//     }

	//     private PrologSubstitution LessThan2(PrologGoal goal)
	//     {
	//         return ApplyBuiltInComparisonOperator(goal, (x, y) => x < y, (x, y) => x < y);
	//     }

	//     private PrologSubstitution GreaterThan2(PrologGoal goal)
	//     {
	//         return ApplyBuiltInComparisonOperator(goal, (x, y) => x > y, (x, y) => x > y);
	//     }

	//     private PrologSubstitution EqualOrLessThan2(PrologGoal goal)
	//     {
	//         return ApplyBuiltInComparisonOperator(goal, (x, y) => x <= y, (x, y) => x <= y);
	//     }

	//     private PrologSubstitution GreaterThanOrEqual2(PrologGoal goal)
	//     {
	//         return ApplyBuiltInComparisonOperator(goal, (x, y) => x >= y, (x, y) => x >= y);
	//     }

	//     private PrologSubstitution ArithmeticEqual2(PrologGoal goal)
	//     {
	//         return ApplyBuiltInComparisonOperator(goal, (x, y) => x == y, (x, y) => x == y);
	//     }

	//     private PrologSubstitution ArithmeticNotEqual2(PrologGoal goal)
	//     {
	//         return ApplyBuiltInComparisonOperator(goal, (x, y) => x != y, (x, y) => x != y);
	//     }

	// 	// #if SUPPORT_USER_DEFINED_OPERATORS
	//     private PrologGoal ParseCaretList(IPrologExpression expr, List<PrologVariable> variablesToExcludeFromKey)
	//     {
	//         var fe = expr as PrologNameExpression<PrologFunctor>;

	//         if (fe != null && fe.Name.Name == "^" && fe.ExpressionList.Count == 2)
	//         {
	//             var v = fe.ExpressionList[0] as PrologVariable;

	//             if (v == null)
	//             {
	//                 //throw new Exception(string.Format("ParseCaretList() : '{0}' is not a variable.", fe.ExpressionList[0]));
	//                 return null;
	//             }

	//             variablesToExcludeFromKey.Add(v);
	//             return ParseCaretList(fe.ExpressionList[1], variablesToExcludeFromKey);
	//         }

	//         return ExpressionToGoal(expr);
	//     }
	// 	// #endif

	//     private PrologSubstitution BagOfSetOf3(
	//         PrologGoal goal,
	//         List<PrologGoal> goalList,
	//         List<CutDetector> cutDetectorList,
	//         int goalNum,
	//         PrologSubstitution oldSubstitution,
	//         HashSet<PrologVariable> parentVariablesToAvoid,
	//         List<PrologVariable> variablesInQuery,
	//         List<PrologModule> listOfCurrentModules)
	//     {
	//         var isSetOf = goal.Name.Name == "setof";
	// 		// #if SUPPORT_USER_DEFINED_OPERATORS
	//         var variablesToExcludeFromKey = new List<PrologVariable>();
	//         var tempGoal = ParseCaretList(goal.ExpressionList[1], variablesToExcludeFromKey);

	//         if (tempGoal == null)
	//         {
	//             return null;
	//         }
	// 		// #else
	// 		// // The second argument may be a PrologNameExpression<PrologFunctor> or a CaretList.
	// 		// var secondArgAsFunctorExpression = goal.ExpressionList[1] as PrologNameExpression<PrologFunctor>;
	// 		// var secondArgAsCaretList = goal.ExpressionList[1] as CaretList;
	// 		// List<PrologVariable> variablesToExcludeFromKey;
	// 		// PrologGoal tempGoal;

	// 		// if (secondArgAsFunctorExpression != null)
	// 		// {
	// 		//     variablesToExcludeFromKey = new List<PrologVariable>();
	// 		//     tempGoal = secondArgAsFunctorExpression.ToGoal();
	// 		// }
	// 		// else if (secondArgAsCaretList != null)
	// 		// {
	// 		//     variablesToExcludeFromKey = secondArgAsCaretList.VariableList;
	// 		//     tempGoal = secondArgAsCaretList.FunctorExpression.ToGoal();
	// 		// }
	// 		// else
	// 		// {
	// 		//     return null;
	// 		// }
	// 		// #endif

	//         var tempGoalList = new List<PrologGoal>() { tempGoal };
	//         // Get the list of variables in the temp goal, and from that list remove all variables in the caret list (Var1 ^ Var2 ^ ... ^ goal).
	//         // This resulting variable list will be used, after substitutions, as the keys for the dictionary below.
	//         var tempCutDetectorList = new List<CutDetector>() { cutDetectorList[goalNum] };
	//         var tempListOfCurrentModules = new List<PrologModule>() { listOfCurrentModules[goalNum] };
	//         var cachedSolutionCollectionMode = solutionCollectionMode;
	//         var cachedFindAll_Expression = findAll_Expression;
	//         var cachedCaretListVariables = caretListVariables;
	//         var cachedDictSolutions = dictSolutions;
	//         var cachedAllMode = allMode;
	//         // The results will be stored in a dictionary, where the key type is a hashable wrapper for List<IPrologExpression>,
	//         // and the value type is List<IPrologExpression>.
	//         //PrologSubstitution unifier = null;
	//         List<ExpressionListAsKey> resultKeys;
	//         List<List<IPrologExpression>> resultValues;
	//         var findAll_Expression_asVariable = goal.ExpressionList[0] as PrologVariable;

	//         solutionCollectionMode = SolutionCollectionMode.BagOfOrSetOf;
	//         findAll_Expression = goal.ExpressionList[0];
	//         // caretListVariables is actually the binding variables in tempGoal that do not occur in variablesToExcludeFromKey.
	//         // Perhaps a better name would be solutionDictKeyVariables.
	//         caretListVariables = tempGoal.GetListOfBindingVariables().Where(v => !variablesToExcludeFromKey.Contains(v)).ToList();

	//         if (findAll_Expression_asVariable != null)
	//         {
	//             caretListVariables = caretListVariables.Where(v => !v.Equals(findAll_Expression_asVariable)).ToList();
	//         }

	//         dictSolutions = new Dictionary<ExpressionListAsKey, List<IPrologExpression>>();
	//         allMode = true;

	//         var solutionDictKeyVariables = caretListVariables;

	//         try
	//         {
	//             ProveGoalList(tempGoalList, tempCutDetectorList, 0, new PrologSubstitution(),
	//                 parentVariablesToAvoid, null, tempListOfCurrentModules);
	//             // Use the dictionary to create a sorted List<> of keys from the dictionary,
	//             // along with a corresponding list of values.
	//             resultKeys = new List<ExpressionListAsKey>(dictSolutions.Keys);
	//             resultKeys.Sort();
	//             resultValues = resultKeys.Select(k => dictSolutions[k]).ToList();

	//             if (isSetOf)
	//             {
	//                 // Process each list of results to remove duplicates and to sort.
	//                 var newResultValues = new List<List<IPrologExpression>>();
	//                 var comparer = new SetOfComparer();

	//                 foreach (var rv in resultValues)
	//                 {
	//                     var new_rv = new List<IPrologExpression>();

	//                     new_rv.AddRangeUnique(rv);
	//                     new_rv.Sort(comparer);
	//                     newResultValues.Add(new_rv);
	//                 }

	//                 resultValues = newResultValues;
	//             }
	//         }
	//         finally
	//         {
	//             solutionCollectionMode = cachedSolutionCollectionMode;
	//             findAll_Expression = cachedFindAll_Expression;
	//             caretListVariables = cachedCaretListVariables;
	//             dictSolutions = cachedDictSolutions;
	//             allMode = cachedAllMode;
	//         }

	//         // Foreach key in the sorted list of keys:
	//         // - Create a substitution that unifies each part of the key with the corresponding variable from above,
	//         //   and compose it with the unification of goal.ExpressionList[2] and the value that corresponds to the key (from the dictionary).
	//         // - Compose this substitution with oldSubstitution and call ProveGoalList().  If non-null is returned, return it.
	//         // End foreach.
	//         // return null.

	//         for (var i = 0; i < resultKeys.Count; ++i)
	//         {
	//             var key = resultKeys[i];
	//             var value = resultValues[i];
	//             var unifier = new PrologSubstitution();

	//             for (var j = 0; j < solutionDictKeyVariables.Count; ++j)
	//             {
	//                 var expr1 = solutionDictKeyVariables[j].ApplySubstitution(unifier);
	//                 var expr2 = key.ExpressionList[j].ApplySubstitution(unifier);
	//                 var subst = expr1.Unify(expr2);

	//                 if (subst == null)
	//                 {
	//                     unifier = null;
	//                     break;
	//                 }

	//                 unifier = unifier.Compose(subst);
	//             }

	//             if (unifier == null)
	//             {
	//                 continue;
	//             }

	//             var expr3 = goal.ExpressionList[2].ApplySubstitution(unifier);
	//             var expr4 = CSharpListToPrologList(value).ApplySubstitution(unifier);
	//             var subst2 = expr3.Unify(expr4);

	//             if (subst2 == null)
	//             {
	//                 continue;
	//             }

	//             unifier = unifier.Compose(subst2);

	//             var result = ProveGoalList(goalList, cutDetectorList, goalNum + 1, oldSubstitution.Compose(unifier), parentVariablesToAvoid,
	//                 variablesInQuery, listOfCurrentModules);

	//             if (result != null)
	//             {
	//                 return result;
	//             }
	//         }

	//         return null;
	//     }

	private ProveGoalList(
		goalList: PrologGoal[],
		// List<CutDetector> cutDetectorList,
		goalNum: number,
		oldSubstitution: PrologSubstitution,
		parentVariablesToAvoid: Set<PrologVariable>,
		variablesInQuery: PrologVariable[], // Print these variables and their values automatically upon success if there is no print() goal at the end
		listOfCurrentModules: PrologModule[]
	): PrologSubstitution | undefined {
		if (goalNum >= goalList.length) {
			// The goal list has been satisfied.

			// **** Begin automatic printing ****
			// const lastGoal =
			// 	goalList.length > 0 ? goalList[goalList.length - 1] : undefined;

			// if (
			// 	typeof lastGoal !== 'undefined' &&
			// 	lastGoal.Name.Name !== 'print'
			// ) {
			// 	// Don't do automatic printing if the last goal was a print() goal.
			// 	AutomaticPrint(variablesInQuery, oldSubstitution);
			// }

			// **** End automatic printing ****

			// **** Begin findall/3, bagof/3, setof/3 support ****

			// switch (solutionCollectionMode)
			// {
			// case SolutionCollectionMode.FindAll:

			// if (findAll_Expression != null && findAll_ResultList != null)
			// {
			// findAll_ResultList.Add(findAll_Expression.ApplySubstitution(oldSubstitution));
			// }

			// break;

			// case SolutionCollectionMode.BagOfOrSetOf:

			// if (findAll_Expression != null && caretListVariables != null && dictSolutions != null)
			// {
			// var key = new ExpressionListAsKey(caretListVariables.Select(v => v.ApplySubstitution(oldSubstitution)).ToList());
			// var result = findAll_Expression.ApplySubstitution(oldSubstitution);

			// if (dictSolutions.ContainsKey(key))
			// {
			// dictSolutions[key].Add(result);
			// }
			// else
			// {
			// dictSolutions[key] = new List<IPrologExpression>() { result };
			// }
			// }

			// break;

			// default:
			// break;
			// }

			// **** End findall/3, bagof/3, setof/3 support ****

			// To continue searching for other solutions (i.e. if we are operating in "all" mode rather than "first" mode), return null.

			// if (allMode) {
			// 	return undefined;
			// }

			return oldSubstitution;
		}

		// #if SUBSTITUTION_KEY_COUNT_LIMIT
		if (oldSubstitution.SubstitutionList.size > 10) {
			console.log(
				'**** Aborting because the substitution is too long. ****'
			);

			return undefined;
		}
		// #endif

		const unsubstitutedGoal = goalList[goalNum];
		const currentModule = listOfCurrentModules[goalNum];
		const nextGoalNum = goalNum + 1;

		// #if CONSOLE_WRITELINE
		// console.log(
		// 	`ProveGoal: unsubstitutedGoal = ${unsubstitutedGoal}; subst = ${oldSubstitution}`
		// );
		// #endif

		// if (unsubstitutedGoal.IsCut) {
		// 	// The "cut" goal always succeeds.
		// 	// #if CONSOLE_WRITELINE
		// 	console.log('ProveGoal: Detected a cut.');
		// 	// #endif

		// 	// 2014/03/07
		// 	var cutDetector = cutDetectorList[goalNum];
		// 	var cutSubstitution = ProveGoalList(goalList, cutDetectorList, nextGoalNum, oldSubstitution, parentVariablesToAvoid,
		// 	variablesInQuery, listOfCurrentModules);

		// 	if (typeof cutSubstitution === 'undefined' && cutDetector !== null) {
		// 		// We may not backtrack through a cut.
		// 		throw new CutBacktrackException(cutDetector.Guid);
		// 	}

		// 	return cutSubstitution;
		// }

		const goal = unsubstitutedGoal.ApplySubstitution(
			oldSubstitution
		) as PrologGoal;

		// #if CONSOLE_WRITELINE
		// Console.WriteLine("ProveGoal: goal after substitution = {0}", goal);
		// #endif

		// const numArgsInGoal = goal.ExpressionList.length;
		// const functionKey = new StringIntKey(goal.Name.Name, numArgsInGoal);

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

		// switch (goal.Name.Name)
		// {
		// case "print": // This can take any number of arguments.
		// Print(unsubstitutedGoal, goal);
		// return ProveGoalList(goalList, cutDetectorList, nextGoalNum, oldSubstitution, parentVariablesToAvoid, variablesInQuery,
		// listOfCurrentModules);

		// default:
		// break;
		// }
		// }

		// switch (goal.Name.Name) {
		// case 'not':
		// case '\\+':

		// if (numArgsInGoal == 1)
		// {
		// //var fe = goal.ExpressionList[0] as PrologNameExpression<PrologFunctor>;
		// var fe = ConvertToFunctorExpression(goal.ExpressionList[0]);

		// if (fe == null)
		// {
		// return null;
		// }

		// var innerGoal = fe.ToGoal();
		// var tempGoalList = new List<PrologGoal>() { innerGoal };
		// // This next line prevents us from adding "not" to the built-in predicates dictionary:
		// var tempCutDetectorList = new List<CutDetector>() { cutDetectorList[goalNum] };
		// var tempListOfCurrentModules = new List<PrologModule>() { listOfCurrentModules[goalNum] };
		// var cachedAllMode = allMode;
		// var cachedSolutionCollectionMode = solutionCollectionMode;
		// PrologSubstitution localSubstitution = null;

		// allMode = false;
		// solutionCollectionMode = SolutionCollectionMode.None;

		// try
		// {
		// // We don't need to use parentVariablesToAvoid here, since we don't propagate localSubstitution.
		// localSubstitution = ProveGoalList(tempGoalList, tempCutDetectorList, 0, new PrologSubstitution(),
		// innerGoal.FindBindingVariables(), null, tempListOfCurrentModules);
		// }
		// finally
		// {
		// allMode = cachedAllMode;
		// solutionCollectionMode = cachedSolutionCollectionMode;
		// }

		// if (localSubstitution != null)
		// {
		// return null;
		// }
		// else
		// {
		// return ProveGoalList(goalList, cutDetectorList, nextGoalNum, oldSubstitution, parentVariablesToAvoid,
		// variablesInQuery, listOfCurrentModules);
		// }
		// }

		// break;

		// case "functor":
		// // See http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlse39

		// if (numArgsInGoal == 3) // functor/3
		// {
		// var functorSubstitution = Functor3(goal, parentVariablesToAvoid);

		// if (functorSubstitution == null)
		// {
		// return null;
		// }

		// return ProveGoalList(goalList, cutDetectorList, nextGoalNum, oldSubstitution.Compose(functorSubstitution),
		// parentVariablesToAvoid, variablesInQuery, listOfCurrentModules);
		// }

		// break;

		// case "findall": // See http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlse49

		// if (numArgsInGoal == 3) // findall/3
		// {
		// var unifier = FindAll3(goal, parentVariablesToAvoid, currentModule);

		// if (unifier == null)
		// {
		// return null;
		// }

		// return ProveGoalList(goalList, cutDetectorList, nextGoalNum, oldSubstitution.Compose(unifier),
		// parentVariablesToAvoid, variablesInQuery, listOfCurrentModules);
		// }

		// break;

		// case "bagof": // See http://www.learnprolognow.org/lpnpage.php?pagetype=html&pageid=lpn-htmlse49
		// case "setof":

		// if (numArgsInGoal == 3) // bagof/3 or setof/3
		// {
		// return BagOfSetOf3(goal, goalList, cutDetectorList, goalNum, oldSubstitution, parentVariablesToAvoid, variablesInQuery,
		// listOfCurrentModules);
		// }

		// break;

		// case "goal_disjunction":
		// case ";":

		// if (numArgsInGoal == 2)
		// {
		// return GoalDisjunction2(goal, goalList, cutDetectorList, goalNum, oldSubstitution, parentVariablesToAvoid, variablesInQuery,
		// listOfCurrentModules);
		// }

		// break;

		// case "if_then_else":

		// if (numArgsInGoal == 3)
		// {
		// return IfThenElse3(goal.ExpressionList[0], goal.ExpressionList[1], goal.ExpressionList[2],
		// goalList, cutDetectorList, goalNum, oldSubstitution, parentVariablesToAvoid, variablesInQuery, listOfCurrentModules);
		// }

		// break;

		// case "->":

		// if (numArgsInGoal == 2)
		// {
		// var thenElseGoal = ExpressionToGoal(goal.ExpressionList[1]);

		// if (thenElseGoal != null && thenElseGoal.Name.Name == ":" && thenElseGoal.ExpressionList.length == 2)
		// {
		// return IfThenElse3(goal.ExpressionList[0], thenElseGoal.ExpressionList[0], thenElseGoal.ExpressionList[1],
		// goalList, cutDetectorList, goalNum, oldSubstitution, parentVariablesToAvoid, variablesInQuery, listOfCurrentModules);
		// }
		// }

		// break;

		// case "retract":

		// if (numArgsInGoal == 1)
		// {
		// var unifier = Retract1(goal, oldSubstitution, parentVariablesToAvoid);

		// if (unifier == null)
		// {
		// return null;
		// }

		// return ProveGoalList(goalList, cutDetectorList, nextGoalNum, oldSubstitution.Compose(unifier),
		// parentVariablesToAvoid, variablesInQuery, listOfCurrentModules);
		// }

		// break;

		// case "retractall":

		// if (numArgsInGoal == 1)
		// {
		// var unifier = RetractAll1(goal, oldSubstitution, parentVariablesToAvoid);

		// if (unifier == null)
		// {
		// return null;
		// }

		// return ProveGoalList(goalList, cutDetectorList, nextGoalNum, oldSubstitution.Compose(unifier),
		// parentVariablesToAvoid, variablesInQuery, listOfCurrentModules);
		// }

		// break;

		// default:
		// 	break;
		// }

		let resultSubstitution = this.ProveGoalListUsingModule(
			goal,
			goalList,
			// cutDetectorList,
			nextGoalNum,
			oldSubstitution,
			parentVariablesToAvoid,
			variablesInQuery,
			currentModule,
			listOfCurrentModules
		);

		if (typeof resultSubstitution !== 'undefined') {
			return resultSubstitution;
		}

		const goalSignature = new StringIntKey(
			goal.Name.Name,
			goal.ExpressionList.length
		).toString();

		for (const key of currentModule.ImportList.keys()) {
			const v = currentModule.ImportList.get(key);

			if (key === goalSignature && typeof v !== 'undefined') {
				resultSubstitution = this.ProveGoalListUsingModule(
					goal,
					goalList,
					// cutDetectorList,
					nextGoalNum,
					oldSubstitution,
					parentVariablesToAvoid,
					variablesInQuery,
					v,
					listOfCurrentModules
				);

				if (typeof resultSubstitution !== 'undefined') {
					return resultSubstitution;
				}
			}

			return undefined;
		}
	}

	private ProveGoalListUsingModule(
		goal: PrologGoal,
		goalList: PrologGoal[],
		// List<CutDetector> cutDetectorList,
		nextGoalNum: number,
		oldSubstitution: PrologSubstitution,
		parentVariablesToAvoid: Set<PrologVariable>,
		variablesInQuery: PrologVariable[], // Print these variables and their values automatically upon success if there is no print() goal at the end
		currentModule: PrologModule,
		listOfCurrentModules: PrologModule[]
	): PrologSubstitution | undefined {
		const variablesToAvoid = goal.FindBindingVariables();

		variablesToAvoid.unionInPlace(parentVariablesToAvoid);
		variablesToAvoid.unionInPlace(oldSubstitution.FindBindingVariables());

		// #if CONSOLE_WRITELINE
		// Console.WriteLine("ProveGoal: *** Trying to prove goal {0}", goal);
		// Console.WriteLine("ProveGoal: oldSubstitution is: {0}", oldSubstitution);
		// #endif

		// Iterate over a copy of the ClauseList to protect against InvalidOperationExceptions due to assert*/retract*.
		const clauseListCopy = currentModule.ClauseList.slice();

		// console.log(
		// 	'typeof clauseListCopy is:',
		// 	typeof clauseListCopy,
		// 	clauseListCopy.constructor.name
		// );

		for (const clause of clauseListCopy) {
			// console.log(
			// 	'typeof clause is:',
			// 	typeof clause,
			// 	clause.constructor.name
			// );

			const newClause = clause.RenameVariables(variablesToAvoid, this);

			// #if CONSOLE_WRITELINE
			// Console.WriteLine("ProveGoal: Trying to unify goal {0} with Lhs of clause {1}", goal, newClause);
			// #endif

			const unifier = newClause.Lhs.Unify(goal);

			if (typeof unifier === 'undefined') {
				// #if CONSOLE_WRITELINE
				// Console.WriteLine("ProveGoal: Unification failed.");
				// #endif
				continue;
			}

			// #if CONSOLE_WRITELINE
			// Console.WriteLine("ProveGoal: goal unifies with Lhs of clause: {0}", newClause);
			// Console.WriteLine("ProveGoal: unifier is: {0}", unifier);
			// //Console.WriteLine("ProveGoal: Composing unifier with substitution: {0}", oldSubstitution);
			// #endif

			let localSubstitution: PrologSubstitution | undefined =
				oldSubstitution.Compose(unifier);

			// #if CONSOLE_WRITELINE
			// Console.WriteLine("ProveGoal: Composed substitution: {0}", localSubstitution);
			// #endif

			// See the program F2.16.txt for a test of the cut.
			const newVariablesToAvoid = this.GetVariablesFromGoalList(
				newClause.Rhs
			);

			newVariablesToAvoid.unionInPlace(variablesToAvoid);

			// ThAW 2014/03/06 : We want to support cuts in goal disjunctions and if/then/else constructs.
			// var cutDetector = new CutDetector();

			// goalList.InsertRange(nextGoalNum, newClause.Rhs);
			goalList.splice(nextGoalNum, 0, ...newClause.Rhs);

			// Insert as many copies of the cutDetector reference as we have subgoals in newClause.Rhs .
			// cutDetectorList.InsertRange(nextGoalNum, newClause.Rhs.Select(g => cutDetector));

			// listOfCurrentModules.InsertRange(
			// 	nextGoalNum,
			// 	newClause.Rhs.Select((g) => currentModule)
			// );

			for (let i = 0; i < newClause.Rhs.length; i++) {
				// cutDetectorList.splice(nextGoalNum, 0, cutDetector);
				listOfCurrentModules.splice(nextGoalNum, 0, currentModule);
			}

			try {
				localSubstitution = this.ProveGoalList(
					goalList,
					// cutDetectorList,
					nextGoalNum,
					localSubstitution,
					newVariablesToAvoid,
					variablesInQuery,
					listOfCurrentModules
				);
			} finally {
				// catch (CutBacktrackException ex) // 2014/03/07
				// {
				// // Clean up the lists before we return or re-throw.
				// //goalList.RemoveRange(nextGoalNum, newClause.Rhs.Count);
				// //cutDetectorList.RemoveRange(nextGoalNum, newClause.Rhs.Count);

				// if (ex.Guid.Equals(cutDetector.Guid))
				// {
				// return null;
				// }

				// throw;
				// }

				// goalList.RemoveRange(nextGoalNum, newClause.Rhs.Count);
				goalList.splice(nextGoalNum, newClause.Rhs.length);

				// cutDetectorList.RemoveRange(nextGoalNum, newClause.Rhs.Count);
				// cutDetectorList.splice(nextGoalNum, newClause.Rhs.length);

				// listOfCurrentModules.RemoveRange(
				// 	nextGoalNum,
				// 	newClause.Rhs.Count
				// );
				listOfCurrentModules.splice(nextGoalNum, newClause.Rhs.length);
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

	//         foreach (var otherClause in currentModule.ClauseList)
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

	// 	// #if SUPPORT_USER_DEFINED_OPERATORS
	//     private OperatorType? ExpressionToOperatorType(IPrologExpression expr)
	//     {
	//         var fe = expr as PrologNameExpression<PrologFunctor>;

	//         if (fe == null || fe.ExpressionList.Count != 0)
	//         {
	//             return null;
	//         }

	//         switch (fe.Name.Name)
	//         {
	//             case "fx": return OperatorType.fx;
	//             case "fy": return OperatorType.fy;
	//             case "xfx": return OperatorType.xfx;
	//             case "xfy": return OperatorType.xfy;
	//             case "yfx": return OperatorType.yfx;
	//             case "xf": return OperatorType.xf;
	//             case "yf": return OperatorType.yf;
	//             default: return null;
	//         }
	//     }

	//     private bool AddOperator(int precedence, OperatorType opType, IPrologExpression expr)
	//     {
	//         var fe = expr as PrologNameExpression<PrologFunctor>;

	//         if (fe == null || fe.ExpressionList.Count != 0)
	//         {
	//             return false;
	//         }

	//         Operators.Add(new PrologOperator(precedence, opType, fe.Name.Name));
	//         return true;
	//     }

	//     public string LoadFile(string filename, string currentModuleName = "")
	//     {

	//         if (FileLoader == null)
	//         {
	//             throw new Exception("Load file command: The file loader is null.");
	//         }

	//         if (!filename.Contains('.'))
	//         {
	//             filename = filename + ".pl";
	//         }

	//         if (!filename.StartsWith(@"\") && !filename.Contains(':') // I.e. if  filename is relative, not absolute.
	//             && !string.IsNullOrEmpty(PathToDefaultDirectory))
	//         {
	//             filename = Path.Combine(PathToDefaultDirectory, filename);
	//         }

	//         return FileLoader.LoadFileUsingCompletedPath(filename, currentModuleName);
	//     }

	//     private string ProcessCommand(IPrologExpression expr, ref string currentModuleName)
	//     {
	//         var exprList = PrologListToCSharpList(expr);

	//         if (exprList != null)
	//         {

	//             foreach (var exprFromList in exprList)
	//             {
	//                 var filename_fe = exprFromList as PrologNameExpression<PrologFunctor>;

	//                 if (filename_fe == null || filename_fe.ExpressionList.Count != 0)
	//                 {
	//                     return string.Format("Load file command: Invalid filename '{0}'.", exprFromList);
	//                 }

	// 				// #if !DEAD_CODE
	// 				LoadFile(filename_fe.Name.Name, currentModuleName);
	// 				// #else
	// 				// var filename = filename_fe.Name.Name;

	// 				// //if (!filename.Contains('\\')
	// 				// if (!filename.StartsWith(@"\") && !filename.Contains(':') // I.e. if  filename is relative, not absolute.
	// 				//     && !string.IsNullOrEmpty(PathToDefaultDirectory))
	// 				// {
	// 				//     filename = Path.Combine(PathToDefaultDirectory, filename);
	// 				// }

	// 				// FileLoader.LoadFileUsingCompletedPath(filename, currentModuleName);
	// 				// #endif
	//             }

	//             return string.Format("{0} file(s) loaded.", exprList.Count);
	//         }

	//         var fe = expr as PrologNameExpression<PrologFunctor>;

	//         if (fe != null)
	//         {

	//             if (fe.Name.Name == "op" && fe.ExpressionList.Count == 3)
	//             {
	//                 var opPredecence = fe.ExpressionList[0] as PrologIntegerLiteral;
	//                 var opType = ExpressionToOperatorType(fe.ExpressionList[1]);
	//                 var nameList = PrologListToCSharpList(fe.ExpressionList[2]);

	//                 if (opPredecence == null || !opType.HasValue /* || name == null */ )
	//                 {
	//                 }
	//                 else if (nameList != null)
	//                 {

	//                     foreach (var name in nameList)
	//                     {

	//                         if (!AddOperator(opPredecence.Value, opType.Value, name))
	//                         {
	//                             return InvalidCommand;
	//                         }
	//                     }

	//                     return OperatorAdded;
	//                 }
	//                 else if (AddOperator(opPredecence.Value, opType.Value, fe.ExpressionList[2]))
	//                 {
	//                     return OperatorAdded;
	//                 }
	//             }
	//             else if (fe.Name.Name == "module" && fe.ExpressionList.Count == 2)
	//             {
	//                 var name_fe = ConvertToFunctorExpression(fe.ExpressionList[0]);
	//                 var exportList = PrologListToCSharpList(fe.ExpressionList[1]);

	//                 if (name_fe == null || name_fe.ExpressionList.Count != 0 || exportList == null)
	//                 {
	//                     return "module/2 command: Syntax error.";
	//                 }

	//                 var processedExportList = new List<StringIntKey>();

	//                 foreach (var ex in exportList)
	//                 {
	//                     var ex_fe = ex as PrologNameExpression<PrologFunctor>;

	//                     if (ex_fe == null || ex_fe.Name.Name != "/" || ex_fe.ExpressionList.Count != 2)
	//                     {
	//                         return "module/2 command: Syntax error in export list (1).";
	//                     }

	//                     var exName_fe = ConvertToFunctorExpression(ex_fe.ExpressionList[0]);
	//                     var exArity = ex_fe.ExpressionList[1] as PrologIntegerLiteral;

	// 					// #if DEAD_CODE
	// 					// if (exName_fe == null || exName_fe.ExpressionList.Count != 0 || exArity == null || exArity.Value < 0)
	// 					// {
	// 					//     return "module/2 command: Syntax error in export list (2).";
	// 					// }
	// 					// #else
	//                     if (exName_fe == null)
	//                     {
	//                         return "exName_fe is null.";
	//                     }
	//                     else if (exName_fe.ExpressionList.Count != 0)
	//                     {
	//                         return string.Format("exName_fe expression count is {0}, not zero.", exName_fe.ExpressionList.Count);
	//                     }
	//                     else if (exArity == null)
	//                     {
	//                         return string.Format("exArity ({0}) is null.", fe.ExpressionList[1]);
	//                     }
	//                     else if (exArity.Value < 0)
	//                     {
	//                         return string.Format("exArity is {0} < 0.", exArity.Value);
	//                     }
	// 					// #endif
	//                     processedExportList.Add(new StringIntKey(exName_fe.Name.Name, exArity.Value));
	//                 }

	//                 var newModule = new PrologModule(processedExportList);

	//                 dictModules[name_fe.Name.Name] = newModule;

	//                 var currentModule = FindModule(currentModuleName);

	//                 foreach (var key in processedExportList)
	//                 {
	//                     currentModule.ImportList.Add(new KeyValuePair<StringIntKey, PrologModule>(key, newModule));
	//                 }

	//                 currentModuleName = name_fe.Name.Name;
	//                 return string.Format("The module '{0}' has been created.", name_fe.Name.Name);
	//             }
	//         }

	//         return InvalidCommand;
	//     }
	// 	// #endif

	public ProcessInput(
		parseResult: PrologClause | PrologGoal[],
		currentModuleName = ''
	): string {
		// #if SUPPORT_USER_DEFINED_OPERATORS
		const inputTypeName = parseResult.constructor.name;

		// console.log('ProcessInput() : Type of parseResult is:', inputTypeName);

		// const inputAsFunctorExpression =
		// 	parseResult as PrologNameExpression<PrologFunctor>;

		// if (typeof inputAsFunctorExpression !== 'undefined' && inputAsFunctorExpression.Name.Name == ':-' && inputAsFunctorExpression.ExpressionList.length === 1) {
		// if (inputTypeName === PrologNameExpression<PrologFunctor>.name && inputAsFunctorExpression.Name.Name === ':-' && inputAsFunctorExpression.ExpressionList.length === 1) {
		// 	return ProcessCommand(inputAsFunctorExpression.ExpressionList[0], currentModuleName);
		// }
		// #endif

		const clause = parseResult as PrologClause;
		const goalList = parseResult as PrologGoal[];

		// console.log('ProcessInput() : parseResult as PrologClause is:', clause);
		// console.log(
		// 	'ProcessInput() : parseResult as PrologGoal[] is:',
		// 	goalList
		// );
		// console.log(
		// 	'ProcessInput() : typeof parseResult.length is:',
		// 	typeof goalList.length
		// );

		// if (parseResult is PrologClause) {
		// if (inputTypeName === PrologClause.name) {
		if (typeof goalList.length === 'undefined') {
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
			currentModule.ClauseList.push(clause);

			return PrologGlobalInfo.ClauseAdded;
			// } else if (parseResult is List<PrologGoal>) {
		} else if (typeof goalList !== 'undefined') {
			// var goalList = new List<PrologGoal>((List<PrologGoal>)parseResult);
			// var cutDetectorList = new List<CutDetector>();
			const listOfCurrentModules: PrologModule[] = [];
			// var cutDetector = new CutDetector();
			// let substitution: PrologSubstitution;

			// sbOutput.Clear();
			// goalList.ForEach(g => cutDetectorList.Add(cutDetector));
			// goalList.forEach((g: PrologGoal) =>
			// 	listOfCurrentModules.push(this.DefaultModule)
			// );

			for (let i = 0; i < goalList.length; i++) {
				listOfCurrentModules.push(this.DefaultModule);
			}

			// try {
			const substitution = this.ProveGoalList(
				goalList,
				// cutDetectorList,
				0,
				new PrologSubstitution(),
				this.GetVariablesFromGoalList(goalList),
				this.GetListOfBindingVariablesFromGoalList(goalList),
				listOfCurrentModules
			);
			// } catch (CutBacktrackException ex) {

			// 	if (!ex.Guid.Equals(cutDetector.Guid)) {
			// 		throw;
			// 	}
			// }

			// if (sbOutput.Length > 0) {
			// 	sbOutput.AppendLine();
			// }

			// sbOutput.Append(substitution != null ? Satisfied : NotSatisfied);

			// return sbOutput.ToString();

			if (typeof substitution !== 'undefined') {
				console.log(
					'Satisfying substitution is:',
					substitution.toString()
				);
			}

			return typeof substitution !== 'undefined'
				? PrologGlobalInfo.Satisfied
				: PrologGlobalInfo.NotSatisfied;
		} else if (typeof parseResult === 'undefined') {
			throw new Error(
				'PrologGlobalInfo.ProcessInput() : parseResult is undefined'
			);
		} else {
			throw new Error(
				`PrologGlobalInfo.ProcessInput() : parseResult is of unrecognized type ${inputTypeName}`
			);
		}
	}

	// 	// #if SUPPORT_USER_DEFINED_OPERATORS
	public static ExpressionToCSharpList(
		expr: IPrologExpression
	): IPrologExpression[] {
		const result = this.PrologListToCSharpList(expr);

		if (typeof result !== 'undefined') {
			return result;
		}

		return this.CommaSeparatedListToCSharpList(expr);
	}

	//     private IPrologExpression ParseList_Operators(List<Token> tokenList, int startIndex, TokenType openingDelimiter, TokenType closingDelimiter,
	//         out int nextIndex)
	//     {
	//         // Assert: tokenList[startIndex - 1].TokenType == openingDelimiter
	//         var delimiterCount = 1;
	//         var i = startIndex;

	//         for (; ; )
	//         {

	//             if (i >= tokenList.Count)
	//             {
	//                 throw new Exception("ParseList_Operators() : Unclosed list.");
	//             }

	//             if (tokenList[i].TokenType == openingDelimiter)
	//             {
	//                 ++delimiterCount;
	//             }
	//             else if (tokenList[i].TokenType == closingDelimiter)
	//             {
	//                 --delimiterCount;

	//                 if (delimiterCount == 0)
	//                 {
	//                     break;
	//                 }
	//             }

	//             ++i;
	//         }

	//         // The list to parse begins at startIndex and ends at i - 1 (inclusive).
	//         nextIndex = i + 1;
	//         return Parse_Operators(tokenList.Skip(startIndex).Take(i - startIndex).ToList());
	//     }

	//     private Stack<object> CloneOperatorSemanticStack(Stack<object> semanticStack)
	//     {
	//         var list = new List<object>();

	//         foreach (var obj in semanticStack)
	//         {
	//             var objAsOperatorUsage = obj as PrologOperatorUsage;

	//             if (objAsOperatorUsage != null)
	//             {
	//                 list.Add(objAsOperatorUsage.Clone());
	//             }
	//             else
	//             {
	//                 list.Add(obj);
	//             }
	//         }

	//         list.Reverse();

	//         return new Stack<object>(list);
	//     }

	public static CommaSeparatedListToCSharpList(
		expr: IPrologExpression
	): IPrologExpression[] {
		const exprAsFunctorExpression =
			expr as PrologNameExpression<PrologFunctor>;

		if (
			typeof exprAsFunctorExpression === 'undefined' ||
			exprAsFunctorExpression.Name.Name !== ',' ||
			exprAsFunctorExpression.ExpressionList.length !== 2
		) {
			return [expr];
		} else {
			// #if !DEAD_CODE
			const list = this.CommaSeparatedListToCSharpList(
				exprAsFunctorExpression.ExpressionList[1]
			);

			list.unshift(exprAsFunctorExpression.ExpressionList[0]);
			// #else
			// var list = CommaSeparatedListToCSharpList(exprAsFunctorExpression.ExpressionList[0]);

			// list.Add(exprAsFunctorExpression.ExpressionList[1]);
			// #endif

			return list;
		}
	}

	//     private IPrologExpression CSharpListToSequence(List<IPrologExpression> list, int index = 0)
	//     {

	//         if (index < 0 || index >= list.Count)
	//         {
	//             throw new Exception("CSharpListToSequence() : Invalid index.");
	//         }
	//         else if (index == list.Count - 1)
	//         {
	//             return list[index];
	//         }
	//         else
	//         {
	//             return new PrologNameExpression<PrologFunctor>(gs, new PrologFunctor("consSeq"),
	//                 new List<IPrologExpression>() { list[index], CSharpListToSequence(list, index + 1)});
	//         }
	//     }

	//     private IPrologExpression AddAsPrologListTail(IPrologExpression expr, IPrologExpression tail)
	//     {
	//         var exprAsFunctorExpression = expr as PrologNameExpression<PrologFunctor>;

	//         if (exprAsFunctorExpression != null && exprAsFunctorExpression.Name.Name == "[]" && exprAsFunctorExpression.ExpressionList.Count == 0)
	//         {
	//             return tail;
	//         }
	//         else if (exprAsFunctorExpression != null && exprAsFunctorExpression.Name.Name == "." && exprAsFunctorExpression.ExpressionList.Count == 2)
	//         {
	//             return new PrologNameExpression<PrologFunctor>(gs, new PrologFunctor("."),
	//                 new List<IPrologExpression>() { exprAsFunctorExpression.ExpressionList[0],
	//                     AddAsPrologListTail(exprAsFunctorExpression.ExpressionList[1], tail) });
	//         }
	//         else
	//         {
	//             throw new Exception("AddAsPrologListTail() : Failed.");
	//         }
	//     }

	//     private IPrologExpression CommaVBarExprToPrologList(IPrologExpression expr)
	//     {
	//         IPrologExpression firstExpr;
	//         IPrologExpression secondExpr;
	//         var exprAsFunctorExpression = expr as PrologNameExpression<PrologFunctor>;

	//         if (exprAsFunctorExpression != null && exprAsFunctorExpression.Name.Name == "," && exprAsFunctorExpression.ExpressionList.Count == 2)
	//         {
	//             firstExpr = exprAsFunctorExpression.ExpressionList[0];
	//             secondExpr = CommaVBarExprToPrologList(exprAsFunctorExpression.ExpressionList[1]);
	//         }
	//         else if (exprAsFunctorExpression != null && exprAsFunctorExpression.Name.Name == "|" && exprAsFunctorExpression.ExpressionList.Count == 2)
	//         {
	//             // Note that "|" binds more loosely than ",".
	//             firstExpr = exprAsFunctorExpression.ExpressionList[0];
	//             secondExpr = exprAsFunctorExpression.ExpressionList[1];
	//             return AddAsPrologListTail(CommaVBarExprToPrologList(firstExpr), secondExpr);
	//         }
	//         else
	//         {
	//             firstExpr = expr;
	//             secondExpr = CreateAtom("[]");
	//         }

	//         return new PrologNameExpression<PrologFunctor>(gs, new PrologFunctor("."), new List<IPrologExpression>() { firstExpr, secondExpr });
	//     }

	//     private IPrologExpression ExpressionToSequence(IPrologExpression expr)
	//     {
	//         var fe = expr as PrologNameExpression<PrologFunctor>;

	//         if (fe != null && fe.Name.Name == "," && fe.ExpressionList.Count == 2)
	//         {
	//             return new PrologNameExpression<PrologFunctor>(gs, new PrologFunctor("consSeq"),
	//                 new List<IPrologExpression>() { fe.ExpressionList[0], ExpressionToSequence(fe.ExpressionList[1]) });
	//         }

	//         return expr;
	//     }

	//     private IPrologExpression MarkCommaListAsNonDCG(IPrologExpression expr)
	//     {
	//         var exprAsFunctorExpression = expr as PrologNameExpression<PrologFunctor>;

	//         if (exprAsFunctorExpression != null && exprAsFunctorExpression.Name.Name == "," && exprAsFunctorExpression.ExpressionList.Count == 2)
	//         {
	//             var expr1AsFunctorExpression = ConvertToFunctorExpression(exprAsFunctorExpression.ExpressionList[0]);

	//             expr1AsFunctorExpression.DCGDoNotAddExtraArguments = true;
	//             return new PrologNameExpression<PrologFunctor>(gs, new PrologFunctor(","),
	//                 new List<IPrologExpression>() { expr1AsFunctorExpression, MarkCommaListAsNonDCG(exprAsFunctorExpression.ExpressionList[1]) });
	//         }
	//         else
	//         {
	//             exprAsFunctorExpression = ConvertToFunctorExpression(expr);

	//             exprAsFunctorExpression.DCGDoNotAddExtraArguments = true;
	//             return exprAsFunctorExpression;
	//         }
	//     }

	//     private bool BindValueToLeftOperator(PrologOperator leftOp, PrologOperator rightOp, out bool error)
	//     {
	//         error = false;

	//         if ((leftOp.IsPrefix || leftOp.IsInfix) && rightOp.IsPrefix)
	//         {
	//             return true;
	//         }
	//         else if (leftOp.IsPostfix && (rightOp.IsInfix || rightOp.IsPostfix))
	//         {
	//             return false;
	//         }
	//         else if (leftOp.IsPostfix && rightOp.IsPrefix)
	//         {
	//             error = true;
	//             return false;
	//         }
	//         else if (leftOp.Precedence < rightOp.Precedence)
	//         {
	//             return true;
	//         }
	//         else if (leftOp.Precedence > rightOp.Precedence)
	//         {
	//             return false;
	//         }
	//         else if ((leftOp.OpType == OperatorType.xfx || leftOp.OpType == OperatorType.yfx || leftOp.OpType == OperatorType.fx) &&
	//             (rightOp.OpType == OperatorType.yfx || rightOp.OpType == OperatorType.yf))
	//         {
	//             return true;
	//         }
	//         else if ((leftOp.OpType == OperatorType.xfy || leftOp.OpType == OperatorType.fy) &&
	//             (rightOp.OpType == OperatorType.xfx || rightOp.OpType == OperatorType.xfy || rightOp.OpType == OperatorType.xf))
	//         {
	//             return false;
	//         }
	//         else
	//         {
	//             error = true;
	//             return false;
	//         }
	//     }

	//     private IPrologExpression Parse_Operators_PushOp(PrologOperator op, List<Token> tokenList, int nextIndex, Stack<object> originalSemanticStack)
	//     {
	//         var semanticStack = CloneOperatorSemanticStack(originalSemanticStack);

	//         semanticStack.Push(new PrologOperatorUsage(op));

	//         // We have an operator.
	//         // Now see if the top three items on the stack are a value between two operators.
	//         // TODO: Also handle the case where a value is preceded by nothing and is followed by an operator (semanticStack.Count == 2).
	//         var done = false;

	//         while (!done)
	//         {
	//             done = true;

	//             if (semanticStack.Count == 0)
	//             {
	//                 return null;
	//             }
	//             else if (semanticStack.Count == 1)
	//             {
	//                 var onlyOp = semanticStack.Peek() as PrologOperatorUsage;

	//                 if (onlyOp != null && !onlyOp.Op.IsPrefix)
	//                 {
	//                     return null;
	//                 }
	//             }
	//             else
	//             //if (semanticStack.Count >= 2)
	//             {
	//                 var rightOp = (PrologOperatorUsage)semanticStack.Pop();
	//                 object rightOpAsObject = rightOp;

	//                 //Console.WriteLine("Parse_Operators_PushOp: rightOp is {0}.", rightOp.Op.Name);
	//                 //Console.WriteLine("Parse_Operators_PushOp: potential value has type {0}.", semanticStack.Peek().GetType().Name);

	//                 if (semanticStack.Peek() is IPrologExpression)
	//                 {
	//                     var inBetweenValue = (IPrologExpression)semanticStack.Pop();

	//                     if (semanticStack.Count == 0) // Not 2; we have just popped twice.
	//                     {
	//                         // Determine if rightOp is a postix operator.

	//                         //if (rightOp.Op.IsPostfix)
	//                         if (!rightOp.Op.IsPrefix)
	//                         {
	//                             //Console.WriteLine("Binding value {0} to rightOp {1}.", inBetweenValue, rightOp.Op.Name);
	//                             rightOp.Arguments.Add(inBetweenValue);
	//                             inBetweenValue = null;  // This value has been bound to an operator.

	//                             if (rightOp.IsComplete)
	//                             {
	//                                 rightOpAsObject = rightOp.ToFunctorExpression();
	//                             }
	//                         }
	//                     }
	//                     else if (semanticStack.Peek() is PrologOperatorUsage)
	//                     {
	//                         var leftOp = (PrologOperatorUsage)semanticStack.Pop();
	//                         object leftOpAsObject = leftOp;
	//                         bool error;
	//                         var bindToLeftOp = BindValueToLeftOperator(leftOp.Op, rightOp.Op, out error);

	//                         if (error)
	//                         {
	//                             //Console.WriteLine("BindValueToLeftOperator() : Cannot bind the value to either operator.");
	//                             return null;
	//                         }

	//                         if (bindToLeftOp)
	//                         {
	//                             //Console.WriteLine("Binding value {0} to leftOp {1}.", inBetweenValue, leftOp.Op.Name);
	//                             leftOp.Arguments.Add(inBetweenValue);

	//                             if (leftOp.IsComplete)
	//                             {
	//                                 //Console.WriteLine("leftOp {0} is now complete; will try to bind again.", leftOp.Op.Name);
	//                                 leftOpAsObject = leftOp.ToFunctorExpression();
	//                                 done = false;
	//                             }
	//                         }
	//                         else
	//                         {
	//                             //Console.WriteLine("Binding value {0} to rightOp {1}.", inBetweenValue, rightOp.Op.Name);
	//                             rightOp.Arguments.Add(inBetweenValue);

	//                             if (rightOp.IsComplete)
	//                             {
	//                                 rightOpAsObject = rightOp.ToFunctorExpression();
	//                             }
	//                         }

	//                         inBetweenValue = null;  // This value has been bound to an operator.
	//                         semanticStack.Push(leftOpAsObject);
	//                     }

	//                     if (inBetweenValue != null)
	//                     {
	//                         semanticStack.Push(inBetweenValue);
	//                     }
	//                 }
	//                 else if (!rightOp.Op.IsPrefix)
	//                 {
	//                     return null;
	//                 }

	//                 semanticStack.Push(rightOpAsObject);
	//             }
	//         }

	//         return Parse_Operators(tokenList, nextIndex, semanticStack);
	//     }

	//     private void FoldStackAtEnd(Stack<object> semanticStack)
	//     {

	//         while (semanticStack.Count > 1)
	//         {

	//             if (!(semanticStack.Peek() is IPrologExpression))
	//             {
	//                 //Console.WriteLine("FoldStackAtEnd: The object at the top of the stack is not an IPrologExpression.");
	//                 return;
	//             }

	//             var value = (IPrologExpression)semanticStack.Pop();

	//             //Console.WriteLine("FoldStackAtEnd: value is {0} ({1}).", value, value.GetType().Name);

	//             var leftOp = semanticStack.Peek() as PrologOperatorUsage;

	//             if (leftOp == null || leftOp.IsComplete || leftOp.Op.IsPostfix)
	//             {
	// 				// #if DEAD_CODE
	// 				// if (leftOp == null)
	// 				// {
	// 				//     Console.WriteLine("FoldStackAtEnd: leftOp is null; top of stack is {0} ({1}).", semanticStack.Peek(), semanticStack.Peek().GetType().Name);
	// 				// }
	// 				// else if (leftOp.IsComplete)
	// 				// {
	// 				//     Console.WriteLine("FoldStackAtEnd: leftOp is already complete.");
	// 				// }
	// 				// else if (leftOp.Op.IsPostfix)
	// 				// {
	// 				//     Console.WriteLine("FoldStackAtEnd: leftOp is postfix.");
	// 				// }
	// 				// #endif
	//                 semanticStack.Push(value);
	//                 return;
	//             }

	//             leftOp.Arguments.Add(value);

	//             if (!leftOp.IsComplete) // This "if" block is probably unnecessary.
	//             {
	//                 //Console.WriteLine("FoldStackAtEnd: After adding an argument, leftOp {0} is not complete.", leftOp.Op.Name);
	//                 return;
	//             }

	//             semanticStack.Pop();
	//             semanticStack.Push(leftOp.ToFunctorExpression());
	//         }

	//         //Console.WriteLine("FoldStackAtEnd succeeded.");
	//     }

	//     private IPrologExpression Parse_Operators(List<Token> tokenList, int index = 0, Stack<object> originalSemanticStack = null)
	//     {
	//         Stack<object> semanticStack;

	//         if (originalSemanticStack == null)
	//         {
	//             semanticStack = new Stack<object>();
	//         }
	//         else
	//         {
	//             semanticStack = CloneOperatorSemanticStack(originalSemanticStack);
	//         }

	//         // The token list must end with a dot.

	//         if (index < 0 || index > tokenList.Count)
	//         {
	//             throw new Exception("Parse_Operators() : Index out of bounds.");
	//         }
	//         else if (index == tokenList.Count)
	//         {
	//             // Do the folding of objects (operators and their values) on the semantic stack so that no operator usage is incomplete.
	//             FoldStackAtEnd(semanticStack);
	//             // Then:

	//             if (semanticStack.Count == 1)
	//             {
	//                 // Is this object complete, and has it been converted to an IPrologExpression ?
	//                 var obj = semanticStack.Pop();

	//                 var objAsOperatorUsage = obj as PrologOperatorUsage;

	//                 if (objAsOperatorUsage != null)
	//                 {

	//                     if (!objAsOperatorUsage.IsComplete)
	//                     {
	//                         //Console.WriteLine("At end: Operator {0} is incomplete.", objAsOperatorUsage.Op.Name);
	//                         return null; // Syntax error.
	//                     }

	//                     return objAsOperatorUsage.ToFunctorExpression();
	//                 }
	//                 else
	//                 {
	//                     return (IPrologExpression)obj;
	//                 }
	//             }
	//             else
	//             {
	//                 //throw new Exception(string.Format("Parse_Operators() : Finishing up: There are {0} objects on the semantic stack.", semanticStack.Count));
	//                 //Console.WriteLine("At end: There are {0} objects on the semantic stack.", semanticStack.Count);
	//                 return null; // Syntax error.
	//             }
	//         }

	//         var token = tokenList[index];
	//         var value = token.TokenValue;
	//         var valueAsString = value.ToString();
	//         IPrologExpression currentExpr = null;
	//         IPrologExpression innerExpr;
	//         var nextIndex = index + 1;

	//         switch (token.TokenType)
	//         {
	//             case TokenType.T_IntLit:
	//                 currentExpr = new PrologIntegerLiteral((int)value);
	//                 break;

	//             case TokenType.T_FltLit:
	//                 currentExpr = new PrologFloatLiteral((double)value);
	//                 break;

	//             case TokenType.T_StrLit:
	//                 currentExpr = CSharpStringToPrologCodeList((string)value);
	//                 break;

	//             case TokenType.T_LeftBracket:
	//                 innerExpr = ParseList_Operators(tokenList, index + 1, TokenType.T_LeftBracket, TokenType.T_RightBracket, out nextIndex);

	//                 if (innerExpr == null)
	//                 {
	//                     return null;
	//                 }

	//                 //Console.WriteLine("Brackets: innerExpr is a {0}.", innerExpr.GetType().Name);

	//                 // If the object at the top of the stack is an atom or a variable, pop it and create a functor expression with arguments.

	//                 if (semanticStack.Count > 0)
	//                 {
	//                     //Console.WriteLine("Brackets: The top of the stack is a {0}.", semanticStack.Peek().GetType().Name);

	//                     var fe = semanticStack.Peek() as PrologNameExpression<PrologFunctor>;
	//                     var v = semanticStack.Peek() as PrologVariable;
	//                     var ou = semanticStack.Peek() as PrologOperatorUsage;
	//                     var inner_fe = innerExpr as PrologNameExpression<PrologFunctor>;

	//                     if ((fe != null && fe.ExpressionList.Count == 0) || v != null)
	//                     {
	//                         var obj = semanticStack.Pop();

	//                         currentExpr = new PrologNameExpression<PrologFunctor>(gs, new PrologFunctor(obj.ToString()), CommaSeparatedListToCSharpList(innerExpr));
	//                     }
	//                     else if (ou != null && (ou.Arguments.Count != ou.ExpectedNumberOfArguments - 1 || (inner_fe != null && inner_fe.Name.Name == ","))
	//                         && ou.Op.Name != "," && ou.Op.Name != "|" && ou.Op.Name != "^") // The ^ check might be unnecessary.
	//                     {
	//                         return null;
	//                     }
	//                     else
	//                     {
	//                         currentExpr = ExpressionToSequence(innerExpr);
	//                     }
	//                 }
	//                 else
	//                 {
	//                     //Console.WriteLine("Brackets: The stack is empty.");
	//                     currentExpr = ExpressionToSequence(innerExpr);
	//                 }

	//                 break;

	//             case TokenType.T_LeftSquareBracket:

	//                 if (index + 1 < tokenList.Count && tokenList[index + 1].TokenType == TokenType.T_RightSquareBracket)
	//                 {
	//                     currentExpr = CreateAtom("[]");
	//                     nextIndex = index + 2;
	//                     break;
	//                 }

	//                 innerExpr = ParseList_Operators(tokenList, index + 1, TokenType.T_LeftSquareBracket, TokenType.T_RightSquareBracket, out nextIndex);

	//                 if (innerExpr == null)
	//                 {
	//                     return null;
	//                 }

	//                 currentExpr = CommaVBarExprToPrologList(innerExpr);
	//                 break;

	//             case TokenType.T_LeftCurlyBrace:
	//                 innerExpr = ParseList_Operators(tokenList, index + 1, TokenType.T_LeftCurlyBrace, TokenType.T_RightCurlyBrace, out nextIndex);

	//                 if (innerExpr == null)
	//                 {
	//                     return null;
	//                 }

	//                 currentExpr = MarkCommaListAsNonDCG(innerExpr);
	//                 break;

	//             case TokenType.T_StrLit2: // The contents of a single-quoted string.  It is never an operator.
	//                 currentExpr = CreateAtom(valueAsString);
	//                 break;

	//             default:
	//                 break;
	//         }

	//         if (currentExpr != null)
	//         {
	//             //Console.WriteLine("Pushing the value {0}.", currentExpr);
	//             semanticStack.Push(currentExpr);
	//             return Parse_Operators(tokenList, nextIndex, semanticStack);
	//         }

	//         // We will interpret the token (valueAsString) as an operator first, if possible, then as a functor.
	//         // Might an operator name begin with a capital letter?

	//         foreach (var op in Operators.Where(op => op.Name == valueAsString))
	//         {
	//             //Console.WriteLine("Attempting to match '{0}' to operator {1} ({2}, {3}).", valueAsString, op.Name, op.OpType, op.Precedence);

	//             var result = Parse_Operators_PushOp(op, tokenList, index + 1, semanticStack);

	//             if (result != null)
	//             {
	//                 return result;
	//             }
	//         }

	//         //if (tokenAsSymbol == Symbol.T_NameBeginningWithCapital)
	//         if (char.IsUpper(valueAsString, 0)
	//             // The following supports non-binding variables such as _ and _Foo .
	//             // See http://www.csupomona.edu/~jrfisher/www/prolog_tutorial/2_3.html
	//             // TODO: Should we require the second character (if it exists) to be a capital letter if the first is an underscore?
	//             || valueAsString.StartsWith("_"))
	//         {
	//             currentExpr = new PrologVariable(valueAsString);
	//         }
	//         else
	//         {
	//             //Console.WriteLine("Creating the atom '{0}'.", valueAsString);
	//             currentExpr = CreateAtom(valueAsString);
	//             // TODO: Should we look for a left bracket immediately following, or should we wait until the left bracket is the current token?
	//         }

	//         semanticStack.Push(currentExpr);
	//         return Parse_Operators(tokenList, index + 1, semanticStack);
	//         // TODO: Do the folding of objects (operators and their values) on the semantic stack so that no operator usage is incomplete.  (?)
	//     }
	// 	// #endif

	//     private object ParserDriver(List<Token> tokenList, bool parse)
	//     {

	//         if (tokenList.Count < 3 ||
	//             tokenList[tokenList.Count - 2].TokenType != TokenType.T_Dot ||
	//             tokenList[tokenList.Count - 1].TokenType != TokenType.T_EOF)
	//         {
	//             throw new Exception("The token list is too short, or it does not end with a dot and an EOF.");
	//         }

	//         object parseResult = Parse_Operators(tokenList.Take(tokenList.Count - 2).ToList());

	//         if (parseResult == null)
	//         {
	//             //throw new Exception("Syntax error: Parse_Operators() returned null.");
	//             throw new SyntaxException(string.Format("Syntax error: Parse_Operators() returned null; token list: {0}",
	//                 string.Join(" ", tokenList.Take(tokenList.Count - 1).Select(t => t.TokenValue)))); // Trim off the EOF.
	//         }

	//         var inputAsFunctorExpression = parseResult as PrologNameExpression<PrologFunctor>;
	//         var inputAsVariable = parseResult as PrologVariable;

	//         if (inputAsVariable != null)
	//         {
	//             // Clause without RHS.
	//             parseResult = new PrologClause(
	//                 new PrologGoal(gs, new PrologPredicate(inputAsVariable.Name), new List<IPrologExpression>()),
	//                 new List<PrologGoal>());
	//         }
	//         else if (inputAsFunctorExpression != null)
	//         {

	//             if (inputAsFunctorExpression.Name.Name == "-->" && inputAsFunctorExpression.ExpressionList.Count == 2)
	//             {
	//                 // Definite Clause Grammar production rule.
	//                 var lhs = ConvertToFunctorExpression(inputAsFunctorExpression.ExpressionList[0]);

	//                 inputAsFunctorExpression = PrologGrammar2_LL1.GenerateDCGClause(lhs,
	//                     CommaSeparatedListToCSharpList(inputAsFunctorExpression.ExpressionList[1]));
	//                 // The result is a clause in the form of a functor expression.
	//             }

	//             parseResult = CreateClause(inputAsFunctorExpression);

	//             if (parseResult != null)
	//             {
	//             }
	//             else if (inputAsFunctorExpression.Name.Name == "?-" && inputAsFunctorExpression.ExpressionList.Count == 1)
	//             {
	//                 // Query.
	//                 parseResult = CommaSeparatedListToCSharpList(inputAsFunctorExpression.ExpressionList[0])
	//                     .Select(expr => ConvertToFunctorExpression(expr).ToGoal())
	//                     .ToList();
	//             }
	//             else if (inputAsFunctorExpression.Name.Name == ":-" && inputAsFunctorExpression.ExpressionList.Count == 1)
	//             {
	// 				// #if DEAD_CODE
	// 				// return ProcessCommand(inputAsFunctorExpression.ExpressionList[0], ref currentModuleName);
	// 				// #else
	//                 // Leave it as a functor expression; it will be detected and processed later.
	//                 parseResult = inputAsFunctorExpression;
	// 				// #endif
	//             }
	//             else
	//             {
	//                 // Clause without RHS.
	//                 parseResult = new PrologClause(
	//                     new PrologGoal(gs, new PrologPredicate(inputAsFunctorExpression.Name.Name), inputAsFunctorExpression.ExpressionList),
	//                     new List<PrologGoal>());
	//             }
	//         }

	//         return parseResult;
	//     }

	//     public void Recognize(List<Token> tokenList)
	//     {
	//         ParserDriver(tokenList, false);
	//     }

	//     public object Parse(List<Token> tokenList)
	//     {
	//         return ParserDriver(tokenList, true);
	//     }

	// public ProcessTokenList(tokenList: Token[], ref string currentModuleName): string {
	// public ProcessTokenList(tokenList: Token[], currentModuleName: string): string {
	// 	// #if SUPPORT_USER_DEFINED_OPERATORS

	// 	// if (gs == LanguageSelector.Prolog2)
	// 	// {
	// 	// #if DEAD_CODE
	// 	// if (tokenList.Count < 3 ||
	// 	//     tokenList[tokenList.Count - 2].TokenType != TokenType.T_Dot ||
	// 	//     tokenList[tokenList.Count - 1].TokenType != TokenType.T_EOF)
	// 	// {
	// 	//     throw new Exception("The token list is too short, or it does not end with a dot and an EOF.");
	// 	// }

	// 	// var parseResult = Parse_Operators(tokenList.Take(tokenList.Count - 2).ToList());

	// 	// if (parseResult == null)
	// 	// {
	// 	//     throw new Exception("Syntax error: Parse_Operators() returned null.");
	// 	// }

	// 	// return ProcessInput(parseResult, ref currentModuleName);
	// 	// #else
	// 	// return ProcessInput(Parse(tokenList), ref currentModuleName);
	// 	// #endif
	// 	// }
	// 	// else
	// 	// {
	// 	// return ProcessInput(parser.Parse(tokenList), ref currentModuleName);
	// 	// }
	// 	// #else
	// 	// return ProcessInput(parser.Parse(tokenList));
	// 	// #endif

	// 	return this.ProcessInput(this.parser.Parse(tokenList), currentModuleName);
	// }

	// public ProcessInputString(input: string):  string {
	// 	// string currentModuleName = string.Empty;
	// 	const currentModuleName = '';

	// 	// return ProcessTokenList(tokenizer.Tokenize(input), ref currentModuleName);
	// 	return this.ProcessTokenList(this.tokenizer.Tokenize(input), currentModuleName);
	// }

	//     public bool SetScoping(bool dynamicScoping)
	//     {
	//         return false;
	//     }

	//     public bool SetDebug(bool debug)
	//     {
	//         return false;
	//     }

	// eslint-disable-next-line @typescript-eslint/no-unused-vars
	private FindModule(filePath: string): PrologModule {
		// if (dictModules.ContainsKey(filePath)) {
		// 	return dictModules[filePath];
		// }

		return this.DefaultModule;
	}
}
