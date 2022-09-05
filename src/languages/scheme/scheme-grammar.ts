// tom-weatherhead/thaw-grammar/src/languages/scheme/scheme-grammar.ts

// On macOS, to install a standard Scheme: $ brew install mit-scheme

// From the book (Kamin) :

// BracketedExpression -> Expression ExpressionList
// Value-Op -> primop?
// Value-Op -> closure?
// Value -> ( lambda ArgList Expression )
// Value -> Value-Op
// BracketedExpression -> LetKeyword ( VarExprList ) Expression
// LetKeyword -> let
// LetKeyword -> let*
// LetKeyword -> letrec
// VarExprList -> ( Variable Expression ) VarExprList
// VarExprList -> Lambda
// BracketedExpression -> call/cc Expression

import { GrammarSymbol, IToken, LexicalState, SemanticStackType } from 'thaw-interpreter-types';

import { ArgumentException, createProduction, Name } from 'thaw-interpreter-core';

import { IExpression } from '../../common/domain-object-model/iexpression';
import { IVariable, Variable } from '../../common/domain-object-model/variable';

import { BeginUsage } from '../../common/domain-object-model/begin-usage';
import { CondUsage } from '../../common/domain-object-model/cond-usage';
import { IfUsage } from '../../common/domain-object-model/if-usage';
import { LetStarUsage } from '../../common/domain-object-model/let-star-usage';
import { LetUsage } from '../../common/domain-object-model/let-usage';
import { SetUsage } from '../../common/domain-object-model/set-usage';
import { WhileUsage } from '../../common/domain-object-model/while-usage';

import { GrammarBase, GrammarException } from 'thaw-interpreter-core';

// import { FloatLiteral } from '../lisp/domain-object-model/float-literal';
import { IntegerLiteral } from '../lisp/domain-object-model/integer-literal';
import { ISExpression } from '../lisp/domain-object-model/isexpression';
import { LISPString } from '../lisp/domain-object-model/lisp-string';
import { LISPSymbol } from '../lisp/domain-object-model/lisp-symbol';
import { MacroDefinition } from '../lisp/domain-object-model/macro-definition';
import { NullSExpression } from '../lisp/domain-object-model/null-sexpression';
import { QuotedConstantWithApostrophe } from '../lisp/domain-object-model/quoted-constant-with-apostrophe';
import { QuotedConstantWithQuoteKeyword } from '../lisp/domain-object-model/quoted-constant-with-quote-keyword';
import { SExpressionList } from '../lisp/domain-object-model/sexpression-list';

import { CallCCUsage } from './domain-object-model/call-cc-usage';
// import { Closure } from './domain-object-model/closure';
// import { Continuation } from './domain-object-model/continuation';
import { EvaluableExpression } from './domain-object-model/evaluable-expression';
import { LambdaExpression } from './domain-object-model/lambda-expression';
import { LetRecUsage } from './domain-object-model/let-rec-usage';
import { PrimOp } from './domain-object-model/primitive-operator';

export class SchemeGrammar extends GrammarBase {
	// The Scheme grammar from Kamin (the book 'Programming Languages: An Interpreter-Based Approach')

	constructor(isSASL = false) {
		super(GrammarSymbol.nonterminalStart);

		// Start with the LISP grammar.

		// Then remove some stuff:

		// Terminals.Remove(Symbol.T_Define);
		// NonTerminals.Remove(Symbol.N_FunDef);
		// NonTerminals.Remove(Symbol.N_Optr);
		// RemoveProductionsContainingSymbol(Symbol.N_FunDef);
		// RemoveProductionsContainingSymbol(Symbol.N_Optr);

		// Then add the Scheme-specific stuff:

		// Terminals.UnionWith(new HashSet<Symbol>() {
		//     Symbol.T_PrimOpPred, Symbol.T_ClosurePred, Symbol.T_LambdaKeyword, Symbol.T_LetRec,
		//     Symbol.T_CallCC });

		// Productions.Add(createProduction(Symbol.N_BracketedExpression, new List<object>() { Symbol.N_Expression, Symbol.N_ExpressionList, "#evaluableExpression" }, 63));
		// Productions.Add(createProduction(Symbol.N_ValueOp, new List<object>() { Symbol.T_PrimOpPred }, 64));
		// Productions.Add(createProduction(Symbol.N_ValueOp, new List<object>() { Symbol.T_ClosurePred }, 65));
		// Productions.Add(createProduction(Symbol.N_Value, new List<object>() { Symbol.T_LeftBracket, Symbol.T_LambdaKeyword, Symbol.N_ArgList, Symbol.N_Expression, Symbol.T_RightBracket, "#lambdaExpression" }, 66));
		// Productions.Add(createProduction(Symbol.N_Value, new List<object>() { Symbol.N_ValueOp, "#valueOp" }, 67));
		// Productions.Add(createProduction(Symbol.N_LetKeyword, new List<object>() { Symbol.T_LetRec }, 68));
		// Productions.Add(createProduction(Symbol.N_BracketedExpression, new List<object>() { Symbol.T_CallCC, Symbol.N_Expression, "#call/cc" }, 69));

		this.terminals.push(GrammarSymbol.terminalLeftBracket);
		this.terminals.push(GrammarSymbol.terminalRightBracket);
		// this.terminals.push(GrammarSymbol.terminalDefine); // Used for LISP; removed for Scheme.
		this.terminals.push(GrammarSymbol.terminalIf);

		this.terminals.push(GrammarSymbol.terminalSet);
		this.terminals.push(GrammarSymbol.terminalPlus);
		this.terminals.push(GrammarSymbol.terminalMinus);
		this.terminals.push(GrammarSymbol.terminalMultiply);
		this.terminals.push(GrammarSymbol.terminalDivide);
		this.terminals.push(GrammarSymbol.terminalEquals);
		this.terminals.push(GrammarSymbol.terminalLessThan);
		// this.terminals.push(GrammarSymbol.terminalGreaterThan);
		this.terminals.push(GrammarSymbol.terminalID);
		this.terminals.push(GrammarSymbol.terminalIntegerLiteral);
		// this.terminals.push(GrammarSymbol.terminalFloatLiteral);
		this.terminals.push(GrammarSymbol.terminalCons);
		this.terminals.push(GrammarSymbol.terminalCar);
		this.terminals.push(GrammarSymbol.terminalCdr);
		this.terminals.push(GrammarSymbol.terminalNumberPred);
		this.terminals.push(GrammarSymbol.terminalSymbolPred);
		this.terminals.push(GrammarSymbol.terminalListPred);
		this.terminals.push(GrammarSymbol.terminalNullPred);
		this.terminals.push(GrammarSymbol.terminalApostrophe);
		this.terminals.push(GrammarSymbol.terminalDot);
		this.terminals.push(GrammarSymbol.terminalList);
		this.terminals.push(GrammarSymbol.terminalCond);
		this.terminals.push(GrammarSymbol.terminalStringPred);
		this.terminals.push(GrammarSymbol.terminalFloor);
		this.terminals.push(GrammarSymbol.terminalRandom);
		this.terminals.push(GrammarSymbol.terminalLet);
		this.terminals.push(GrammarSymbol.terminalLetStar);
		this.terminals.push(GrammarSymbol.terminalLetRec);
		this.terminals.push(GrammarSymbol.terminalQuoteKeyword);
		this.terminals.push(GrammarSymbol.terminalStringLiteral);
		// this.terminals.push(Symbol.terminalToString);
		// this.terminals.push(Symbol.terminalListToString);
		// this.terminals.push(Symbol.terminalStringToList);
		// this.terminals.push(Symbol.terminalStringToSymbol);
		// this.terminals.push(Symbol.terminalStringLessThan);
		// this.terminals.push(Symbol.terminalPow);
		// this.terminals.push(Symbol.terminalExp);
		// this.terminals.push(Symbol.terminalLn);
		// this.terminals.push(Symbol.terminalSin);
		// this.terminals.push(Symbol.terminalCos);
		// this.terminals.push(Symbol.terminalTan);
		// this.terminals.push(Symbol.terminalAtan2);
		// this.terminals.push(Symbol.terminalThrow);

		this.terminals.push(GrammarSymbol.terminalPrimOpPred); // Added for Scheme.
		this.terminals.push(GrammarSymbol.terminalClosurePred); // Added for Scheme.
		this.terminals.push(GrammarSymbol.terminalLambdaKeyword); // Added for Scheme.
		// this.terminals.push(GrammarSymbol.terminalLetRec); // Added for Scheme.
		this.terminals.push(GrammarSymbol.terminalCallCC); // Added for Scheme.

		this.terminals.push(GrammarSymbol.terminalEOF);

		if (!isSASL) {
			this.terminals.push(GrammarSymbol.terminalWhile);
			this.terminals.push(GrammarSymbol.terminalBegin);
			this.terminals.push(GrammarSymbol.terminalPrint);
			this.terminals.push(GrammarSymbol.terminalRplaca);
			this.terminals.push(GrammarSymbol.terminalRplacd);
			this.terminals.push(GrammarSymbol.terminalDefineMacro);
		}

		this.nonTerminals.push(GrammarSymbol.nonterminalStart);
		this.nonTerminals.push(GrammarSymbol.nonterminalInput);
		this.nonTerminals.push(GrammarSymbol.nonterminalExpression);
		// this.nonTerminals.push(GrammarSymbol.nonterminalFunDef); // Used for LISP; removed for Scheme.
		this.nonTerminals.push(GrammarSymbol.nonterminalFunction);
		this.nonTerminals.push(GrammarSymbol.nonterminalArgList);
		this.nonTerminals.push(GrammarSymbol.nonterminalVariableList);
		this.nonTerminals.push(GrammarSymbol.nonterminalVariable);
		this.nonTerminals.push(GrammarSymbol.nonterminalValue);
		this.nonTerminals.push(GrammarSymbol.nonterminalBracketedExpression);
		this.nonTerminals.push(GrammarSymbol.nonterminalExpressionList);
		// this.nonTerminals.push(GrammarSymbol.nonterminalOptr); // Used for LISP; removed for Scheme.
		this.nonTerminals.push(GrammarSymbol.nonterminalValueOp);
		this.nonTerminals.push(GrammarSymbol.nonterminalQuotedConst);
		this.nonTerminals.push(GrammarSymbol.nonterminalSExpression);
		this.nonTerminals.push(GrammarSymbol.nonterminalSExpressionList);
		this.nonTerminals.push(GrammarSymbol.nonterminalSExpressionListTail);
		this.nonTerminals.push(GrammarSymbol.nonterminalSymbol);
		this.nonTerminals.push(GrammarSymbol.nonterminalExprPairList);
		this.nonTerminals.push(GrammarSymbol.nonterminalLetKeyword);
		this.nonTerminals.push(GrammarSymbol.nonterminalVarExprList);

		// this.nonTerminals.push(GrammarSymbol.nonterminalBracketedEntity);
		// this.nonTerminals.push(GrammarSymbol.nonterminalBracketedInput);
		// this.nonTerminals.push(GrammarSymbol.nonterminalUnbracketedInput);

		// if (!isSASL) {
		// 	this.nonTerminals.push(GrammarSymbol.nonterminalMacroDef);
		// }

		// This initial production needed to be added: Start -> Input EOF
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalStart,
				[GrammarSymbol.nonterminalInput, GrammarSymbol.terminalEOF],
				1
			)
		);

		/* Old: Uses nonterminalBracketedInput and nonterminalUnbracketedInput
		// Input -> ( BracketedInput )
		this.productions.push(createProduction(GrammarSymbol.nonterminalInput,
			[
				GrammarSymbol.terminalLeftBracket,
				GrammarSymbol.nonterminalBracketedInput,
				GrammarSymbol.terminalRightBracket
			], 2));

		// Input -> UnbracketedInput
		this.productions.push(createProduction(GrammarSymbol.nonterminalInput,
			[
				GrammarSymbol.nonterminalUnbracketedInput
			], 3));

		// BracketedInput -> BracketedExpression
		this.productions.push(createProduction(GrammarSymbol.nonterminalBracketedInput,
			[
				GrammarSymbol.nonterminalBracketedExpression
			], 4));

		// BracketedInput -> FunDef // Used for LISP; removed for Scheme.
		// this.productions.push(createProduction(GrammarSymbol.nonterminalBracketedInput,
		// 	[
		// 		GrammarSymbol.nonterminalFunDef
		// 	], 5));

		// - UnbracketedInput -> Value
		this.productions.push(createProduction(GrammarSymbol.nonterminalUnbracketedInput,
			[
				GrammarSymbol.nonterminalValue
			], 6));

		// - UnbracketedInput -> Variable
		this.productions.push(createProduction(GrammarSymbol.nonterminalUnbracketedInput,
			[
				GrammarSymbol.nonterminalVariable
			], 7));
		 */

		// BEGIN: New: Does not use nonterminalBracketedInput and nonterminalUnbracketedInput

		// Input -> Expression
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalInput,
				[GrammarSymbol.nonterminalExpression],
				2
			)
		);

		// Expression -> Value
		// this.productions.push(createProduction(GrammarSymbol.nonterminalExpression,
		// 	[
		// 		GrammarSymbol.nonterminalValue
		// 	], 3));

		// Expression -> Variable
		// this.productions.push(createProduction(GrammarSymbol.nonterminalExpression,
		// 	[
		// 		GrammarSymbol.nonterminalVariable
		// 	], 4));

		// Expression -> ( BracketedExpression )
		// this.productions.push(createProduction(GrammarSymbol.nonterminalExpression,
		// 	[
		// 		GrammarSymbol.terminalLeftBracket,
		// 		GrammarSymbol.nonterminalBracketedExpression,
		// 		GrammarSymbol.terminalRightBracket
		// 	], 5));

		// END: New: Does not use nonterminalBracketedInput and nonterminalUnbracketedInput

		// FunDef -> define Function ArgList Expression // Used for LISP; removed for Scheme.
		// this.productions.push(createProduction(GrammarSymbol.nonterminalFunDef,
		// 	[
		// 		GrammarSymbol.terminalDefine,
		// 		GrammarSymbol.nonterminalFunction,
		// 		GrammarSymbol.nonterminalArgList,
		// 		GrammarSymbol.nonterminalExpression,
		// 		'#functionDefinition'
		// 	], 8));

		// ArgList -> ( VariableList )
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalArgList,
				[
					GrammarSymbol.terminalLeftBracket,
					GrammarSymbol.nonterminalVariableList,
					GrammarSymbol.terminalRightBracket
				],
				9
			)
		);

		// VariableList -> Variable VariableList
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalVariableList,
				[
					GrammarSymbol.nonterminalVariable,
					GrammarSymbol.nonterminalVariableList,
					'#variableList'
				],
				10
			)
		);

		// VariableList -> Lambda
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalVariableList,
				[GrammarSymbol.Lambda, '#emptyVariableList'],
				11
			)
		);

		// Expression -> Value
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalExpression,
				[GrammarSymbol.nonterminalValue],
				12
			)
		);

		// Expression -> Variable
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalExpression,
				[GrammarSymbol.nonterminalVariable],
				13
			)
		);

		// Expression -> ( BracketedExpression )
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalExpression,
				[
					GrammarSymbol.terminalLeftBracket,
					GrammarSymbol.nonterminalBracketedExpression,
					GrammarSymbol.terminalRightBracket
				],
				14
			)
		);

		// BracketedExpression -> set Variable Expression
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalBracketedExpression,
				[
					GrammarSymbol.terminalSet,
					GrammarSymbol.nonterminalVariable,
					GrammarSymbol.nonterminalExpression,
					'#set'
				],
				17
			)
		);

		// BracketedExpression -> Optr ExpressionList // Used for LISP; removed for Scheme.
		// this.productions.push(createProduction(GrammarSymbol.nonterminalBracketedExpression,
		// 	[
		// 		GrammarSymbol.nonterminalOptr,
		// 		GrammarSymbol.nonterminalExpressionList,
		// 		'#operatorUsage'
		// 	], 19));

		// ExpressionList -> Expression ExpressionList
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalExpressionList,
				[
					GrammarSymbol.nonterminalExpression,
					GrammarSymbol.nonterminalExpressionList,
					'#expressionList'
				],
				20
			)
		);

		// ExpressionList -> Lambda
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalExpressionList,
				[GrammarSymbol.Lambda, '#emptyExpressionList'],
				21
			)
		);

		// Optr -> Function // Used for LISP; removed for Scheme.
		// this.productions.push(createProduction(GrammarSymbol.nonterminalOptr,
		// 	[
		// 		GrammarSymbol.nonterminalFunction
		// 	], 22));

		// Optr -> Value-Op // Used for LISP; removed for Scheme.
		// this.productions.push(createProduction(GrammarSymbol.nonterminalOptr,
		// 	[
		// 		GrammarSymbol.nonterminalValueOp
		// 	], 23));

		// Value -> Integer
		// this.productions.push(
		// 	createProduction(
		// 		GrammarSymbol.nonterminalValue,
		// 		[GrammarSymbol.terminalIntegerLiteral],
		// 		24
		// 	)
		// );

		this.addProduction(GrammarSymbol.nonterminalValue, [GrammarSymbol.terminalIntegerLiteral]);

		this.addProduction(GrammarSymbol.nonterminalValue, [GrammarSymbol.terminalStringLiteral]);

		// Value-Op -> +
		this.productions.push(
			createProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalPlus], 25)
		);

		// Value-Op -> -
		this.productions.push(
			createProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalMinus], 26)
		);

		// Value-Op -> *
		this.productions.push(
			createProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalMultiply], 27)
		);

		// Value-Op -> /
		this.productions.push(
			createProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalDivide], 28)
		);

		// Value-Op -> =
		this.productions.push(
			createProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalEquals], 29)
		);

		// Value-Op -> <
		this.productions.push(
			createProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalLessThan], 30)
		);

		// Value-Op -> >
		// this.productions.push(
		// 	createProduction(
		// 		GrammarSymbol.nonterminalValueOp,
		// 		[GrammarSymbol.terminalGreaterThan],
		// 		31
		// 	)
		// );

		this.addProduction(GrammarSymbol.nonterminalValueOp, [
			GrammarSymbol.terminalStringLessThan
		]);

		// Function -> Name
		this.productions.push(
			createProduction(GrammarSymbol.nonterminalFunction, [GrammarSymbol.terminalID], 33)
		);

		// Variable -> Name
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalVariable,
				[GrammarSymbol.terminalID, '#variable'],
				34
			)
		);

		// Value -> Quoted-Const
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalValue,
				[GrammarSymbol.nonterminalQuotedConst],
				35
			)
		);

		// Value-Op -> cons
		this.productions.push(
			createProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalCons], 36)
		);

		// Value-Op -> car
		this.productions.push(
			createProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalCar], 37)
		);

		// Value-Op -> cdr
		this.productions.push(
			createProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalCdr], 38)
		);

		// Value-Op -> number?
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalValueOp,
				[GrammarSymbol.terminalNumberPred],
				39
			)
		);

		// Value-Op -> GrammarSymbol?
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalValueOp,
				[GrammarSymbol.terminalSymbolPred],
				40
			)
		);

		// Value-Op -> list?
		this.productions.push(
			createProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalListPred], 41)
		);

		// Value-Op -> null?
		this.productions.push(
			createProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalNullPred], 42)
		);

		// Quoted-Const -> ' S-Expression
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalQuotedConst,
				[
					GrammarSymbol.terminalApostrophe,
					GrammarSymbol.nonterminalSExpression,
					'#quotedConstantWithApostrophe'
				],
				43
			)
		);

		// S-Expression -> Integer
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalSExpression,
				[GrammarSymbol.terminalIntegerLiteral],
				44
			)
		);

		// S-Expression -> Symbol
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalSExpression,
				[GrammarSymbol.nonterminalSymbol],
				45
			)
		);

		// S-Expression -> ( S-Expression-List )
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalSExpression,
				[
					GrammarSymbol.terminalLeftBracket,
					GrammarSymbol.nonterminalSExpressionList,
					GrammarSymbol.terminalRightBracket
				],
				46
			)
		);

		// TODO: 2020-01-08: Replace:
		// S-Expression -> ( S-Expression-List )
		// ...with:
		// S-Expression -> ( Bracketed-S-Expression )
		// Bracketed-S-Expression -> S-Expression-List
		// Bracketed-S-Expression -> quote S-Expression #quotedConstantWithQuoteKeyword

		// Q: In what situations do we need to use the quote keyword?

		// **** BEGIN Old ****
		// S-Expression-List -> S-Expression S-Expression-List
		// this.productions.push(createProduction(Symbol.nonterminalSExpressionList, [Symbol.nonterminalSExpression, Symbol.nonterminalSExpressionList, '#sExpressionList'], 51));

		// S-Expression-List -> S-Expression . S-Expression
		// this.productions.push(createProduction(Symbol.nonterminalSExpressionList, [Symbol.nonterminalSExpression, Symbol.terminalDot, Symbol.nonterminalSExpression, '#sExpressionList'], 52));
		// **** END Old ****

		// **** BEGIN New ****

		// S-Expression-List -> S-Expression S-Expression-List-Tail
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalSExpressionList,
				[
					GrammarSymbol.nonterminalSExpression,
					GrammarSymbol.nonterminalSExpressionListTail,
					'#sExpressionList'
				],
				47
			)
		);

		// S-Expression-List-Tail -> S-Expression-List
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalSExpressionListTail,
				[GrammarSymbol.nonterminalSExpressionList],
				48
			)
		);

		// S-Expression-List-Tail -> . S-Expression
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalSExpressionListTail,
				[GrammarSymbol.terminalDot, GrammarSymbol.nonterminalSExpression],
				49
			)
		);
		// **** END New ****

		// S-Expression-List -> Lambda
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalSExpressionList,
				[GrammarSymbol.Lambda, '#emptySExpressionList'],
				50
			)
		);

		// Symbol -> Name
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalSymbol,
				[GrammarSymbol.terminalID, '#symbol'],
				51
			)
		);

		// Value-Op -> list
		this.productions.push(
			createProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalList], 55)
		);

		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalValueOp,
				[GrammarSymbol.terminalStringPred],
				56
			)
		);
		this.productions.push(
			createProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalFloor], 57)
		);
		this.productions.push(
			createProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalRandom], 58)
		);
		// this.productions.push(createProduction(Symbol.nonterminalValue, [Symbol.terminalFloatLiteral], 60)); // Note: Number is out of order

		this.addProduction(GrammarSymbol.nonterminalBracketedExpression, [
			GrammarSymbol.nonterminalLetKeyword,
			GrammarSymbol.terminalLeftBracket,
			GrammarSymbol.nonterminalVarExprList,
			GrammarSymbol.terminalRightBracket,
			GrammarSymbol.nonterminalExpression,
			'#letUsage'
		]);
		this.addProduction(GrammarSymbol.nonterminalLetKeyword, [GrammarSymbol.terminalLet]);
		this.addProduction(GrammarSymbol.nonterminalLetKeyword, [GrammarSymbol.terminalLetStar]);
		this.addProduction(GrammarSymbol.nonterminalLetKeyword, [GrammarSymbol.terminalLetRec]);
		this.addProduction(GrammarSymbol.nonterminalVarExprList, [
			GrammarSymbol.terminalLeftBracket,
			GrammarSymbol.nonterminalVariable,
			GrammarSymbol.nonterminalExpression,
			GrammarSymbol.terminalRightBracket,
			GrammarSymbol.nonterminalVarExprList,
			'#varExprList'
		]);
		this.addProduction(GrammarSymbol.nonterminalVarExprList, [
			GrammarSymbol.Lambda,
			'#emptyVarExprList'
		]);

		// this.productions.push(createProduction(
		// 	Symbol.nonterminalSExpression, // TODO? : Create Symbol.nonterminalBracketedSExpression ?
		// 	[
		// 		Symbol.terminalLeftBracket,
		// 		Symbol.terminalQuoteKeyword,
		// 		Symbol.nonterminalSExpression,
		// 		Symbol.terminalRightBracket,
		// 		'#quotedConstantWithQuoteKeyword'
		// 	], n));

		// Quoted-Const -> ' S-Expression
		// this.productions.push(createProduction(Symbol.nonterminalQuotedConst,
		// 	[
		// 		Symbol.terminalLeftBracket,
		// 		Symbol.terminalQuoteKeyword,
		// 		Symbol.nonterminalSExpression,
		// 		Symbol.terminalRightBracket,
		// 		'#quotedConstantWithQuoteKeyword'
		// 	], 65));

		// BracketedExpression -> begin Expression ExpressionList
		// this.productions.push(createProduction(Symbol.nonterminalBracketedExpression,
		// this.productions.push(createProduction(Symbol.nonterminalSExpressionList, // Real HACK.
		// 	[
		// 		Symbol.terminalQuoteKeyword,
		// 		Symbol.nonterminalSExpression,
		// 		'#quotedConstantWithQuoteKeyword'
		// 	], 65));

		// **** BEGIN : Ignore for now ****

		// Productions.Add(createProduction(Symbol.N_Value, new List<object>() { Symbol.T_StringLiteral }, 75)); // Note: Number is out of order
		// Productions.Add(createProduction(Symbol.N_Value, new List<object>() { Symbol.T_FloatLiteral }, 79)); // Note: Number is out of order

		// this.productions.push(createProduction(Symbol.nonterminalValue, [Symbol.terminalStringLiteral], 75)); // Note: Number is out of order

		// this.productions.push(createProduction(Symbol.nonterminalValueOp, [Symbol.terminalThrow], 34));
		// this.productions.push(createProduction(Symbol.nonterminalValueOp, [Symbol.terminalToString], 76));
		// this.productions.push(createProduction(Symbol.nonterminalValueOp, [Symbol.terminalListToString], 77));
		// this.productions.push(createProduction(Symbol.nonterminalValueOp, [Symbol.terminalStringToList], 78));
		// this.productions.push(createProduction(Symbol.nonterminalValueOp, [Symbol.terminalStringToSymbol], 81));
		// this.productions.push(createProduction(Symbol.nonterminalValueOp, [Symbol.terminalPow], 88));
		// this.productions.push(createProduction(Symbol.nonterminalValueOp, [Symbol.terminalExp], 82));
		// this.productions.push(createProduction(Symbol.nonterminalValueOp, [Symbol.terminalLn], 83));
		// this.productions.push(createProduction(Symbol.nonterminalValueOp, [Symbol.terminalSin], 84));
		// this.productions.push(createProduction(Symbol.nonterminalValueOp, [Symbol.terminalCos], 85));
		// this.productions.push(createProduction(Symbol.nonterminalValueOp, [Symbol.terminalTan], 86));
		// this.productions.push(createProduction(Symbol.nonterminalValueOp, [Symbol.terminalAtan2], 89));
		// this.productions.push(createProduction(Symbol.nonterminalValueOp, [Symbol.terminalStringLessThan], 91));
		// **** END : Ignore for now ****

		// Next production number is 56.

		// Added for Scheme:

		// BracketedExpression -> Expression ExpressionList
		// Productions.Add(createProduction(Symbol.N_BracketedExpression, new List<object>() { Symbol.N_Expression, Symbol.N_ExpressionList, "#evaluableExpression" }, 63));
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalBracketedExpression,
				[
					GrammarSymbol.nonterminalExpression,
					GrammarSymbol.nonterminalExpressionList,
					'#evaluableExpression'
				],
				101
			)
		);

		// Value-Op -> primop?
		// Productions.Add(createProduction(Symbol.N_ValueOp, new List<object>() { Symbol.T_PrimOpPred }, 64));
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalValueOp,
				[GrammarSymbol.terminalPrimOpPred],
				102
			)
		);

		// Value-Op -> closure?
		// Productions.Add(createProduction(Symbol.N_ValueOp, new List<object>() { Symbol.T_ClosurePred }, 65));
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalValueOp,
				[GrammarSymbol.terminalClosurePred],
				103
			)
		);

		// Value -> ( lambda ArgList Expression )
		// Productions.Add(createProduction(Symbol.N_Value, new List<object>() { Symbol.T_LeftBracket, Symbol.T_LambdaKeyword, Symbol.N_ArgList, Symbol.N_Expression, Symbol.T_RightBracket, "#lambdaExpression" }, 66));
		// this.productions.push(createProduction(Symbol.nonterminalValue,
		// 	[
		// 		Symbol.terminalLeftBracket,
		// 		Symbol.terminalLambdaKeyword,
		// 		Symbol.nonterminalArgList,
		// 		Symbol.nonterminalExpression,
		// 		Symbol.terminalRightBracket,
		// 		'#lambdaExpression'
		// 	], 104));
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalBracketedExpression,
				[
					// GrammarSymbol.terminalLeftBracket,
					GrammarSymbol.terminalLambdaKeyword,
					GrammarSymbol.nonterminalArgList,
					GrammarSymbol.nonterminalExpression,
					// GrammarSymbol.terminalRightBracket,
					'#lambdaExpression'
				],
				104
			)
		);

		// Value -> Value-Op
		// Productions.Add(createProduction(Symbol.N_Value, new List<object>() { Symbol.N_ValueOp, "#valueOp" }, 67));
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalValue,
				[GrammarSymbol.nonterminalValueOp, '#valueOp'],
				105
			)
		);

		// BracketedExpression -> LetKeyword ( VarExprList ) Expression
		// LetKeyword -> let
		// LetKeyword -> let*
		// LetKeyword -> letrec
		// VarExprList -> ( Variable Expression ) VarExprList
		// VarExprList -> Lambda
		// Productions.Add(createProduction(Symbol.N_LetKeyword, new List<object>() { Symbol.T_LetRec }, 68));
		// this.productions.push(createProduction(Symbol.nonterminalLetKeyword,
		// 	[
		// 		Symbol.terminalLetRec
		// 	], 106));

		// BracketedExpression -> call/cc Expression
		// Productions.Add(createProduction(Symbol.N_BracketedExpression, new List<object>() { Symbol.T_CallCC, Symbol.N_Expression, "#call/cc" }, 69));
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalBracketedExpression,
				[GrammarSymbol.terminalCallCC, GrammarSymbol.nonterminalExpression, '#call/cc'],
				107
			)
		);

		if (!isSASL) {
			// BracketedExpression -> if Expression Expression Expression
			this.productions.push(
				createProduction(
					GrammarSymbol.nonterminalBracketedExpression,
					[
						GrammarSymbol.terminalIf,
						GrammarSymbol.nonterminalExpression,
						GrammarSymbol.nonterminalExpression,
						GrammarSymbol.nonterminalExpression,
						'#if'
					],
					15
				)
			);

			// BracketedExpression -> while Expression Expression
			this.productions.push(
				createProduction(
					GrammarSymbol.nonterminalBracketedExpression,
					[
						GrammarSymbol.terminalWhile,
						GrammarSymbol.nonterminalExpression,
						GrammarSymbol.nonterminalExpression,
						'#while'
					],
					16
				)
			);

			// BracketedExpression -> begin Expression ExpressionList
			this.productions.push(
				createProduction(
					GrammarSymbol.nonterminalBracketedExpression,
					[
						GrammarSymbol.terminalBegin,
						GrammarSymbol.nonterminalExpression,
						GrammarSymbol.nonterminalExpressionList,
						'#begin'
					],
					18
				)
			);

			// Value-Op -> print
			this.productions.push(
				createProduction(
					GrammarSymbol.nonterminalValueOp,
					[GrammarSymbol.terminalPrint],
					32
				)
			);

			// BracketedExpression -> cond ( Expression Expression ) ExprPairList
			this.productions.push(
				createProduction(
					GrammarSymbol.nonterminalBracketedExpression,
					[
						GrammarSymbol.terminalCond,
						GrammarSymbol.terminalLeftBracket,
						GrammarSymbol.nonterminalExpression,
						GrammarSymbol.nonterminalExpression,
						GrammarSymbol.terminalRightBracket,
						GrammarSymbol.nonterminalExprPairList,
						'#condUsage'
					],
					52
				)
			);

			// ExprPairList -> ( Expression Expression ) ExprPairList
			this.productions.push(
				createProduction(
					GrammarSymbol.nonterminalExprPairList,
					[
						GrammarSymbol.terminalLeftBracket,
						GrammarSymbol.nonterminalExpression,
						GrammarSymbol.nonterminalExpression,
						GrammarSymbol.terminalRightBracket,
						GrammarSymbol.nonterminalExprPairList,
						'#exprPairList'
					],
					53
				)
			);

			// ExprPairList -> Lambda
			this.productions.push(
				createProduction(
					GrammarSymbol.nonterminalExprPairList,
					[GrammarSymbol.Lambda, '#emptyExprPairList'],
					54
				)
			);

			this.productions.push(
				createProduction(
					GrammarSymbol.nonterminalValueOp,
					[GrammarSymbol.terminalRplaca],
					56
				)
			);
			this.productions.push(
				createProduction(
					GrammarSymbol.nonterminalValueOp,
					[GrammarSymbol.terminalRplacd],
					57
				)
			);

			// Old
			// this.productions.push(createProduction(Symbol.nonterminalInput, [Symbol.nonterminalMacroDef], 58));
			// this.productions.push(createProduction(
			// 	Symbol.nonterminalMacroDef,
			// 	[
			// 		Symbol.terminalLeftBracket,
			// 		Symbol.terminalDefineMacro,
			// 		Symbol.nonterminalFunction,
			// 		Symbol.nonterminalArgList,
			// 		Symbol.nonterminalExpression,
			// 		Symbol.terminalRightBracket,
			// 		'#macroDefinition'
			// 	],
			// 	59));

			// New
			// BracketedInput -> MacroDef
			// this.productions.push(createProduction(Symbol.nonterminalBracketedInput,
			// 	[
			// 		Symbol.nonterminalMacroDef
			// 	], n));
			// this.productions.push(createProduction(
			// 	Symbol.nonterminalMacroDef,
			// 	[
			// 		Symbol.terminalDefineMacro,
			// 		Symbol.nonterminalFunction,
			// 		Symbol.nonterminalArgList,
			// 		Symbol.nonterminalExpression,
			// 		'#macroDefinition'
			// 	], n));
			this.productions.push(
				createProduction(
					GrammarSymbol.nonterminalBracketedExpression,
					[
						GrammarSymbol.terminalDefineMacro,
						GrammarSymbol.nonterminalFunction,
						GrammarSymbol.nonterminalArgList,
						GrammarSymbol.nonterminalExpression,
						'#macroDefinition'
					],
					59
				)
			);
		}
	}

	public get languageName(): string {
		return 'Scheme';
	}

	public executeSemanticAction(semanticStack: SemanticStackType, action: string): void {
		// console.log(`SchemeGrammar.executeSemanticAction() : action is ${typeof action} ${action}`);

		let name: Name;
		let variable: IVariable<ISExpression>;
		let variableList: IVariable<ISExpression>[];
		let expression: IExpression<ISExpression>;
		let expression2: IExpression<ISExpression>;
		let expression3: IExpression<ISExpression>;
		let expressionList: IExpression<ISExpression>[];
		let sexpression: ISExpression;
		let head: ISExpression;
		let tail: ISExpression;
		let varExprList: [IVariable<ISExpression>, IExpression<ISExpression>][];
		let exprPairList: [IExpression<ISExpression>, IExpression<ISExpression>][];

		switch (action) {
			// case '#functionDefinition':
			// 	expression = semanticStack.pop() as IExpression<ISExpression>; // The function's body
			// 	variableList = semanticStack.pop() as VariableList<ISExpression>; // The function's formal argument list
			// 	name = semanticStack.pop() as Name; // The function name
			// 	semanticStack.push(new FunctionDefinition<ISExpression>(name, variableList, expression)); // Add line and column?
			// 	break;

			case '#variableList':
				variableList = semanticStack.pop() as IVariable<ISExpression>[];
				variable = semanticStack.pop() as IVariable<ISExpression>;
				variableList.unshift(variable);
				semanticStack.push(variableList);
				break;

			case '#emptyVariableList':
				semanticStack.push([] as IVariable<ISExpression>[]); // Add line and column?
				break;

			case '#if':
				expression3 = semanticStack.pop() as IExpression<ISExpression>;
				expression2 = semanticStack.pop() as IExpression<ISExpression>;
				expression = semanticStack.pop() as IExpression<ISExpression>;
				semanticStack.push(new IfUsage<ISExpression>(expression, expression2, expression3)); // Add line and column?
				break;

			case '#while':
				expression2 = semanticStack.pop() as IExpression<ISExpression>;
				expression = semanticStack.pop() as IExpression<ISExpression>;
				semanticStack.push(new WhileUsage<ISExpression>(expression, expression2)); // Add line and column?
				break;

			case '#set':
				expression = semanticStack.pop() as IExpression<ISExpression>;
				variable = semanticStack.pop() as Variable<ISExpression>;
				semanticStack.push(new SetUsage<ISExpression>(variable, expression)); // Add line and column?
				break;

			case '#begin':
				expressionList = semanticStack.pop() as IExpression<ISExpression>[];
				expression = semanticStack.pop() as IExpression<ISExpression>;
				semanticStack.push(new BeginUsage<ISExpression>(expression, expressionList)); // Add line and column?
				break;

			// case '#operatorUsage':
			// 	expressionList = semanticStack.pop() as ExpressionList<ISExpression>;
			// 	name = semanticStack.pop() as Name;
			// 	semanticStack.push(new LISPOperatorUsage(name, expressionList));
			// 	break;

			case '#expressionList':
				expressionList = semanticStack.pop() as IExpression<ISExpression>[];
				expression = semanticStack.pop() as IExpression<ISExpression>;
				expressionList.unshift(expression);
				semanticStack.push(expressionList);
				break;

			case '#emptyExpressionList':
				semanticStack.push([] as IExpression<ISExpression>[]);
				break;

			case '#variable':
				name = semanticStack.pop() as Name;
				semanticStack.push(new Variable<ISExpression>(name.value, name.line, name.column));
				break;

			case '#quotedConstantWithApostrophe':
				sexpression = semanticStack.pop() as ISExpression;
				semanticStack.push(new QuotedConstantWithApostrophe(sexpression));
				break;

			case '#quotedConstantWithQuoteKeyword':
				sexpression = semanticStack.pop() as ISExpression;
				semanticStack.push(new QuotedConstantWithQuoteKeyword(sexpression));
				break;

			case '#sExpressionList':
				tail = semanticStack.pop() as ISExpression;
				head = semanticStack.pop() as ISExpression;
				semanticStack.push(new SExpressionList(head, tail));
				break;

			case '#emptySExpressionList':
				semanticStack.push(new NullSExpression());
				break;

			case '#symbol':
				name = semanticStack.pop() as Name;
				semanticStack.push(new LISPSymbol(name.value));
				break;

			case '#condUsage':
				exprPairList = semanticStack.pop() as [
					IExpression<ISExpression>,
					IExpression<ISExpression>
				][];
				expression2 = semanticStack.pop() as IExpression<ISExpression>;
				expression = semanticStack.pop() as IExpression<ISExpression>;
				exprPairList.unshift([expression, expression2]);
				semanticStack.push(new CondUsage<ISExpression>(exprPairList));
				break;

			case '#exprPairList':
				exprPairList = semanticStack.pop() as [
					IExpression<ISExpression>,
					IExpression<ISExpression>
				][];
				expression2 = semanticStack.pop() as IExpression<ISExpression>;
				expression = semanticStack.pop() as IExpression<ISExpression>;
				exprPairList.unshift([expression, expression2]);
				semanticStack.push(exprPairList);
				break;

			case '#emptyExprPairList':
				semanticStack.push(
					new Array<[IExpression<ISExpression>, IExpression<ISExpression>]>()
				);
				break;

			case '#letUsage':
				expression = semanticStack.pop() as IExpression<ISExpression>;
				varExprList = semanticStack.pop() as [
					Variable<ISExpression>,
					IExpression<ISExpression>
				][];
				name = semanticStack.pop() as Name;
				semanticStack.push(this.createLetUsage(name, varExprList, expression));
				break;

			case '#varExprList':
				varExprList = semanticStack.pop() as [
					Variable<ISExpression>,
					IExpression<ISExpression>
				][];
				expression = semanticStack.pop() as IExpression<ISExpression>;
				variable = semanticStack.pop() as Variable<ISExpression>;
				varExprList.unshift([variable, expression]);
				semanticStack.push(varExprList);
				break;

			case '#emptyVarExprList':
				semanticStack.push(
					new Array<[Variable<ISExpression>, IExpression<ISExpression>]>()
				);
				break;

			case '#macroDefinition':
				expression = semanticStack.pop() as IExpression<ISExpression>; // macroBody
				variableList = semanticStack.pop() as IVariable<ISExpression>[]; // macroArgList
				name = semanticStack.pop() as Name; // macroName
				semanticStack.push(new MacroDefinition(name, variableList, expression));
				break;

			case '#lambdaExpression':
				const body = semanticStack.pop() as IExpression<ISExpression>;
				const argList = semanticStack.pop() as IVariable<ISExpression>[];

				semanticStack.push(new LambdaExpression(argList, body));
				break;

			case '#evaluableExpression':
				expressionList = semanticStack.pop() as IExpression<ISExpression>[];
				expression = semanticStack.pop() as IExpression<ISExpression>;
				semanticStack.push(new EvaluableExpression(expression, expressionList));
				break;

			case '#valueOp':
				name = semanticStack.pop() as Name;
				semanticStack.push(new PrimOp(name));
				break;

			case '#call/cc':
				expression = semanticStack.pop() as IExpression<ISExpression>;
				semanticStack.push(new CallCCUsage(expression));
				break;

			default:
				throw new GrammarException(`Unrecognized semantic action: ${action}`);
		}
	}

	public override tokenToSymbol(token: IToken): GrammarSymbol {
		const tokenValueAsString: string = token.tokenValue as string;

		// if (token.isQuoted && tokenValueAsString !== '.') {
		// 	// 2021-11-29 : This allows e.g. '+ to be an S-expression
		//
		// 	return GrammarSymbol.terminalID;
		// }

		if (token.isQuoted) {
			// 2021-11-29, 2022-09-05 : This allows e.g. '+ to be an S-expression

			token.tokenType = LexicalState.tokenIdent;
		}

		switch (token.tokenType) {
			case LexicalState.tokenEOF:
				return GrammarSymbol.terminalEOF;
			case LexicalState.tokenIntLit:
				return GrammarSymbol.terminalIntegerLiteral;

			// HACK version 2 : ThAW 2020-01-06 : The value in an IntegerLiteral can be int or float.
			// In the future, we will need to properly support FloatLiterals
			// and distinguish them from IntegerLiterals.
			// case LexicalState.tokenFltLit: return GrammarSymbol.terminalFloatLiteral;
			case LexicalState.tokenFltLit:
				return GrammarSymbol.terminalIntegerLiteral;

			case LexicalState.tokenStrLit:
				return GrammarSymbol.terminalStringLiteral;
			// case LexicalState.tokenIdent: return GrammarSymbol.terminalID;
			case LexicalState.tokenLeftBracket:
				return GrammarSymbol.terminalLeftBracket;
			case LexicalState.tokenRightBracket:
				return GrammarSymbol.terminalRightBracket;
			case LexicalState.tokenPlus:
				return GrammarSymbol.terminalPlus;
			case LexicalState.tokenMinus:
				return GrammarSymbol.terminalMinus;
			case LexicalState.tokenMult:
				return GrammarSymbol.terminalMultiply;
			case LexicalState.tokenDiv:
				return GrammarSymbol.terminalDivide;
			case LexicalState.tokenEqual:
				return GrammarSymbol.terminalEquals;
			case LexicalState.tokenLess:
				return GrammarSymbol.terminalLessThan;
			case LexicalState.tokenGreater:
				// return GrammarSymbol.terminalGreaterThan;
				return GrammarSymbol.terminalID;
			case LexicalState.tokenLessEqual:
				return GrammarSymbol.terminalID;
			case LexicalState.tokenGreaterEqual:
				return GrammarSymbol.terminalID;
			case LexicalState.tokenApostrophe:
				return GrammarSymbol.terminalApostrophe;
			case LexicalState.tokenQuoteKeyword:
				return GrammarSymbol.terminalQuoteKeyword;

			case LexicalState.tokenIdent:
				switch (tokenValueAsString) {
					// Quoting never changes tokens with these values into IDs.
					case '.':
						return GrammarSymbol.terminalDot; // We could modify the tokenizer to generate TokenType.T_Dot in this case, to obviate this line.
					// case 'quote': return GrammarSymbol.terminalQuoteKeyword;
					default:
						break;
				}

				if (token.isQuoted) {
					return GrammarSymbol.terminalID;
				}

				switch (tokenValueAsString) {
					case 'define':
						return GrammarSymbol.terminalDefine;
					case 'if':
						return GrammarSymbol.terminalIf;
					case 'while':
						return GrammarSymbol.terminalWhile;
					case 'set':
						return GrammarSymbol.terminalSet;
					case 'begin':
						return GrammarSymbol.terminalBegin;
					case 'print':
						return GrammarSymbol.terminalPrint;
					case 'cons':
						return GrammarSymbol.terminalCons;
					case 'car':
						return GrammarSymbol.terminalCar;
					case 'cdr':
						return GrammarSymbol.terminalCdr;
					case 'number?':
						return GrammarSymbol.terminalNumberPred;
					case 'symbol?':
						return GrammarSymbol.terminalSymbolPred;
					case 'list?':
						return GrammarSymbol.terminalListPred;
					case 'null?':
						return GrammarSymbol.terminalNullPred;
					case 'primop?':
						return GrammarSymbol.terminalPrimOpPred; // Added for Scheme
					case 'closure?':
						return GrammarSymbol.terminalClosurePred; // Added for Scheme
					case 'string?':
						return GrammarSymbol.terminalStringPred;
					case 'list':
						return GrammarSymbol.terminalList;
					case 'rplaca':
						return GrammarSymbol.terminalRplaca;
					case 'rplacd':
						return GrammarSymbol.terminalRplacd;
					case 'define-macro':
						return GrammarSymbol.terminalDefineMacro;
					case 'random':
						return GrammarSymbol.terminalRandom;
					case 'tostring':
						return GrammarSymbol.terminalToString;
					case 'listtostring':
						return GrammarSymbol.terminalListToString;
					case 'stringtolist':
						return GrammarSymbol.terminalStringToList;
					case 'stringtosymbol':
						return GrammarSymbol.terminalStringToSymbol;
					case 'pow':
						return GrammarSymbol.terminalPow;
					case 'exp':
						return GrammarSymbol.terminalExp;
					case 'ln':
						return GrammarSymbol.terminalLn;
					case 'sin':
						return GrammarSymbol.terminalSin;
					case 'cos':
						return GrammarSymbol.terminalCos;
					case 'tan':
						return GrammarSymbol.terminalTan;
					case 'atan2':
						return GrammarSymbol.terminalAtan2;
					case 'floor':
						return GrammarSymbol.terminalFloor;
					case 'throw':
						return GrammarSymbol.terminalThrow;
					case 'string<':
						return GrammarSymbol.terminalStringLessThan;
					case 'cond':
						return GrammarSymbol.terminalCond;
					case 'let':
						return GrammarSymbol.terminalLet;
					case 'let*':
						return GrammarSymbol.terminalLetStar;
					case 'letrec':
						return GrammarSymbol.terminalLetRec;
					case 'lambda':
						return GrammarSymbol.terminalLambdaKeyword; // Added for Scheme
					case 'call/cc':
						return GrammarSymbol.terminalCallCC;
					default:
						// return GrammarSymbol.terminalID;
						break;
				}

				break;

			default:
				break;
		}

		return super.tokenToSymbol(token);
	}

	public override pushTokenOntoSemanticStack(
		semanticStack: SemanticStackType,
		tokenAsSymbol: number,
		token: IToken
	): void {
		const value = token.tokenValue;

		switch (tokenAsSymbol) {
			case GrammarSymbol.terminalCons:
			case GrammarSymbol.terminalCar:
			case GrammarSymbol.terminalCdr:
			case GrammarSymbol.terminalNumberPred:
			case GrammarSymbol.terminalSymbolPred:
			case GrammarSymbol.terminalListPred:
			case GrammarSymbol.terminalNullPred:
			case GrammarSymbol.terminalPrimOpPred: // Added for Scheme
			case GrammarSymbol.terminalClosurePred: // Added for Scheme
			case GrammarSymbol.terminalStringPred:
			case GrammarSymbol.terminalList:
			case GrammarSymbol.terminalRplaca:
			case GrammarSymbol.terminalRplacd:
			case GrammarSymbol.terminalRandom:
			case GrammarSymbol.terminalToString:
			case GrammarSymbol.terminalListToString:
			case GrammarSymbol.terminalStringToList:
			case GrammarSymbol.terminalStringToSymbol:
			case GrammarSymbol.terminalAtan2:
			case GrammarSymbol.terminalFloor:
			case GrammarSymbol.terminalStringLessThan:
			case GrammarSymbol.terminalLetRec:
				semanticStack.push(new Name(value as string, token.line, token.column));
				break;

			case GrammarSymbol.terminalIntegerLiteral:
				semanticStack.push(new IntegerLiteral(value));
				break;

			// case GrammarSymbol.terminalFloatLiteral:
			// 	// semanticStack.push(new FloatLiteral(value));

			// 	// HACK ThAW 2020-01-06 : The value in an IntegerLiteral can be int or float.
			// 	// In the future, we will need to properly support FloatLiterals
			// 	// and distinguish them from IntegerLiterals.
			// 	semanticStack.push(new IntegerLiteral(value));
			// 	break;

			case GrammarSymbol.terminalStringLiteral:
				semanticStack.push(new LISPString(value as string));
				break;

			case GrammarSymbol.terminalApostrophe:
			case GrammarSymbol.terminalDefineMacro:
			case GrammarSymbol.terminalQuoteKeyword:
			case GrammarSymbol.terminalLambdaKeyword: // Added for Scheme
			case GrammarSymbol.terminalCallCC: // Added for Scheme
				break;

			default:
				super.pushTokenOntoSemanticStack(semanticStack, tokenAsSymbol, token);
				break;
		}
	}

	protected createLetUsage(
		letName: Name,
		varExprList: [IVariable<ISExpression>, IExpression<ISExpression>][],
		expression: IExpression<ISExpression>
	): IExpression<ISExpression> {
		switch (letName.value) {
			case 'let':
				return new LetUsage<ISExpression>(varExprList, expression);

			case 'let*':
				return new LetStarUsage<ISExpression>(varExprList, expression);

			case 'letrec':
				return new LetRecUsage<ISExpression>(varExprList, expression);

			default:
				throw new ArgumentException(
					`SchemeGrammar.createLetUsage() : Unknown 'let' keyword '${letName.value}.`,
					'letName',
					letName.line,
					letName.column
				);
		}
	}
}
