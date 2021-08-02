// tom-weatherhead/thaw-grammar/src/languages/scheme/scheme-grammar.ts

// From the book:

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

import { Stack } from 'thaw-common-utilities.ts';

import { LexicalState, Token } from 'thaw-lexical-analyzer';

import { ExpressionList } from '../../common/domain-object-model/expression-list';
import { IExpression } from '../../common/domain-object-model/iexpression';
import { Name } from '../../common/domain-object-model/name';
import { Variable } from '../../common/domain-object-model/variable';
import { VariableList } from '../../common/domain-object-model/variable-list';

import { BeginUsage } from '../../common/domain-object-model/begin-usage';
import { CondUsage } from '../../common/domain-object-model/cond-usage';
import { IfUsage } from '../../common/domain-object-model/if-usage';
import { LetStarUsage } from '../../common/domain-object-model/let-star-usage';
import { LetUsage } from '../../common/domain-object-model/let-usage';
import { SetUsage } from '../../common/domain-object-model/set-usage';
import { WhileUsage } from '../../common/domain-object-model/while-usage';

import { ArgumentException } from '../../common/exceptions/argument-exception';
import { GrammarException } from '../../common/exceptions/grammar-exception';

import { GrammarBase } from '../../common/grammar-base';
import { ParserSelector } from '../../common/parser-selectors';
import { Production } from '../../common/production';
import { Symbol } from '../../common/symbol';

// import { FloatLiteral } from '../lisp/domain-object-model/float-literal';
import { IntegerLiteral } from '../lisp/domain-object-model/integer-literal';
import { ISExpression } from '../lisp/domain-object-model/isexpression';
import { LISPString } from '../lisp/domain-object-model/lisp-string';
import { LISPSymbol } from '../lisp/domain-object-model/lisp-symbol';
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
		super(Symbol.nonterminalStart);

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

		// Productions.Add(new Production(Symbol.N_BracketedExpression, new List<object>() { Symbol.N_Expression, Symbol.N_ExpressionList, "#evaluableExpression" }, 63));
		// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_PrimOpPred }, 64));
		// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_ClosurePred }, 65));
		// Productions.Add(new Production(Symbol.N_Value, new List<object>() { Symbol.T_LeftBracket, Symbol.T_LambdaKeyword, Symbol.N_ArgList, Symbol.N_Expression, Symbol.T_RightBracket, "#lambdaExpression" }, 66));
		// Productions.Add(new Production(Symbol.N_Value, new List<object>() { Symbol.N_ValueOp, "#valueOp" }, 67));
		// Productions.Add(new Production(Symbol.N_LetKeyword, new List<object>() { Symbol.T_LetRec }, 68));
		// Productions.Add(new Production(Symbol.N_BracketedExpression, new List<object>() { Symbol.T_CallCC, Symbol.N_Expression, "#call/cc" }, 69));

		this.terminals.push(Symbol.terminalLeftBracket);
		this.terminals.push(Symbol.terminalRightBracket);
		// this.terminals.push(Symbol.terminalDefine); // Used for LISP; removed for Scheme.
		this.terminals.push(Symbol.terminalIf);

		this.terminals.push(Symbol.terminalSet);
		this.terminals.push(Symbol.terminalPlus);
		this.terminals.push(Symbol.terminalMinus);
		this.terminals.push(Symbol.terminalMultiply);
		this.terminals.push(Symbol.terminalDivide);
		this.terminals.push(Symbol.terminalEquals);
		this.terminals.push(Symbol.terminalLessThan);
		this.terminals.push(Symbol.terminalGreaterThan);
		this.terminals.push(Symbol.terminalID);
		this.terminals.push(Symbol.terminalIntegerLiteral);
		// this.terminals.push(Symbol.terminalFloatLiteral);
		this.terminals.push(Symbol.terminalCons);
		this.terminals.push(Symbol.terminalCar);
		this.terminals.push(Symbol.terminalCdr);
		this.terminals.push(Symbol.terminalNumberPred);
		this.terminals.push(Symbol.terminalSymbolPred);
		this.terminals.push(Symbol.terminalListPred);
		this.terminals.push(Symbol.terminalNullPred);
		this.terminals.push(Symbol.terminalApostrophe);
		this.terminals.push(Symbol.terminalDot);
		this.terminals.push(Symbol.terminalList);
		this.terminals.push(Symbol.terminalCond);
		this.terminals.push(Symbol.terminalStringPred);
		this.terminals.push(Symbol.terminalFloor);
		this.terminals.push(Symbol.terminalRandom);
		this.terminals.push(Symbol.terminalLet);
		this.terminals.push(Symbol.terminalLetStar);
		this.terminals.push(Symbol.terminalLetRec);
		this.terminals.push(Symbol.terminalQuoteKeyword);
		// this.terminals.push(Symbol.terminalStringLiteral);
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

		this.terminals.push(Symbol.terminalPrimOpPred); // Added for Scheme.
		this.terminals.push(Symbol.terminalClosurePred); // Added for Scheme.
		this.terminals.push(Symbol.terminalLambdaKeyword); // Added for Scheme.
		this.terminals.push(Symbol.terminalLetRec); // Added for Scheme.
		this.terminals.push(Symbol.terminalCallCC); // Added for Scheme.

		this.terminals.push(Symbol.terminalEOF);

		if (!isSASL) {
			this.terminals.push(Symbol.terminalWhile);
			this.terminals.push(Symbol.terminalBegin);
			this.terminals.push(Symbol.terminalPrint);
			// this.terminals.push(Symbol.terminalRplaca);
			// this.terminals.push(Symbol.terminalRplacd);
			// this.terminals.push(Symbol.terminalDefineMacro);
		}

		this.nonTerminals.push(Symbol.nonterminalStart);
		this.nonTerminals.push(Symbol.nonterminalInput);
		this.nonTerminals.push(Symbol.nonterminalExpression);
		// this.nonTerminals.push(Symbol.nonterminalFunDef); // Used for LISP; removed for Scheme.
		this.nonTerminals.push(Symbol.nonterminalFunction);
		this.nonTerminals.push(Symbol.nonterminalArgList);
		this.nonTerminals.push(Symbol.nonterminalVariableList);
		this.nonTerminals.push(Symbol.nonterminalVariable);
		this.nonTerminals.push(Symbol.nonterminalValue);
		this.nonTerminals.push(Symbol.nonterminalBracketedExpression);
		this.nonTerminals.push(Symbol.nonterminalExpressionList);
		// this.nonTerminals.push(Symbol.nonterminalOptr); // Used for LISP; removed for Scheme.
		this.nonTerminals.push(Symbol.nonterminalValueOp);
		this.nonTerminals.push(Symbol.nonterminalQuotedConst);
		this.nonTerminals.push(Symbol.nonterminalSExpression);
		this.nonTerminals.push(Symbol.nonterminalSExpressionList);
		this.nonTerminals.push(Symbol.nonterminalSExpressionListTail);
		this.nonTerminals.push(Symbol.nonterminalSymbol);
		this.nonTerminals.push(Symbol.nonterminalExprPairList);
		this.nonTerminals.push(Symbol.nonterminalLetKeyword);
		this.nonTerminals.push(Symbol.nonterminalVarExprList);

		// this.nonTerminals.push(Symbol.nonterminalBracketedEntity);
		// this.nonTerminals.push(Symbol.nonterminalBracketedInput);
		// this.nonTerminals.push(Symbol.nonterminalUnbracketedInput);

		// if (!isSASL) {
		// 	this.nonTerminals.push(Symbol.nonterminalMacroDef);
		// }

		// This initial production needed to be added: Start -> Input EOF
		this.productions.push(
			new Production(
				Symbol.nonterminalStart,
				[Symbol.nonterminalInput, Symbol.terminalEOF],
				1
			)
		);

		/* Old: Uses nonterminalBracketedInput and nonterminalUnbracketedInput
		// Input -> ( BracketedInput )
		this.productions.push(new Production(Symbol.nonterminalInput,
			[
				Symbol.terminalLeftBracket,
				Symbol.nonterminalBracketedInput,
				Symbol.terminalRightBracket
			], 2));

		// Input -> UnbracketedInput
		this.productions.push(new Production(Symbol.nonterminalInput,
			[
				Symbol.nonterminalUnbracketedInput
			], 3));

		// BracketedInput -> BracketedExpression
		this.productions.push(new Production(Symbol.nonterminalBracketedInput,
			[
				Symbol.nonterminalBracketedExpression
			], 4));

		// BracketedInput -> FunDef // Used for LISP; removed for Scheme.
		// this.productions.push(new Production(Symbol.nonterminalBracketedInput,
		// 	[
		// 		Symbol.nonterminalFunDef
		// 	], 5));

		// - UnbracketedInput -> Value
		this.productions.push(new Production(Symbol.nonterminalUnbracketedInput,
			[
				Symbol.nonterminalValue
			], 6));

		// - UnbracketedInput -> Variable
		this.productions.push(new Production(Symbol.nonterminalUnbracketedInput,
			[
				Symbol.nonterminalVariable
			], 7));
		 */

		// BEGIN: New: Does not use nonterminalBracketedInput and nonterminalUnbracketedInput

		// Input -> Expression
		this.productions.push(
			new Production(Symbol.nonterminalInput, [Symbol.nonterminalExpression], 2)
		);

		// Expression -> Value
		// this.productions.push(new Production(Symbol.nonterminalExpression,
		// 	[
		// 		Symbol.nonterminalValue
		// 	], 3));

		// Expression -> Variable
		// this.productions.push(new Production(Symbol.nonterminalExpression,
		// 	[
		// 		Symbol.nonterminalVariable
		// 	], 4));

		// Expression -> ( BracketedExpression )
		// this.productions.push(new Production(Symbol.nonterminalExpression,
		// 	[
		// 		Symbol.terminalLeftBracket,
		// 		Symbol.nonterminalBracketedExpression,
		// 		Symbol.terminalRightBracket
		// 	], 5));

		// END: New: Does not use nonterminalBracketedInput and nonterminalUnbracketedInput

		// FunDef -> define Function ArgList Expression // Used for LISP; removed for Scheme.
		// this.productions.push(new Production(Symbol.nonterminalFunDef,
		// 	[
		// 		Symbol.terminalDefine,
		// 		Symbol.nonterminalFunction,
		// 		Symbol.nonterminalArgList,
		// 		Symbol.nonterminalExpression,
		// 		'#functionDefinition'
		// 	], 8));

		// ArgList -> ( VariableList )
		this.productions.push(
			new Production(
				Symbol.nonterminalArgList,
				[
					Symbol.terminalLeftBracket,
					Symbol.nonterminalVariableList,
					Symbol.terminalRightBracket
				],
				9
			)
		);

		// VariableList -> Variable VariableList
		this.productions.push(
			new Production(
				Symbol.nonterminalVariableList,
				[Symbol.nonterminalVariable, Symbol.nonterminalVariableList, '#variableList'],
				10
			)
		);

		// VariableList -> Lambda
		this.productions.push(
			new Production(
				Symbol.nonterminalVariableList,
				[Symbol.Lambda, '#emptyVariableList'],
				11
			)
		);

		// Expression -> Value
		this.productions.push(
			new Production(Symbol.nonterminalExpression, [Symbol.nonterminalValue], 12)
		);

		// Expression -> Variable
		this.productions.push(
			new Production(Symbol.nonterminalExpression, [Symbol.nonterminalVariable], 13)
		);

		// Expression -> ( BracketedExpression )
		this.productions.push(
			new Production(
				Symbol.nonterminalExpression,
				[
					Symbol.terminalLeftBracket,
					Symbol.nonterminalBracketedExpression,
					Symbol.terminalRightBracket
				],
				14
			)
		);

		// BracketedExpression -> set Variable Expression
		this.productions.push(
			new Production(
				Symbol.nonterminalBracketedExpression,
				[
					Symbol.terminalSet,
					Symbol.nonterminalVariable,
					Symbol.nonterminalExpression,
					'#set'
				],
				17
			)
		);

		// BracketedExpression -> Optr ExpressionList // Used for LISP; removed for Scheme.
		// this.productions.push(new Production(Symbol.nonterminalBracketedExpression,
		// 	[
		// 		Symbol.nonterminalOptr,
		// 		Symbol.nonterminalExpressionList,
		// 		'#operatorUsage'
		// 	], 19));

		// ExpressionList -> Expression ExpressionList
		this.productions.push(
			new Production(
				Symbol.nonterminalExpressionList,
				[Symbol.nonterminalExpression, Symbol.nonterminalExpressionList, '#expressionList'],
				20
			)
		);

		// ExpressionList -> Lambda
		this.productions.push(
			new Production(
				Symbol.nonterminalExpressionList,
				[Symbol.Lambda, '#emptyExpressionList'],
				21
			)
		);

		// Optr -> Function // Used for LISP; removed for Scheme.
		// this.productions.push(new Production(Symbol.nonterminalOptr,
		// 	[
		// 		Symbol.nonterminalFunction
		// 	], 22));

		// Optr -> Value-Op // Used for LISP; removed for Scheme.
		// this.productions.push(new Production(Symbol.nonterminalOptr,
		// 	[
		// 		Symbol.nonterminalValueOp
		// 	], 23));

		// Value -> Integer
		this.productions.push(
			new Production(Symbol.nonterminalValue, [Symbol.terminalIntegerLiteral], 24)
		);

		// Value-Op -> +
		this.productions.push(new Production(Symbol.nonterminalValueOp, [Symbol.terminalPlus], 25));

		// Value-Op -> -
		this.productions.push(
			new Production(Symbol.nonterminalValueOp, [Symbol.terminalMinus], 26)
		);

		// Value-Op -> *
		this.productions.push(
			new Production(Symbol.nonterminalValueOp, [Symbol.terminalMultiply], 27)
		);

		// Value-Op -> /
		this.productions.push(
			new Production(Symbol.nonterminalValueOp, [Symbol.terminalDivide], 28)
		);

		// Value-Op -> =
		this.productions.push(
			new Production(Symbol.nonterminalValueOp, [Symbol.terminalEquals], 29)
		);

		// Value-Op -> <
		this.productions.push(
			new Production(Symbol.nonterminalValueOp, [Symbol.terminalLessThan], 30)
		);

		// Value-Op -> >
		this.productions.push(
			new Production(Symbol.nonterminalValueOp, [Symbol.terminalGreaterThan], 31)
		);

		// Function -> Name
		this.productions.push(new Production(Symbol.nonterminalFunction, [Symbol.terminalID], 33));

		// Variable -> Name
		this.productions.push(
			new Production(Symbol.nonterminalVariable, [Symbol.terminalID, '#variable'], 34)
		);

		// Value -> Quoted-Const
		this.productions.push(
			new Production(Symbol.nonterminalValue, [Symbol.nonterminalQuotedConst], 35)
		);

		// Value-Op -> cons
		this.productions.push(new Production(Symbol.nonterminalValueOp, [Symbol.terminalCons], 36));

		// Value-Op -> car
		this.productions.push(new Production(Symbol.nonterminalValueOp, [Symbol.terminalCar], 37));

		// Value-Op -> cdr
		this.productions.push(new Production(Symbol.nonterminalValueOp, [Symbol.terminalCdr], 38));

		// Value-Op -> number?
		this.productions.push(
			new Production(Symbol.nonterminalValueOp, [Symbol.terminalNumberPred], 39)
		);

		// Value-Op -> symbol?
		this.productions.push(
			new Production(Symbol.nonterminalValueOp, [Symbol.terminalSymbolPred], 40)
		);

		// Value-Op -> list?
		this.productions.push(
			new Production(Symbol.nonterminalValueOp, [Symbol.terminalListPred], 41)
		);

		// Value-Op -> null?
		this.productions.push(
			new Production(Symbol.nonterminalValueOp, [Symbol.terminalNullPred], 42)
		);

		// Quoted-Const -> ' S-Expression
		this.productions.push(
			new Production(
				Symbol.nonterminalQuotedConst,
				[
					Symbol.terminalApostrophe,
					Symbol.nonterminalSExpression,
					'#quotedConstantWithApostrophe'
				],
				43
			)
		);

		// S-Expression -> Integer
		this.productions.push(
			new Production(Symbol.nonterminalSExpression, [Symbol.terminalIntegerLiteral], 44)
		);

		// S-Expression -> Symbol
		this.productions.push(
			new Production(Symbol.nonterminalSExpression, [Symbol.nonterminalSymbol], 45)
		);

		// S-Expression -> ( S-Expression-List )
		this.productions.push(
			new Production(
				Symbol.nonterminalSExpression,
				[
					Symbol.terminalLeftBracket,
					Symbol.nonterminalSExpressionList,
					Symbol.terminalRightBracket
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
		// this.productions.push(new Production(Symbol.nonterminalSExpressionList, [Symbol.nonterminalSExpression, Symbol.nonterminalSExpressionList, '#sExpressionList'], 51));

		// S-Expression-List -> S-Expression . S-Expression
		// this.productions.push(new Production(Symbol.nonterminalSExpressionList, [Symbol.nonterminalSExpression, Symbol.terminalDot, Symbol.nonterminalSExpression, '#sExpressionList'], 52));
		// **** END Old ****

		// **** BEGIN New ****

		// S-Expression-List -> S-Expression S-Expression-List-Tail
		this.productions.push(
			new Production(
				Symbol.nonterminalSExpressionList,
				[
					Symbol.nonterminalSExpression,
					Symbol.nonterminalSExpressionListTail,
					'#sExpressionList'
				],
				47
			)
		);

		// S-Expression-List-Tail -> S-Expression-List
		this.productions.push(
			new Production(
				Symbol.nonterminalSExpressionListTail,
				[Symbol.nonterminalSExpressionList],
				48
			)
		);

		// S-Expression-List-Tail -> . S-Expression
		this.productions.push(
			new Production(
				Symbol.nonterminalSExpressionListTail,
				[Symbol.terminalDot, Symbol.nonterminalSExpression],
				49
			)
		);
		// **** END New ****

		// S-Expression-List -> Lambda
		this.productions.push(
			new Production(
				Symbol.nonterminalSExpressionList,
				[Symbol.Lambda, '#emptySExpressionList'],
				50
			)
		);

		// Symbol -> Name
		this.productions.push(
			new Production(Symbol.nonterminalSymbol, [Symbol.terminalID, '#symbol'], 51)
		);

		// Value-Op -> list
		this.productions.push(new Production(Symbol.nonterminalValueOp, [Symbol.terminalList], 55));

		this.productions.push(
			new Production(Symbol.nonterminalValueOp, [Symbol.terminalStringPred], 56)
		);
		this.productions.push(
			new Production(Symbol.nonterminalValueOp, [Symbol.terminalFloor], 57)
		);
		this.productions.push(
			new Production(Symbol.nonterminalValueOp, [Symbol.terminalRandom], 58)
		);
		// this.productions.push(new Production(Symbol.nonterminalValue, [Symbol.terminalFloatLiteral], 60)); // Note: Number is out of order

		// Productions.Add(new Production(Symbol.N_BracketedExpression, new List<object>() {
		//     Symbol.N_LetKeyword,
		//     Symbol.T_LeftBracket,
		//     Symbol.N_VarExprList,
		//     Symbol.T_RightBracket,
		//     Symbol.N_Expression, "#letUsage" }, 34));
		this.productions.push(
			new Production(
				Symbol.nonterminalBracketedExpression,
				[
					Symbol.nonterminalLetKeyword,
					Symbol.terminalLeftBracket,
					Symbol.nonterminalVarExprList,
					Symbol.terminalRightBracket,
					Symbol.nonterminalExpression,
					'#letUsage'
				],
				59
			)
		);
		// Productions.Add(new Production(Symbol.N_LetKeyword, new List<object>() { Symbol.T_Let }, 35));
		// Productions.Add(new Production(Symbol.N_LetKeyword, new List<object>() { Symbol.T_LetStar }, 36));
		this.productions.push(
			new Production(Symbol.nonterminalLetKeyword, [Symbol.terminalLet], 60)
		);
		this.productions.push(
			new Production(Symbol.nonterminalLetKeyword, [Symbol.terminalLetStar], 61)
		);
		this.productions.push(
			new Production(Symbol.nonterminalLetKeyword, [Symbol.terminalLetRec], 62)
		);
		// Productions.Add(new Production(Symbol.N_VarExprList, new List<object>() {
		//     Symbol.T_LeftBracket,
		//     Symbol.N_Variable,
		//     Symbol.N_Expression,
		//     Symbol.T_RightBracket,
		//     Symbol.N_VarExprList, "#varExprList" }, 37));
		this.productions.push(
			new Production(
				Symbol.nonterminalVarExprList,
				[
					Symbol.terminalLeftBracket,
					Symbol.nonterminalVariable,
					Symbol.nonterminalExpression,
					Symbol.terminalRightBracket,
					Symbol.nonterminalVarExprList,
					'#varExprList'
				],
				63
			)
		);
		// Productions.Add(new Production(Symbol.N_VarExprList, new List<object>() { Symbol.Lambda, "#emptyVarExprList" }, 38));
		this.productions.push(
			new Production(Symbol.nonterminalVarExprList, [Symbol.Lambda, '#emptyVarExprList'], 64)
		);

		// this.productions.push(new Production(
		// 	Symbol.nonterminalSExpression, // TODO? : Create Symbol.nonterminalBracketedSExpression ?
		// 	[
		// 		Symbol.terminalLeftBracket,
		// 		Symbol.terminalQuoteKeyword,
		// 		Symbol.nonterminalSExpression,
		// 		Symbol.terminalRightBracket,
		// 		'#quotedConstantWithQuoteKeyword'
		// 	], n));

		// Quoted-Const -> ' S-Expression
		// this.productions.push(new Production(Symbol.nonterminalQuotedConst,
		// 	[
		// 		Symbol.terminalLeftBracket,
		// 		Symbol.terminalQuoteKeyword,
		// 		Symbol.nonterminalSExpression,
		// 		Symbol.terminalRightBracket,
		// 		'#quotedConstantWithQuoteKeyword'
		// 	], 65));

		// BracketedExpression -> begin Expression ExpressionList
		// this.productions.push(new Production(Symbol.nonterminalBracketedExpression,
		// this.productions.push(new Production(Symbol.nonterminalSExpressionList, // Real HACK.
		// 	[
		// 		Symbol.terminalQuoteKeyword,
		// 		Symbol.nonterminalSExpression,
		// 		'#quotedConstantWithQuoteKeyword'
		// 	], 65));

		// **** BEGIN : Ignore for now ****

		// Productions.Add(new Production(Symbol.N_Value, new List<object>() { Symbol.T_StringLiteral }, 75)); // Note: Number is out of order
		// Productions.Add(new Production(Symbol.N_Value, new List<object>() { Symbol.T_FloatLiteral }, 79)); // Note: Number is out of order

		// this.productions.push(new Production(Symbol.nonterminalValue, [Symbol.terminalStringLiteral], 75)); // Note: Number is out of order

		// this.productions.push(new Production(Symbol.nonterminalValueOp, [Symbol.terminalThrow], 34));
		// this.productions.push(new Production(Symbol.nonterminalValueOp, [Symbol.terminalToString], 76));
		// this.productions.push(new Production(Symbol.nonterminalValueOp, [Symbol.terminalListToString], 77));
		// this.productions.push(new Production(Symbol.nonterminalValueOp, [Symbol.terminalStringToList], 78));
		// this.productions.push(new Production(Symbol.nonterminalValueOp, [Symbol.terminalStringToSymbol], 81));
		// this.productions.push(new Production(Symbol.nonterminalValueOp, [Symbol.terminalPow], 88));
		// this.productions.push(new Production(Symbol.nonterminalValueOp, [Symbol.terminalExp], 82));
		// this.productions.push(new Production(Symbol.nonterminalValueOp, [Symbol.terminalLn], 83));
		// this.productions.push(new Production(Symbol.nonterminalValueOp, [Symbol.terminalSin], 84));
		// this.productions.push(new Production(Symbol.nonterminalValueOp, [Symbol.terminalCos], 85));
		// this.productions.push(new Production(Symbol.nonterminalValueOp, [Symbol.terminalTan], 86));
		// this.productions.push(new Production(Symbol.nonterminalValueOp, [Symbol.terminalAtan2], 89));
		// this.productions.push(new Production(Symbol.nonterminalValueOp, [Symbol.terminalStringLessThan], 91));
		// **** END : Ignore for now ****

		// Next production number is 56.

		// Added for Scheme:

		// BracketedExpression -> Expression ExpressionList
		// Productions.Add(new Production(Symbol.N_BracketedExpression, new List<object>() { Symbol.N_Expression, Symbol.N_ExpressionList, "#evaluableExpression" }, 63));
		this.productions.push(
			new Production(
				Symbol.nonterminalBracketedExpression,
				[
					Symbol.nonterminalExpression,
					Symbol.nonterminalExpressionList,
					'#evaluableExpression'
				],
				101
			)
		);

		// Value-Op -> primop?
		// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_PrimOpPred }, 64));
		this.productions.push(
			new Production(Symbol.nonterminalValueOp, [Symbol.terminalPrimOpPred], 102)
		);

		// Value-Op -> closure?
		// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_ClosurePred }, 65));
		this.productions.push(
			new Production(Symbol.nonterminalValueOp, [Symbol.terminalClosurePred], 103)
		);

		// Value -> ( lambda ArgList Expression )
		// Productions.Add(new Production(Symbol.N_Value, new List<object>() { Symbol.T_LeftBracket, Symbol.T_LambdaKeyword, Symbol.N_ArgList, Symbol.N_Expression, Symbol.T_RightBracket, "#lambdaExpression" }, 66));
		// this.productions.push(new Production(Symbol.nonterminalValue,
		// 	[
		// 		Symbol.terminalLeftBracket,
		// 		Symbol.terminalLambdaKeyword,
		// 		Symbol.nonterminalArgList,
		// 		Symbol.nonterminalExpression,
		// 		Symbol.terminalRightBracket,
		// 		'#lambdaExpression'
		// 	], 104));
		this.productions.push(
			new Production(
				Symbol.nonterminalBracketedExpression,
				[
					// Symbol.terminalLeftBracket,
					Symbol.terminalLambdaKeyword,
					Symbol.nonterminalArgList,
					Symbol.nonterminalExpression,
					// Symbol.terminalRightBracket,
					'#lambdaExpression'
				],
				104
			)
		);

		// Value -> Value-Op
		// Productions.Add(new Production(Symbol.N_Value, new List<object>() { Symbol.N_ValueOp, "#valueOp" }, 67));
		this.productions.push(
			new Production(Symbol.nonterminalValue, [Symbol.nonterminalValueOp, '#valueOp'], 105)
		);

		// BracketedExpression -> LetKeyword ( VarExprList ) Expression
		// LetKeyword -> let
		// LetKeyword -> let*
		// LetKeyword -> letrec
		// VarExprList -> ( Variable Expression ) VarExprList
		// VarExprList -> Lambda
		// Productions.Add(new Production(Symbol.N_LetKeyword, new List<object>() { Symbol.T_LetRec }, 68));
		// this.productions.push(new Production(Symbol.nonterminalLetKeyword,
		// 	[
		// 		Symbol.terminalLetRec
		// 	], 106));

		// BracketedExpression -> call/cc Expression
		// Productions.Add(new Production(Symbol.N_BracketedExpression, new List<object>() { Symbol.T_CallCC, Symbol.N_Expression, "#call/cc" }, 69));
		this.productions.push(
			new Production(
				Symbol.nonterminalBracketedExpression,
				[Symbol.terminalCallCC, Symbol.nonterminalExpression, '#call/cc'],
				107
			)
		);

		if (!isSASL) {
			// BracketedExpression -> if Expression Expression Expression
			this.productions.push(
				new Production(
					Symbol.nonterminalBracketedExpression,
					[
						Symbol.terminalIf,
						Symbol.nonterminalExpression,
						Symbol.nonterminalExpression,
						Symbol.nonterminalExpression,
						'#if'
					],
					15
				)
			);

			// BracketedExpression -> while Expression Expression
			this.productions.push(
				new Production(
					Symbol.nonterminalBracketedExpression,
					[
						Symbol.terminalWhile,
						Symbol.nonterminalExpression,
						Symbol.nonterminalExpression,
						'#while'
					],
					16
				)
			);

			// BracketedExpression -> begin Expression ExpressionList
			this.productions.push(
				new Production(
					Symbol.nonterminalBracketedExpression,
					[
						Symbol.terminalBegin,
						Symbol.nonterminalExpression,
						Symbol.nonterminalExpressionList,
						'#begin'
					],
					18
				)
			);

			// Value-Op -> print
			this.productions.push(
				new Production(Symbol.nonterminalValueOp, [Symbol.terminalPrint], 32)
			);

			// BracketedExpression -> cond ( Expression Expression ) ExprPairList
			this.productions.push(
				new Production(
					Symbol.nonterminalBracketedExpression,
					[
						Symbol.terminalCond,
						Symbol.terminalLeftBracket,
						Symbol.nonterminalExpression,
						Symbol.nonterminalExpression,
						Symbol.terminalRightBracket,
						Symbol.nonterminalExprPairList,
						'#condUsage'
					],
					52
				)
			);

			// ExprPairList -> ( Expression Expression ) ExprPairList
			this.productions.push(
				new Production(
					Symbol.nonterminalExprPairList,
					[
						Symbol.terminalLeftBracket,
						Symbol.nonterminalExpression,
						Symbol.nonterminalExpression,
						Symbol.terminalRightBracket,
						Symbol.nonterminalExprPairList,
						'#exprPairList'
					],
					53
				)
			);

			// ExprPairList -> Lambda
			this.productions.push(
				new Production(
					Symbol.nonterminalExprPairList,
					[Symbol.Lambda, '#emptyExprPairList'],
					54
				)
			);

			// this.productions.push(new Production(Symbol.nonterminalValueOp, [Symbol.terminalRplaca], 56));
			// this.productions.push(new Production(Symbol.nonterminalValueOp, [Symbol.terminalRplacd], 57));

			// Old
			// this.productions.push(new Production(Symbol.nonterminalInput, [Symbol.nonterminalMacroDef], 58));
			// this.productions.push(new Production(
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
			// this.productions.push(new Production(Symbol.nonterminalBracketedInput,
			// 	[
			// 		Symbol.nonterminalMacroDef
			// 	], n));
			// this.productions.push(new Production(
			// 	Symbol.nonterminalMacroDef,
			// 	[
			// 		Symbol.terminalDefineMacro,
			// 		Symbol.nonterminalFunction,
			// 		Symbol.nonterminalArgList,
			// 		Symbol.nonterminalExpression,
			// 		'#macroDefinition'
			// 	], n));
		}
	}

	public get languageName(): string {
		return 'Scheme';
	}

	public get selectorsOfCompatibleParsers(): number[] {
		return [ParserSelector.LL1];
	}

	public executeSemanticAction(semanticStack: Stack<any>, action: string): void {
		// console.log(`SchemeGrammar.executeSemanticAction() : action is ${typeof action} ${action}`);

		let name: Name;
		let variable: Variable<ISExpression>;
		let variableList: VariableList<ISExpression>;
		let expression: IExpression<ISExpression>;
		let expression2: IExpression<ISExpression>;
		let expression3: IExpression<ISExpression>;
		let expressionList: ExpressionList<ISExpression>;
		let sexpression: ISExpression;
		let head: ISExpression;
		let tail: ISExpression;
		let varExprList: [Variable<ISExpression>, IExpression<ISExpression>][];
		let exprPairList: [IExpression<ISExpression>, IExpression<ISExpression>][];

		switch (action) {
			// case '#functionDefinition':
			// 	expression = semanticStack.pop() as IExpression<ISExpression>; // The function's body
			// 	variableList = semanticStack.pop() as VariableList<ISExpression>; // The function's formal argument list
			// 	name = semanticStack.pop() as Name; // The function name
			// 	semanticStack.push(new FunctionDefinition<ISExpression>(name, variableList, expression)); // Add line and column?
			// 	break;

			case '#variableList':
				variableList = semanticStack.pop() as VariableList<ISExpression>;
				variable = semanticStack.pop() as Variable<ISExpression>;
				variableList.value.unshift(variable);
				semanticStack.push(variableList);
				break;

			case '#emptyVariableList':
				semanticStack.push(new VariableList<ISExpression>()); // Add line and column?
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
				expressionList = semanticStack.pop() as ExpressionList<ISExpression>;
				expression = semanticStack.pop() as IExpression<ISExpression>;
				semanticStack.push(new BeginUsage<ISExpression>(expression, expressionList)); // Add line and column?
				break;

			// case '#operatorUsage':
			// 	expressionList = semanticStack.pop() as ExpressionList<ISExpression>;
			// 	name = semanticStack.pop() as Name;
			// 	semanticStack.push(new LISPOperatorUsage(name, expressionList));
			// 	break;

			case '#expressionList':
				expressionList = semanticStack.pop() as ExpressionList<ISExpression>;
				expression = semanticStack.pop() as IExpression<ISExpression>;
				expressionList.value.unshift(expression);
				semanticStack.push(expressionList);
				break;

			case '#emptyExpressionList':
				semanticStack.push(new ExpressionList<ISExpression>());
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

			// case '#macroDefinition':
			// 	expression = semanticStack.pop() as IExpression<ISExpression>; // macroBody
			// 	variableList = semanticStack.pop() as VariableList<ISExpression>; // macroArgList
			// 	name = semanticStack.pop() as Name; // macroName
			// 	semanticStack.push(new MacroDefinition(name, variableList, expression));
			// 	break;

			case '#lambdaExpression':
				const body = semanticStack.pop() as IExpression<ISExpression>;
				const argList = semanticStack.pop() as VariableList<ISExpression>;

				semanticStack.push(new LambdaExpression(argList, body));
				break;

			case '#evaluableExpression':
				expressionList = semanticStack.pop() as ExpressionList<ISExpression>;
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

	public tokenToSymbol(token: Token): number {
		// Returns Symbol
		const tokenValueAsString: string = token.tokenValue as string;

		switch (token.tokenType) {
			case LexicalState.tokenEOF:
				return Symbol.terminalEOF;
			case LexicalState.tokenIntLit:
				return Symbol.terminalIntegerLiteral;

			// HACK version 2 : ThAW 2020-01-06 : The value in an IntegerLiteral can be int or float.
			// In the future, we will need to properly support FloatLiterals
			// and distinguish them from IntegerLiterals.
			// case LexicalState.tokenFltLit: return Symbol.terminalFloatLiteral;
			case LexicalState.tokenFltLit:
				return Symbol.terminalIntegerLiteral;

			case LexicalState.tokenStrLit:
				return Symbol.terminalStringLiteral;
			// case LexicalState.tokenIdent: return Symbol.terminalID;
			case LexicalState.tokenLeftBracket:
				return Symbol.terminalLeftBracket;
			case LexicalState.tokenRightBracket:
				return Symbol.terminalRightBracket;
			case LexicalState.tokenPlus:
				return Symbol.terminalPlus;
			case LexicalState.tokenMinus:
				return Symbol.terminalMinus;
			case LexicalState.tokenMult:
				return Symbol.terminalMultiply;
			case LexicalState.tokenDiv:
				return Symbol.terminalDivide;
			case LexicalState.tokenEqual:
				return Symbol.terminalEquals;
			case LexicalState.tokenLess:
				return Symbol.terminalLessThan;
			case LexicalState.tokenGreater:
				return Symbol.terminalGreaterThan;
			case LexicalState.tokenApostrophe:
				return Symbol.terminalApostrophe;
			case LexicalState.tokenQuoteKeyword:
				return Symbol.terminalQuoteKeyword;

			case LexicalState.tokenIdent:
				switch (tokenValueAsString) {
					// Quoting never changes tokens with these values into IDs.
					case '.':
						return Symbol.terminalDot; // We could modify the tokenizer to generate TokenType.T_Dot in this case, to obviate this line.
					// case 'quote': return Symbol.terminalQuoteKeyword;
					default:
						break;
				}

				if (token.isQuoted) {
					return Symbol.terminalID;
				}

				switch (tokenValueAsString) {
					case 'define':
						return Symbol.terminalDefine;
					case 'if':
						return Symbol.terminalIf;
					case 'while':
						return Symbol.terminalWhile;
					case 'set':
						return Symbol.terminalSet;
					case 'begin':
						return Symbol.terminalBegin;
					case 'print':
						return Symbol.terminalPrint;
					case 'cons':
						return Symbol.terminalCons;
					case 'car':
						return Symbol.terminalCar;
					case 'cdr':
						return Symbol.terminalCdr;
					case 'number?':
						return Symbol.terminalNumberPred;
					case 'symbol?':
						return Symbol.terminalSymbolPred;
					case 'list?':
						return Symbol.terminalListPred;
					case 'null?':
						return Symbol.terminalNullPred;
					case 'primop?':
						return Symbol.terminalPrimOpPred; // Added for Scheme
					case 'closure?':
						return Symbol.terminalClosurePred; // Added for Scheme
					case 'string?':
						return Symbol.terminalStringPred;
					case 'list':
						return Symbol.terminalList;
					case 'rplaca':
						return Symbol.terminalRplaca;
					case 'rplacd':
						return Symbol.terminalRplacd;
					case 'define-macro':
						return Symbol.terminalDefineMacro;
					case 'random':
						return Symbol.terminalRandom;
					case 'tostring':
						return Symbol.terminalToString;
					case 'listtostring':
						return Symbol.terminalListToString;
					case 'stringtolist':
						return Symbol.terminalStringToList;
					case 'stringtosymbol':
						return Symbol.terminalStringToSymbol;
					case 'pow':
						return Symbol.terminalPow;
					case 'exp':
						return Symbol.terminalExp;
					case 'ln':
						return Symbol.terminalLn;
					case 'sin':
						return Symbol.terminalSin;
					case 'cos':
						return Symbol.terminalCos;
					case 'tan':
						return Symbol.terminalTan;
					case 'atan2':
						return Symbol.terminalAtan2;
					case 'floor':
						return Symbol.terminalFloor;
					case 'throw':
						return Symbol.terminalThrow;
					case 'string<':
						return Symbol.terminalStringLessThan;
					case 'cond':
						return Symbol.terminalCond;
					case 'let':
						return Symbol.terminalLet;
					case 'let*':
						return Symbol.terminalLetStar;
					case 'letrec':
						return Symbol.terminalLetRec;
					case 'lambda':
						return Symbol.terminalLambdaKeyword; // Added for Scheme
					case 'call/cc':
						return Symbol.terminalCallCC;
					default:
						return Symbol.terminalID;
				}

			// break;

			default:
				break;
		}

		throw new GrammarException(
			`No grammar symbol matches token ${token.tokenType} ${
				LexicalState[token.tokenType]
			} (value '${token.tokenValue}')`,
			token.line,
			token.column
		);
	}

	public pushTokenOntoSemanticStack(
		semanticStack: Stack<any>,
		tokenAsSymbol: number,
		token: Token
	): void {
		const value = token.tokenValue;

		switch (tokenAsSymbol) {
			case Symbol.terminalID:
			case Symbol.terminalPrint:
			case Symbol.terminalPlus:
			case Symbol.terminalMinus:
			case Symbol.terminalMultiply:
			case Symbol.terminalDivide:
			case Symbol.terminalEquals:
			case Symbol.terminalLessThan:
			case Symbol.terminalGreaterThan:
			case Symbol.terminalCons:
			case Symbol.terminalCar:
			case Symbol.terminalCdr:
			case Symbol.terminalNumberPred:
			case Symbol.terminalSymbolPred:
			case Symbol.terminalListPred:
			case Symbol.terminalNullPred:
			case Symbol.terminalPrimOpPred: // Added for Scheme
			case Symbol.terminalClosurePred: // Added for Scheme
			case Symbol.terminalStringPred:
			case Symbol.terminalList:
			case Symbol.terminalRplaca:
			case Symbol.terminalRplacd:
			case Symbol.terminalRandom:
			case Symbol.terminalToString:
			case Symbol.terminalListToString:
			case Symbol.terminalStringToList:
			case Symbol.terminalStringToSymbol:
			case Symbol.terminalPow:
			case Symbol.terminalExp:
			case Symbol.terminalLn:
			case Symbol.terminalSin:
			case Symbol.terminalCos:
			case Symbol.terminalTan:
			case Symbol.terminalAtan2:
			case Symbol.terminalFloor:
			case Symbol.terminalThrow:
			case Symbol.terminalStringLessThan:
			case Symbol.terminalLet:
			case Symbol.terminalLetStar:
			case Symbol.terminalLetRec:
				semanticStack.push(new Name(value as string, token.line, token.column));
				break;

			case Symbol.terminalIntegerLiteral:
				semanticStack.push(new IntegerLiteral(value));
				break;

			// case Symbol.terminalFloatLiteral:
			// 	// semanticStack.push(new FloatLiteral(value));

			// 	// HACK ThAW 2020-01-06 : The value in an IntegerLiteral can be int or float.
			// 	// In the future, we will need to properly support FloatLiterals
			// 	// and distinguish them from IntegerLiterals.
			// 	semanticStack.push(new IntegerLiteral(value));
			// 	break;

			case Symbol.terminalStringLiteral:
				semanticStack.push(new LISPString(value as string));
				break;

			case Symbol.terminalLeftBracket:
			case Symbol.terminalRightBracket:
			case Symbol.terminalApostrophe:
			case Symbol.terminalQuoteKeyword:
			case Symbol.terminalDefine:
			case Symbol.terminalIf:
			case Symbol.terminalWhile:
			case Symbol.terminalSet:
			case Symbol.terminalBegin:
			case Symbol.terminalCond:
			case Symbol.terminalLambdaKeyword: // Added for Scheme
			case Symbol.terminalCallCC: // Added for Scheme
			case Symbol.terminalEOF:
				break;

			default:
				throw new GrammarException(
					`pushTokenOntoSemanticStack() : Unexpected tokenAsSymbol ${Symbol[tokenAsSymbol]} (${tokenAsSymbol})`,
					token.line,
					token.column
				);
		}
	}

	protected createLetUsage(
		letName: Name,
		varExprList: [Variable<ISExpression>, IExpression<ISExpression>][],
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
