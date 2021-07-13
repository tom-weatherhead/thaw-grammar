// tom-weatherhead/thaw-grammar/src/languages/lisp/lisp-grammar.ts

// **** BEGIN : From the C# version in the Inference project ****

// Terminals.UnionWith(new HashSet<Symbol>() {
//     Symbol.T_LeftBracket, Symbol.T_RightBracket, Symbol.T_Define,
//     Symbol.T_If, Symbol.T_While, Symbol.T_Set, Symbol.T_Begin,
//     Symbol.T_Plus, Symbol.T_Minus, Symbol.T_Multiply, Symbol.T_Divide,
//     Symbol.T_Equals, Symbol.T_LessThan, //Symbol.T_GreaterThan,
//     Symbol.T_Print, Symbol.T_ID, Symbol.T_IntegerLiteral, Symbol.T_Cond,
//     Symbol.T_Let, Symbol.T_LetStar, Symbol.T_EOF });

// NonTerminals.UnionWith(new HashSet<Symbol>() { Symbol.N_Start,
//     Symbol.N_Input, Symbol.N_Expression, Symbol.N_FunDef, Symbol.N_Function,
//     Symbol.N_ArgList, Symbol.N_VariableList, Symbol.N_Variable, Symbol.N_Value,
//     Symbol.N_BracketedExpression, Symbol.N_ExpressionList, Symbol.N_Optr, Symbol.N_ValueOp,
//     Symbol.N_ExprPairList, Symbol.N_LetKeyword, Symbol.N_VarExprList });

// Productions.Add(new Production(Symbol.N_Start, new List<object>() { Symbol.N_Input, Symbol.T_EOF }, 1));
// Productions.Add(new Production(Symbol.N_Input, new List<object>() { Symbol.N_Expression }, 2));
// Productions.Add(new Production(Symbol.N_Input, new List<object>() { Symbol.N_FunDef }, 3));
// Productions.Add(new Production(Symbol.N_FunDef, new List<object>() { Symbol.T_LeftBracket, Symbol.T_Define, Symbol.N_Function, Symbol.N_ArgList, Symbol.N_Expression, Symbol.T_RightBracket, "#functionDefinition" }, 4));
// Productions.Add(new Production(Symbol.N_ArgList, new List<object>() { Symbol.T_LeftBracket, Symbol.N_VariableList, Symbol.T_RightBracket }, 5));
// Productions.Add(new Production(Symbol.N_VariableList, new List<object>() { Symbol.N_Variable, Symbol.N_VariableList, "#variableList" }, 6));
// Productions.Add(new Production(Symbol.N_VariableList, new List<object>() { Symbol.Lambda, "#emptyVariableList" }, 7));
// Productions.Add(new Production(Symbol.N_Expression, new List<object>() { Symbol.N_Value }, 8));
// Productions.Add(new Production(Symbol.N_Expression, new List<object>() { Symbol.N_Variable }, 9));
// Productions.Add(new Production(Symbol.N_Expression, new List<object>() { Symbol.T_LeftBracket, Symbol.N_BracketedExpression, Symbol.T_RightBracket }, 10));
// Productions.Add(new Production(Symbol.N_BracketedExpression, new List<object>() { Symbol.T_If, Symbol.N_Expression, Symbol.N_Expression, Symbol.N_Expression, "#if" }, 11));
// Productions.Add(new Production(Symbol.N_BracketedExpression, new List<object>() { Symbol.T_While, Symbol.N_Expression, Symbol.N_Expression, "#while" }, 12));
// Productions.Add(new Production(Symbol.N_BracketedExpression, new List<object>() { Symbol.T_Set, Symbol.N_Variable, Symbol.N_Expression, "#set" }, 13));
// Productions.Add(new Production(Symbol.N_BracketedExpression, new List<object>() { Symbol.T_Begin, Symbol.N_Expression, Symbol.N_ExpressionList, "#begin" }, 14));
// Productions.Add(new Production(Symbol.N_BracketedExpression, new List<object>() { Symbol.N_Optr, Symbol.N_ExpressionList, "#operatorUsage" }, 15));
// Productions.Add(new Production(Symbol.N_ExpressionList, new List<object>() { Symbol.N_Expression, Symbol.N_ExpressionList, "#expressionList" }, 16));
// Productions.Add(new Production(Symbol.N_ExpressionList, new List<object>() { Symbol.Lambda, "#emptyExpressionList" }, 17));
// Productions.Add(new Production(Symbol.N_Optr, new List<object>() { Symbol.N_Function }, 18));
// Productions.Add(new Production(Symbol.N_Optr, new List<object>() { Symbol.N_ValueOp }, 19));
// Productions.Add(new Production(Symbol.N_Value, new List<object>() { Symbol.T_IntegerLiteral }, 20));
// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Plus }, 21));
// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Minus }, 22));
// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Multiply }, 23));
// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Divide }, 24));
// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Equals }, 25));
// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_LessThan }, 26));
// //Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_GreaterThan }, 27));
// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Print }, 28));
// Productions.Add(new Production(Symbol.N_Function, new List<object>() { Symbol.T_ID }, 29));
// Productions.Add(new Production(Symbol.N_Variable, new List<object>() { Symbol.T_ID, "#variable" }, 30));
// Productions.Add(new Production(Symbol.N_BracketedExpression, new List<object>() {
//     Symbol.T_Cond,
//     Symbol.T_LeftBracket,
//     Symbol.N_Expression,
//     Symbol.N_Expression,
//     Symbol.T_RightBracket,
//     Symbol.N_ExprPairList, "#condUsage" }, 31));
// Productions.Add(new Production(Symbol.N_ExprPairList, new List<object>() {
//     Symbol.T_LeftBracket,
//     Symbol.N_Expression,
//     Symbol.N_Expression,
//     Symbol.T_RightBracket,
//     Symbol.N_ExprPairList, "#exprPairList" }, 32));
// Productions.Add(new Production(Symbol.N_ExprPairList, new List<object>() { Symbol.Lambda, "#emptyExprPairList" }, 33));
// Productions.Add(new Production(Symbol.N_BracketedExpression, new List<object>() {
//     Symbol.N_LetKeyword,
//     Symbol.T_LeftBracket,
//     Symbol.N_VarExprList,
//     Symbol.T_RightBracket,
//     Symbol.N_Expression, "#letUsage" }, 34));
// Productions.Add(new Production(Symbol.N_LetKeyword, new List<object>() { Symbol.T_Let }, 35));
// Productions.Add(new Production(Symbol.N_LetKeyword, new List<object>() { Symbol.T_LetStar }, 36));
// Productions.Add(new Production(Symbol.N_VarExprList, new List<object>() {
//     Symbol.T_LeftBracket,
//     Symbol.N_Variable,
//     Symbol.N_Expression,
//     Symbol.T_RightBracket,
//     Symbol.N_VarExprList, "#varExprList" }, 37));
// Productions.Add(new Production(Symbol.N_VarExprList, new List<object>() { Symbol.Lambda, "#emptyVarExprList" }, 38));

// From Chapter 2 of Kamin:

// Terminals.UnionWith(new HashSet<Symbol>() {
//     Symbol.T_Cons, Symbol.T_Car, Symbol.T_Cdr,
//     Symbol.T_NumberPred, Symbol.T_SymbolPred, Symbol.T_ListPred, Symbol.T_NullPred,
//     Symbol.T_Apostrophe, Symbol.T_Dot, Symbol.T_List,
//     Symbol.T_Rplaca, Symbol.T_Rplacd, Symbol.T_DefineMacro, Symbol.T_QuoteKeyword,
//     Symbol.T_Random, Symbol.T_StringLiteral, Symbol.T_StringPred,
//     Symbol.T_ToString, Symbol.T_ListToString, Symbol.T_StringToList, Symbol.T_StringToSymbol,
//     Symbol.T_FloatLiteral, Symbol.T_Pow, Symbol.T_Exp, Symbol.T_Ln, Symbol.T_Sin, Symbol.T_Cos, Symbol.T_Tan, Symbol.T_Atan2, Symbol.T_Floor,
//     Symbol.T_Throw, Symbol.T_StringLessThan });

// NonTerminals.UnionWith(new HashSet<Symbol>() {
//     Symbol.N_QuotedConst, Symbol.N_SExpression, Symbol.N_SExpressionList, Symbol.N_Symbol,
//     Symbol.N_MacroDef });

// Productions.Add(new Production(Symbol.N_Value, new List<object>() { Symbol.N_QuotedConst }, 39));
// Productions.Add(new Production(Symbol.N_Value, new List<object>() { Symbol.T_StringLiteral }, 75)); // Note: Number is out of order
// Productions.Add(new Production(Symbol.N_Value, new List<object>() { Symbol.T_FloatLiteral }, 79)); // Note: Number is out of order
// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Cons }, 40));
// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Car }, 41));
// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Cdr }, 42));
// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_NumberPred }, 43));
// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_SymbolPred }, 44));
// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_ListPred }, 45));
// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_NullPred }, 46));
// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_StringPred }, 73)); // Note: Number is out of order
// Productions.Add(new Production(Symbol.N_QuotedConst, new List<object>() { Symbol.T_Apostrophe, Symbol.N_SExpression, "#quotedConstantWithApostrophe" }, 47));
// Productions.Add(new Production(Symbol.N_SExpression, new List<object>() { Symbol.T_IntegerLiteral }, 48));
// Productions.Add(new Production(Symbol.N_SExpression, new List<object>() { Symbol.N_Symbol }, 49));
// Productions.Add(new Production(Symbol.N_SExpression, new List<object>() { Symbol.T_StringLiteral }, 74)); // Note: Number is out of order
// Productions.Add(new Production(Symbol.N_SExpression, new List<object>() { Symbol.T_FloatLiteral }, 80)); // Note: Number is out of order
// Productions.Add(new Production(Symbol.N_SExpression, new List<object>() { Symbol.T_LeftBracket, Symbol.N_SExpressionList, Symbol.T_RightBracket }, 50));
// Productions.Add(new Production(Symbol.N_SExpressionList, new List<object>() { Symbol.N_SExpression, Symbol.N_SExpressionList, "#sExpressionList" }, 51));
// Productions.Add(new Production(Symbol.N_SExpressionList, new List<object>() { Symbol.N_SExpression, Symbol.T_Dot, Symbol.N_SExpression, "#sExpressionList" }, 52));
// Productions.Add(new Production(Symbol.N_SExpressionList, new List<object>() { Symbol.Lambda, "#emptySExpressionList" }, 53));
// Productions.Add(new Production(Symbol.N_Symbol, new List<object>() { Symbol.T_ID, "#symbol" }, 54));
// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_List }, 55));
// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Rplaca }, 56));
// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Rplacd }, 57));
// Productions.Add(new Production(Symbol.N_Input, new List<object>() { Symbol.N_MacroDef }, 58));
// Productions.Add(new Production(Symbol.N_MacroDef, new List<object>() { Symbol.T_LeftBracket, Symbol.T_DefineMacro, Symbol.N_Function, Symbol.N_ArgList, Symbol.N_Expression, Symbol.T_RightBracket, "#macroDefinition" }, 59));
// Productions.Add(new Production(Symbol.N_SExpression, new List<object>() { Symbol.T_LeftBracket, Symbol.T_QuoteKeyword, Symbol.N_SExpression, Symbol.T_RightBracket, "#quotedConstantWithQuoteKeyword" }, 60));
// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Random }, 61));
// //Productions.Add(new Production(Symbol.N_QuotedConst, new List<object>() { Symbol.T_LeftBracket, Symbol.T_QuoteKeyword, Symbol.N_SExpression, Symbol.T_RightBracket, "#quotedConstantWithQuoteKeyword" }, 62)); // Needed for macros.
// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_ToString }, 76)); // Note: Number is out of order
// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_ListToString }, 77)); // Note: Number is out of order
// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_StringToList }, 78)); // Note: Number is out of order
// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_StringToSymbol }, 81)); // Note: Number is out of order
// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Pow }, 88)); // Note: Number is out of order
// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Exp }, 82)); // Note: Number is out of order
// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Ln }, 83)); // Note: Number is out of order
// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Sin }, 84)); // Note: Number is out of order
// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Cos }, 85)); // Note: Number is out of order
// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Tan }, 86)); // Note: Number is out of order
// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Floor }, 87)); // Note: Number is out of order
// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Atan2 }, 89)); // Note: Number is out of order
// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Throw }, 90)); // Note: Number is out of order
// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_StringLessThan }, 91)); // Note: Number is out of order

// **** END : From the C# version in the Inference project ****

'use strict';

import { Stack } from 'thaw-common-utilities.ts';

import { LexicalState, Token } from 'thaw-lexical-analyzer';

import { ExpressionList } from '../../common/domain-object-model/expression-list';
import { IExpression } from '../../common/domain-object-model/iexpression';
import { Name } from '../../common/domain-object-model/name';
import { Variable } from '../../common/domain-object-model/variable';
import { VariableList } from '../../common/domain-object-model/variable-list';

import { BeginUsage } from '../../common/domain-object-model/begin-usage';
import { CondUsage } from '../../common/domain-object-model/cond-usage';
import { FunctionDefinition } from '../../common/domain-object-model/function-definition';
import { IfUsage } from '../../common/domain-object-model/if-usage';
import { LetStarUsage } from '../../common/domain-object-model/let-star-usage';
import { LetUsage } from '../../common/domain-object-model/let-usage';
// import { OperatorUsage } from '../../common/domain-object-model/operator-usage';
import { SetUsage } from '../../common/domain-object-model/set-usage';
import { WhileUsage } from '../../common/domain-object-model/while-usage';

import { ArgumentException } from '../../common/exceptions/argument-exception';
import { GrammarException } from '../../common/exceptions/grammar-exception';

import { GrammarBase } from '../../common/grammar-base';
import { ParserSelector } from '../../common/parser-selectors';
import { Production } from '../../common/production';
import { Symbol } from '../../common/symbol';

import { FloatLiteral } from './domain-object-model/float-literal';
import { IntegerLiteral } from './domain-object-model/integer-literal';
import { ISExpression } from './domain-object-model/isexpression';
import { LISPOperatorUsage } from './domain-object-model/lisp-operator-usage';
import { LISPString } from './domain-object-model/lisp-string';
import { LISPSymbol } from './domain-object-model/lisp-symbol';
import { NullSExpression } from './domain-object-model/null-sexpression';
import { QuotedConstantWithApostrophe } from './domain-object-model/quoted-constant-with-apostrophe';
import { QuotedConstantWithQuoteKeyword } from './domain-object-model/quoted-constant-with-quote-keyword';
import { SExpressionList } from './domain-object-model/sexpression-list';

export class LISPGrammar extends GrammarBase {
	// The LISP grammar from Kamin (the book 'Programming Languages: An Interpreter-Based Approach')

	constructor() {
		super(Symbol.nonterminalStart);

		this.terminals.push(Symbol.terminalLeftBracket);
		this.terminals.push(Symbol.terminalRightBracket);
		this.terminals.push(Symbol.terminalDefine);
		this.terminals.push(Symbol.terminalIf);
		this.terminals.push(Symbol.terminalWhile);
		this.terminals.push(Symbol.terminalSet);
		this.terminals.push(Symbol.terminalBegin);
		this.terminals.push(Symbol.terminalPlus);
		this.terminals.push(Symbol.terminalMinus);
		this.terminals.push(Symbol.terminalMultiply);
		this.terminals.push(Symbol.terminalDivide);
		this.terminals.push(Symbol.terminalEquals);
		this.terminals.push(Symbol.terminalLessThan);
		this.terminals.push(Symbol.terminalGreaterThan);
		this.terminals.push(Symbol.terminalPrint);
		this.terminals.push(Symbol.terminalID);
		this.terminals.push(Symbol.terminalIntegerLiteral);
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
		// this.terminals.push(Symbol.terminalRplaca);
		// this.terminals.push(Symbol.terminalRplacd);
		// this.terminals.push(Symbol.terminalDefineMacro);
		// this.terminals.push(Symbol.terminalQuoteKeyword);
		// this.terminals.push(Symbol.terminalStringLiteral);
		// this.terminals.push(Symbol.terminalStringPred);
		// this.terminals.push(Symbol.terminalToString);
		// this.terminals.push(Symbol.terminalListToString);
		// this.terminals.push(Symbol.terminalStringToList);
		// this.terminals.push(Symbol.terminalStringToSymbol);
		// this.terminals.push(Symbol.terminalFloatLiteral);
		// this.terminals.push(Symbol.terminalPow);
		// this.terminals.push(Symbol.terminalExp);
		// this.terminals.push(Symbol.terminalLn);
		// this.terminals.push(Symbol.terminalSin);
		// this.terminals.push(Symbol.terminalCos);
		// this.terminals.push(Symbol.terminalTan);
		// this.terminals.push(Symbol.terminalAtan2);
		// this.terminals.push(Symbol.terminalFloor);
		// this.terminals.push(Symbol.terminalStringLessThan);
		// this.terminals.push(Symbol.terminalRandom);
		// this.terminals.push(Symbol.terminalThrow);
		this.terminals.push(Symbol.terminalEOF);

		this.nonTerminals.push(Symbol.nonterminalStart);
		this.nonTerminals.push(Symbol.nonterminalInput);
		this.nonTerminals.push(Symbol.nonterminalExpression);
		this.nonTerminals.push(Symbol.nonterminalFunDef);
		this.nonTerminals.push(Symbol.nonterminalFunction);
		this.nonTerminals.push(Symbol.nonterminalArgList);
		this.nonTerminals.push(Symbol.nonterminalVariableList);
		this.nonTerminals.push(Symbol.nonterminalVariable);
		this.nonTerminals.push(Symbol.nonterminalValue);
		this.nonTerminals.push(Symbol.nonterminalBracketedExpression);
		this.nonTerminals.push(Symbol.nonterminalExpressionList);
		this.nonTerminals.push(Symbol.nonterminalOptr);
		this.nonTerminals.push(Symbol.nonterminalValueOp);
		this.nonTerminals.push(Symbol.nonterminalQuotedConst);
		this.nonTerminals.push(Symbol.nonterminalSExpression);
		this.nonTerminals.push(Symbol.nonterminalSExpressionList);
		this.nonTerminals.push(Symbol.nonterminalSExpressionListTail);
		this.nonTerminals.push(Symbol.nonterminalSymbol);
		this.nonTerminals.push(Symbol.nonterminalExprPairList);
		// this.nonTerminals.push(Symbol.nonterminalMacroDef);

		// this.nonTerminals.push(Symbol.nonterminalBracketedEntity);
		this.nonTerminals.push(Symbol.nonterminalBracketedInput);
		this.nonTerminals.push(Symbol.nonterminalUnbracketedInput);

		// From Chapter 1 of Kamin:

		// Input -> Expression
		// Input -> FunDef
		// FunDef -> ( define Function ArgList Expression )
		// ArgList -> ( VariableList )
		// VariableList -> Variable VariableList
		// VariableList -> Lambda
		// Expression -> Value
		// Expression -> Variable
		// Expression -> ( BracketedExpression )
		// BracketedExpression -> if Expression Expression Expression
		// BracketedExpression -> while Expression Expression
		// BracketedExpression -> set Variable Expression
		// BracketedExpression -> begin Expression ExpressionList
		// BracketedExpression -> Optr ExpressionList
		// ExpressionList -> Expression ExpressionList
		// ExpressionList -> Lambda
		// Optr -> Function
		// Optr -> Value-Op
		// Value -> Integer
		// Value-Op -> +
		// Value-Op -> -
		// Value-Op -> *
		// Value-Op -> /
		// Value-Op -> =
		// Value-Op -> <
		// Value-Op -> >
		// Value-Op -> print
		// Function -> Name
		// Variable -> Name
		// Integer -> ...
		// Name -> ...

		// This initial production needed to be added: Start -> Input EOF
		this.productions.push(
			new Production(
				Symbol.nonterminalStart,
				[Symbol.nonterminalInput, Symbol.terminalEOF],
				1
			)
		);

		// Input -> ( BracketedInput )
		this.productions.push(
			new Production(
				Symbol.nonterminalInput,
				[
					Symbol.terminalLeftBracket,
					Symbol.nonterminalBracketedInput,
					Symbol.terminalRightBracket
				],
				2
			)
		);

		// Input -> UnbracketedInput
		this.productions.push(
			new Production(Symbol.nonterminalInput, [Symbol.nonterminalUnbracketedInput], 3)
		);

		// BracketedInput -> BracketedExpression
		this.productions.push(
			new Production(
				Symbol.nonterminalBracketedInput,
				[Symbol.nonterminalBracketedExpression],
				4
			)
		);

		// BracketedInput -> FunDef
		this.productions.push(
			new Production(Symbol.nonterminalBracketedInput, [Symbol.nonterminalFunDef], 5)
		);

		// - UnbracketedInput -> Value
		this.productions.push(
			new Production(Symbol.nonterminalUnbracketedInput, [Symbol.nonterminalValue], 6)
		);

		// - UnbracketedInput -> Variable
		this.productions.push(
			new Production(Symbol.nonterminalUnbracketedInput, [Symbol.nonterminalVariable], 7)
		);

		// FunDef -> define Function ArgList Expression
		this.productions.push(
			new Production(
				Symbol.nonterminalFunDef,
				[
					Symbol.terminalDefine,
					Symbol.nonterminalFunction,
					Symbol.nonterminalArgList,
					Symbol.nonterminalExpression,
					'#functionDefinition'
				],
				8
			)
		);

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

		// BracketedExpression -> Optr ExpressionList
		this.productions.push(
			new Production(
				Symbol.nonterminalBracketedExpression,
				[Symbol.nonterminalOptr, Symbol.nonterminalExpressionList, '#operatorUsage'],
				19
			)
		);

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

		// Optr -> Function
		this.productions.push(
			new Production(Symbol.nonterminalOptr, [Symbol.nonterminalFunction], 22)
		);

		// Optr -> Value-Op
		this.productions.push(
			new Production(Symbol.nonterminalOptr, [Symbol.nonterminalValueOp], 23)
		);

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

		// Value-Op -> print
		this.productions.push(
			new Production(Symbol.nonterminalValueOp, [Symbol.terminalPrint], 32)
		);

		// Function -> Name
		this.productions.push(new Production(Symbol.nonterminalFunction, [Symbol.terminalID], 33));

		// Variable -> Name
		this.productions.push(
			new Production(Symbol.nonterminalVariable, [Symbol.terminalID, '#variable'], 34)
		);

		// Integer -> ...
		// Name -> ...

		// From Chapter 2 of Kamin:

		// Value -> Quoted-Const
		// Value-Op -> cons
		// Value-Op -> car
		// Value-Op -> cdr
		// Value-Op -> number?
		// Value-Op -> symbol?
		// Value-Op -> list?
		// Value-Op -> null?
		// Quoted-Const -> ' S-Expression
		// S-Expression -> Integer
		// S-Expression -> Symbol
		// S-Expression -> ( S-Expression-List )
		// S-Expression-List -> S-Expression S-Expression-List
		// S-Expression-List -> S-Expression . S-Expression
		// S-Expression-List -> Lambda
		// Symbol -> Name
		// BracketedExpression -> cond ( Expression Expression ) ExprPairList
		// ExprPairList -> ( Expression Expression ) ExprPairList
		// ExprPairList -> Lambda
		// Value-Op -> list

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
					Symbol.nonterminalExprPairList
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

		// Value-Op -> list
		this.productions.push(new Production(Symbol.nonterminalValueOp, [Symbol.terminalList], 55));

		// **** BEGIN : Ignore for now ****
		// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_StringPred }, 73)); // Note: Number is out of order
		// Productions.Add(new Production(Symbol.N_Value, new List<object>() { Symbol.T_StringLiteral }, 75)); // Note: Number is out of order
		// Productions.Add(new Production(Symbol.N_Value, new List<object>() { Symbol.T_FloatLiteral }, 79)); // Note: Number is out of order

		// Productions.Add(new Production(Symbol.N_BracketedExpression, new List<object>() {
		//     Symbol.N_LetKeyword,
		//     Symbol.T_LeftBracket,
		//     Symbol.N_VarExprList,
		//     Symbol.T_RightBracket,
		//     Symbol.N_Expression, "#letUsage" }, 34));
		// Productions.Add(new Production(Symbol.N_LetKeyword, new List<object>() { Symbol.T_Let }, 35));
		// Productions.Add(new Production(Symbol.N_LetKeyword, new List<object>() { Symbol.T_LetStar }, 36));
		// Productions.Add(new Production(Symbol.N_VarExprList, new List<object>() {
		//     Symbol.T_LeftBracket,
		//     Symbol.N_Variable,
		//     Symbol.N_Expression,
		//     Symbol.T_RightBracket,
		//     Symbol.N_VarExprList, "#varExprList" }, 37));
		// Productions.Add(new Production(Symbol.N_VarExprList, new List<object>() { Symbol.Lambda, "#emptyVarExprList" }, 38));

		// this.productions.push(new Production(Symbol.nonterminalValue, [Symbol.terminalStringLiteral], 75)); // Note: Number is out of order
		// this.productions.push(new Production(Symbol.nonterminalValue, [Symbol.terminalFloatLiteral], 79)); // Note: Number is out of order
		// this.productions.push(new Production(Symbol.nonterminalValueOp, [Symbol.terminalStringPred], 73)); // Note: Number is out of order
		// this.productions.push(new Production(Symbol.nonterminalValueOp, [Symbol.terminalRandom], 33));
		// this.productions.push(new Production(Symbol.nonterminalValueOp, [Symbol.terminalThrow], 34));
		// this.productions.push(new Production(Symbol.nonterminalValueOp, [Symbol.terminalRplaca], 56));
		// this.productions.push(new Production(Symbol.nonterminalValueOp, [Symbol.terminalRplacd], 57));
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
		// this.productions.push(new Production(Symbol.nonterminalValueOp, [Symbol.terminalFloor], 87));
		// this.productions.push(new Production(Symbol.nonterminalValueOp, [Symbol.terminalAtan2], 89));
		// this.productions.push(new Production(Symbol.nonterminalValueOp, [Symbol.terminalStringLessThan], 91));
		// **** END : Ignore for now ****

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

		// this.productions.push(new Production(
		// 	Symbol.nonterminalSExpression, // TODO? : Create Symbol.nonterminalBracketedSExpression ?
		// 	[
		// 		Symbol.terminalLeftBracket,
		// 		Symbol.terminalQuoteKeyword,
		// 		Symbol.nonterminalSExpression,
		// 		Symbol.terminalRightBracket,
		// 		'#quotedConstantWithQuoteKeyword'
		// 	], n));

		// Next production number is 56.
	}

	public get languageName(): string {
		return 'LISP';
	}

	public get selectorsOfCompatibleParsers(): number[] {
		return [ParserSelector.LL1];
	}

	public executeSemanticAction(semanticStack: Stack<any>, action: string): void {
		// console.log(`LISPGrammar.executeSemanticAction() : action is ${typeof action} ${action}`);

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
			case '#functionDefinition':
				expression = semanticStack.pop() as IExpression<ISExpression>; // The function's body
				variableList = semanticStack.pop() as VariableList<ISExpression>; // The function's formal argument list
				name = semanticStack.pop() as Name; // The function name
				semanticStack.push(
					new FunctionDefinition<ISExpression>(name, variableList, expression)
				); // Add line and column?
				break;

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

			case '#operatorUsage':
				expressionList = semanticStack.pop() as ExpressionList<ISExpression>;
				name = semanticStack.pop() as Name;
				semanticStack.push(new LISPOperatorUsage(name, expressionList));
				break;

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
			case LexicalState.tokenFltLit:
				return Symbol.terminalFloatLiteral;
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
					case 'quote':
						return Symbol.terminalQuoteKeyword;
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
				semanticStack.push(new Name(value as string, token.line, token.column));
				break;

			case Symbol.terminalIntegerLiteral:
				semanticStack.push(new IntegerLiteral(value));
				break;

			case Symbol.terminalFloatLiteral:
				semanticStack.push(new FloatLiteral(value));
				break;

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

			default:
				throw new ArgumentException(
					`LISPGrammar.createLetUsage() : Unknown 'let' keyword '${letName.value}.`,
					'letName',
					letName.line,
					letName.column
				);
		}
	}
}
