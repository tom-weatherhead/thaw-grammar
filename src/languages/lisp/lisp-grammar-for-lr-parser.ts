// tom-weatherhead/thaw-grammar/src/languages/lisp/lisp-grammar-for-lr-parser.ts

// !!!! Not yet tested with any LR parser

// **** BEGIN : From the C# version in the Inference project ****

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
// // Next number is 92.

// **** END : From the C# version in the Inference project ****

// import { Stack } from 'thaw-common-utilities.ts';

import {
	GrammarSymbol,
	IToken,
	LexicalState,
	ParserSelector,
	SemanticStackType
} from 'thaw-interpreter-types';

import { createProduction } from 'thaw-interpreter-core';

// import { LexicalState, Token } from 'thaw-lexical-analyzer';

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
// import { GrammarException } from '../../common/exceptions/grammar-exception';

import { GrammarBase, GrammarException } from 'thaw-interpreter-core';
// import { ParserSelector } from '../../common/parser-selectors';
// import { createProduction } from '../../common/production';

// import { Symbol } from '../../common/symbol';

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

export class LISPGrammarForLRParser extends GrammarBase {
	// The LISP grammar from Kamin

	constructor() {
		super(GrammarSymbol.nonterminalStart);

		this.terminals.push(GrammarSymbol.terminalLeftBracket);
		this.terminals.push(GrammarSymbol.terminalRightBracket);
		this.terminals.push(GrammarSymbol.terminalDefine);
		this.terminals.push(GrammarSymbol.terminalIf);
		this.terminals.push(GrammarSymbol.terminalWhile);
		this.terminals.push(GrammarSymbol.terminalSet);
		this.terminals.push(GrammarSymbol.terminalBegin);
		this.terminals.push(GrammarSymbol.terminalPlus);
		this.terminals.push(GrammarSymbol.terminalMinus);
		this.terminals.push(GrammarSymbol.terminalMultiply);
		this.terminals.push(GrammarSymbol.terminalDivide);
		this.terminals.push(GrammarSymbol.terminalEquals);
		this.terminals.push(GrammarSymbol.terminalLessThan);
		this.terminals.push(GrammarSymbol.terminalGreaterThan);
		this.terminals.push(GrammarSymbol.terminalPrint);
		this.terminals.push(GrammarSymbol.terminalID);
		this.terminals.push(GrammarSymbol.terminalIntegerLiteral);
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
		// this.terminals.push(GrammarSymbol.terminalRplaca);
		// this.terminals.push(GrammarSymbol.terminalRplacd);
		// this.terminals.push(GrammarSymbol.terminalDefineMacro);
		// this.terminals.push(GrammarSymbol.terminalQuoteKeyword);
		// this.terminals.push(GrammarSymbol.terminalStringLiteral);
		// this.terminals.push(GrammarSymbol.terminalStringPred);
		// this.terminals.push(GrammarSymbol.terminalToString);
		// this.terminals.push(GrammarSymbol.terminalListToString);
		// this.terminals.push(GrammarSymbol.terminalStringToList);
		// this.terminals.push(GrammarSymbol.terminalStringToSymbol);
		// this.terminals.push(GrammarSymbol.terminalFloatLiteral);
		// this.terminals.push(GrammarSymbol.terminalPow);
		// this.terminals.push(GrammarSymbol.terminalExp);
		// this.terminals.push(GrammarSymbol.terminalLn);
		// this.terminals.push(GrammarSymbol.terminalSin);
		// this.terminals.push(GrammarSymbol.terminalCos);
		// this.terminals.push(GrammarSymbol.terminalTan);
		// this.terminals.push(GrammarSymbol.terminalAtan2);
		// this.terminals.push(GrammarSymbol.terminalFloor);
		// this.terminals.push(GrammarSymbol.terminalStringLessThan);
		// this.terminals.push(GrammarSymbol.terminalRandom);
		// this.terminals.push(GrammarSymbol.terminalThrow);
		this.terminals.push(GrammarSymbol.terminalEOF);

		this.nonTerminals.push(GrammarSymbol.nonterminalStart);
		this.nonTerminals.push(GrammarSymbol.nonterminalInput);
		this.nonTerminals.push(GrammarSymbol.nonterminalExpression);
		this.nonTerminals.push(GrammarSymbol.nonterminalFunDef);
		this.nonTerminals.push(GrammarSymbol.nonterminalFunction);
		this.nonTerminals.push(GrammarSymbol.nonterminalArgList);
		this.nonTerminals.push(GrammarSymbol.nonterminalVariableList);
		this.nonTerminals.push(GrammarSymbol.nonterminalVariable);
		this.nonTerminals.push(GrammarSymbol.nonterminalValue);
		this.nonTerminals.push(GrammarSymbol.nonterminalBracketedExpression);
		this.nonTerminals.push(GrammarSymbol.nonterminalExpressionList);
		this.nonTerminals.push(GrammarSymbol.nonterminalOptr);
		this.nonTerminals.push(GrammarSymbol.nonterminalValueOp);
		this.nonTerminals.push(GrammarSymbol.nonterminalQuotedConst);
		this.nonTerminals.push(GrammarSymbol.nonterminalSExpression);
		this.nonTerminals.push(GrammarSymbol.nonterminalSExpressionList);
		this.nonTerminals.push(GrammarSymbol.nonterminalSymbol);
		this.nonTerminals.push(GrammarSymbol.nonterminalExprPairList);
		// this.nonTerminals.push(GrammarSymbol.nonterminalMacroDef);

		// this.nonTerminals.push(GrammarSymbol.nonterminalBracketedEntity);

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

		// We prevent function definitions from being considered as expressions.

		// Productions.Add(createProduction(Symbol.N_Start, new List<object>() { Symbol.N_Input, Symbol.T_EOF }, 1));
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalStart,
				[GrammarSymbol.nonterminalInput, GrammarSymbol.terminalEOF],
				1
			)
		);

		// Input -> Expression
		// Productions.Add(new Production(Symbol.N_Input, new List<object>() { Symbol.N_Expression }, 2));
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalInput,
				[GrammarSymbol.nonterminalExpression],
				2
			)
		);

		// Input -> FunDef
		// Productions.Add(new Production(Symbol.N_Input, new List<object>() { Symbol.N_FunDef }, 3));
		this.productions.push(
			createProduction(GrammarSymbol.nonterminalInput, [GrammarSymbol.nonterminalFunDef], 3)
		);

		// FunDef -> ( define Function ArgList Expression )
		// Productions.Add(new Production(Symbol.N_FunDef, new List<object>() { Symbol.T_LeftBracket, Symbol.T_Define, Symbol.N_Function, Symbol.N_ArgList, Symbol.N_Expression, Symbol.T_RightBracket, "#functionDefinition" }, 4));
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalFunDef,
				[
					GrammarSymbol.terminalLeftBracket,
					GrammarSymbol.terminalDefine,
					GrammarSymbol.nonterminalFunction,
					GrammarSymbol.nonterminalArgList,
					GrammarSymbol.nonterminalExpression,
					GrammarSymbol.terminalRightBracket,
					'#functionDefinition'
				],
				4
			)
		);

		// ArgList -> ( VariableList )
		// Productions.Add(new Production(Symbol.N_ArgList, new List<object>() { Symbol.T_LeftBracket, Symbol.N_VariableList, Symbol.T_RightBracket }, 5));
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalArgList,
				[
					GrammarSymbol.terminalLeftBracket,
					GrammarSymbol.nonterminalVariableList,
					GrammarSymbol.terminalRightBracket
				],
				7
			)
		);

		// VariableList -> Variable VariableList
		// Productions.Add(new Production(Symbol.N_VariableList, new List<object>() { Symbol.N_Variable, Symbol.N_VariableList, "#variableList" }, 6));
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalVariableList,
				[
					GrammarSymbol.nonterminalVariable,
					GrammarSymbol.nonterminalVariableList,
					'#variableList'
				],
				8
			)
		);

		// VariableList -> Lambda
		// Productions.Add(new Production(Symbol.N_VariableList, new List<object>() { Symbol.Lambda, "#emptyVariableList" }, 7));
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalVariableList,
				[GrammarSymbol.Lambda, '#emptyVariableList'],
				9
			)
		);

		// Expression -> Value
		// Productions.Add(new Production(Symbol.N_Expression, new List<object>() { Symbol.N_Value }, 8));
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalExpression,
				[GrammarSymbol.nonterminalValue],
				10
			)
		);

		// Expression -> Variable
		// Productions.Add(new Production(Symbol.N_Expression, new List<object>() { Symbol.N_Variable }, 9));
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalExpression,
				[GrammarSymbol.nonterminalVariable],
				11
			)
		);

		// Expression -> ( BracketedExpression )
		// Productions.Add(new Production(Symbol.N_Expression, new List<object>() { Symbol.T_LeftBracket, Symbol.N_BracketedExpression, Symbol.T_RightBracket }, 10));
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalExpression,
				[
					GrammarSymbol.terminalLeftBracket,
					GrammarSymbol.nonterminalBracketedExpression,
					GrammarSymbol.terminalRightBracket
				],
				12
			)
		);

		// BracketedExpression -> if Expression Expression Expression
		// Productions.Add(new Production(Symbol.N_BracketedExpression, new List<object>() { Symbol.T_If, Symbol.N_Expression, Symbol.N_Expression, Symbol.N_Expression, "#if" }, 11));
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
				13
			)
		);

		// BracketedExpression -> while Expression Expression
		// Productions.Add(new Production(Symbol.N_BracketedExpression, new List<object>() { Symbol.T_While, Symbol.N_Expression, Symbol.N_Expression, "#while" }, 12));
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalBracketedExpression,
				[
					GrammarSymbol.terminalWhile,
					GrammarSymbol.nonterminalExpression,
					GrammarSymbol.nonterminalExpression,
					'#while'
				],
				14
			)
		);

		// BracketedExpression -> set Variable Expression
		// Productions.Add(new Production(Symbol.N_BracketedExpression, new List<object>() { Symbol.T_Set, Symbol.N_Variable, Symbol.N_Expression, "#set" }, 13));
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalBracketedExpression,
				[
					GrammarSymbol.terminalSet,
					GrammarSymbol.nonterminalVariable,
					GrammarSymbol.nonterminalExpression,
					'#set'
				],
				15
			)
		);

		// BracketedExpression -> begin Expression ExpressionList
		// Productions.Add(new Production(Symbol.N_BracketedExpression, new List<object>() { Symbol.T_Begin, Symbol.N_Expression, Symbol.N_ExpressionList, "#begin" }, 14));
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalBracketedExpression,
				[
					GrammarSymbol.terminalBegin,
					GrammarSymbol.nonterminalExpression,
					GrammarSymbol.nonterminalExpressionList,
					'#begin'
				],
				16
			)
		);

		// BracketedExpression -> Optr ExpressionList
		// Productions.Add(new Production(Symbol.N_BracketedExpression, new List<object>() { Symbol.N_Optr, Symbol.N_ExpressionList, "#operatorUsage" }, 15));
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalBracketedExpression,
				[
					GrammarSymbol.nonterminalOptr,
					GrammarSymbol.nonterminalExpressionList,
					'#operatorUsage'
				],
				17
			)
		);

		// ExpressionList -> Expression ExpressionList
		// Productions.Add(new Production(Symbol.N_ExpressionList, new List<object>() { Symbol.N_Expression, Symbol.N_ExpressionList, "#expressionList" }, 16));
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalExpressionList,
				[
					GrammarSymbol.nonterminalExpression,
					GrammarSymbol.nonterminalExpressionList,
					'#expressionList'
				],
				18
			)
		);

		// ExpressionList -> Lambda
		// Productions.Add(new Production(Symbol.N_ExpressionList, new List<object>() { Symbol.Lambda, "#emptyExpressionList" }, 17));
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalExpressionList,
				[GrammarSymbol.Lambda, '#emptyExpressionList'],
				19
			)
		);

		// Optr -> Function
		// Productions.Add(new Production(Symbol.N_Optr, new List<object>() { Symbol.N_Function }, 18));
		this.productions.push(
			createProduction(GrammarSymbol.nonterminalOptr, [GrammarSymbol.nonterminalFunction], 20)
		);

		// Optr -> Value-Op
		// Productions.Add(new Production(Symbol.N_Optr, new List<object>() { Symbol.N_ValueOp }, 19));
		this.productions.push(
			createProduction(GrammarSymbol.nonterminalOptr, [GrammarSymbol.nonterminalValueOp], 21)
		);

		// Value -> Integer
		// Productions.Add(new Production(Symbol.N_Value, new List<object>() { Symbol.T_IntegerLiteral }, 20));
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalValue,
				[GrammarSymbol.terminalIntegerLiteral],
				22
			)
		);

		// Value-Op -> +
		// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Plus }, 21));
		this.productions.push(
			createProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalPlus], 23)
		);

		// Value-Op -> -
		// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Minus }, 22));
		this.productions.push(
			createProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalMinus], 24)
		);

		// Value-Op -> *
		// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Multiply }, 23));
		this.productions.push(
			createProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalMultiply], 25)
		);

		// Value-Op -> /
		// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Divide }, 24));
		this.productions.push(
			createProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalDivide], 26)
		);

		// Value-Op -> =
		// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Equals }, 25));
		this.productions.push(
			createProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalEquals], 27)
		);

		// Value-Op -> <
		// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_LessThan }, 26));
		this.productions.push(
			createProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalLessThan], 28)
		);

		// Value-Op -> >
		// //Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_GreaterThan }, 27));
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalValueOp,
				[GrammarSymbol.terminalGreaterThan],
				29
			)
		);

		// Value-Op -> print
		// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Print }, 28));
		this.productions.push(
			createProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalPrint], 30)
		);

		// Function -> Name
		// Productions.Add(new Production(Symbol.N_Function, new List<object>() { Symbol.T_ID }, 29));
		this.productions.push(
			createProduction(GrammarSymbol.nonterminalFunction, [GrammarSymbol.terminalID], 31)
		);

		// Variable -> Name
		// Productions.Add(new Production(Symbol.N_Variable, new List<object>() { Symbol.T_ID, "#variable" }, 30));
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalVariable,
				[GrammarSymbol.terminalID, '#variable'],
				32
			)
		);

		// Integer -> ...
		// Name -> ...

		// Value -> Quoted-Const
		// Productions.Add(new Production(Symbol.N_Value, new List<object>() { Symbol.N_QuotedConst }, 39));
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalValue,
				[GrammarSymbol.nonterminalQuotedConst],
				22
			)
		);

		// Value-Op -> cons
		// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Cons }, 40));
		this.productions.push(
			createProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalCons], 40)
		);

		// Value-Op -> car
		// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Car }, 41));
		this.productions.push(
			createProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalCar], 41)
		);

		// Value-Op -> cdr
		// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Cdr }, 42));
		this.productions.push(
			createProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalCdr], 42)
		);

		// Value-Op -> number?
		// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_NumberPred }, 43));
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalValueOp,
				[GrammarSymbol.terminalNumberPred],
				43
			)
		);

		// Value-Op -> symbol?
		// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_SymbolPred }, 44));
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalValueOp,
				[GrammarSymbol.terminalSymbolPred],
				44
			)
		);

		// Value-Op -> list?
		// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_ListPred }, 45));
		this.productions.push(
			createProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalListPred], 45)
		);

		// Value-Op -> null?
		// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_NullPred }, 46));
		this.productions.push(
			createProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalNullPred], 46)
		);

		// Quoted-Const -> ' S-Expression
		// Productions.Add(new Production(Symbol.N_QuotedConst, new List<object>() { Symbol.T_Apostrophe, Symbol.N_SExpression, "#quotedConstantWithApostrophe" }, 47));
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalQuotedConst,
				[
					GrammarSymbol.terminalApostrophe,
					GrammarSymbol.nonterminalSExpression,
					'#quotedConstantWithApostrophe'
				],
				47
			)
		);

		// S-Expression -> Integer
		// Productions.Add(new Production(Symbol.N_SExpression, new List<object>() { Symbol.T_IntegerLiteral }, 48));
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalSExpression,
				[GrammarSymbol.terminalIntegerLiteral],
				48
			)
		);

		// S-Expression -> Symbol
		// Productions.Add(new Production(Symbol.N_SExpression, new List<object>() { Symbol.N_Symbol }, 49));
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalSExpression,
				[GrammarSymbol.nonterminalSymbol],
				49
			)
		);

		// S-Expression -> ( S-Expression-List )
		// Productions.Add(new Production(Symbol.N_SExpression, new List<object>() { Symbol.T_LeftBracket, Symbol.N_SExpressionList, Symbol.T_RightBracket }, 50));
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalSExpression,
				[
					GrammarSymbol.terminalLeftBracket,
					GrammarSymbol.nonterminalSExpressionList,
					GrammarSymbol.terminalRightBracket
				],
				50
			)
		);

		// S-Expression-List -> S-Expression S-Expression-List
		// Productions.Add(new Production(Symbol.N_SExpressionList, new List<object>() { Symbol.N_SExpression, Symbol.N_SExpressionList, "#sExpressionList" }, 51));
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalSExpressionList,
				[
					GrammarSymbol.nonterminalSExpression,
					GrammarSymbol.nonterminalSExpressionList,
					'#sExpressionList'
				],
				51
			)
		);

		// S-Expression-List -> S-Expression . S-Expression
		// Productions.Add(new Production(Symbol.N_SExpressionList, new List<object>() { Symbol.N_SExpression, Symbol.T_Dot, Symbol.N_SExpression, "#sExpressionList" }, 52));
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalSExpressionList,
				[
					GrammarSymbol.nonterminalSExpression,
					GrammarSymbol.terminalDot,
					GrammarSymbol.nonterminalSExpression,
					'#sExpressionList'
				],
				52
			)
		);

		// S-Expression-List -> Lambda
		// Productions.Add(new Production(Symbol.N_SExpressionList, new List<object>() { Symbol.Lambda, "#emptySExpressionList" }, 53));
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalSExpressionList,
				[GrammarSymbol.Lambda, '#emptySExpressionList'],
				53
			)
		);

		// Symbol -> Name
		// Productions.Add(new Production(Symbol.N_Symbol, new List<object>() { Symbol.T_ID, "#symbol" }, 54));
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalSymbol,
				[GrammarSymbol.terminalID, '#symbol'],
				54
			)
		);

		// BracketedExpression -> cond ( Expression Expression ) ExprPairList
		// Productions.Add(new Production(Symbol.N_BracketedExpression, new List<object>() {
		//     Symbol.T_Cond,
		//     Symbol.T_LeftBracket,
		//     Symbol.N_Expression,
		//     Symbol.N_Expression,
		//     Symbol.T_RightBracket,
		//     Symbol.N_ExprPairList, "#condUsage" }, 31));
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
				54
			)
		);

		// ExprPairList -> ( Expression Expression ) ExprPairList
		// Productions.Add(new Production(Symbol.N_ExprPairList, new List<object>() {
		//     Symbol.T_LeftBracket,
		//     Symbol.N_Expression,
		//     Symbol.N_Expression,
		//     Symbol.T_RightBracket,
		//     Symbol.N_ExprPairList, "#exprPairList" }, 32));
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalExprPairList,
				[
					GrammarSymbol.terminalLeftBracket,
					GrammarSymbol.nonterminalExpression,
					GrammarSymbol.nonterminalExpression,
					GrammarSymbol.terminalRightBracket,
					GrammarSymbol.nonterminalExprPairList
				],
				54
			)
		);

		// ExprPairList -> Lambda
		// Productions.Add(new Production(Symbol.N_ExprPairList, new List<object>() { Symbol.Lambda, "#emptyExprPairList" }, 33));
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalExprPairList,
				[GrammarSymbol.Lambda, '#emptyExprPairList'],
				54
			)
		);

		// Value-Op -> list
		// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_List }, 55));
		this.productions.push(
			createProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalList], 55)
		);

		// **** BEGIN : Probably deletia ****
		// Productions.Add(new Production(Symbol.N_Value, new List<object>() { Symbol.T_StringLiteral }, 75)); // Note: Number is out of order
		// Productions.Add(new Production(Symbol.N_Value, new List<object>() { Symbol.T_FloatLiteral }, 79)); // Note: Number is out of order
		// **** END : Probably deletia ****

		// **** BEGIN : Ignore for now ****
		// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_StringPred }, 73)); // Note: Number is out of order
		// Productions.Add(new Production(Symbol.N_SExpression, new List<object>() { Symbol.T_StringLiteral }, 74)); // Note: Number is out of order
		// Productions.Add(new Production(Symbol.N_SExpression, new List<object>() { Symbol.T_FloatLiteral }, 80)); // Note: Number is out of order

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
		// **** END : Ignore for now ****

		// **** BEGIN : New and fscked ****
		// this.productions.push(new Production(Symbol.nonterminalInput, [
		// 	Symbol.terminalLeftBracket,
		// 	Symbol.nonterminalBracketedEntity,
		// 	Symbol.terminalRightBracket
		// ], 3));
		// this.productions.push(new Production(Symbol.nonterminalBracketedEntity, [Symbol.nonterminalBracketedExpression], 4));
		// this.productions.push(new Production(Symbol.nonterminalBracketedEntity, [Symbol.nonterminalFunDef], 5));
		// this.productions.push(new Production(Symbol.nonterminalFunDef,
		// 	[
		// 		Symbol.terminalDefine,
		// 		Symbol.nonterminalFunction,
		// 		Symbol.nonterminalArgList,
		// 		Symbol.nonterminalExpression,
		// 		'#functionDefinition'
		// 	], 6));
		// **** END : New and fscked ****

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
		// this.productions.push(new Production(
		// 	Symbol.nonterminalSExpression,
		// 	[
		// 		Symbol.terminalLeftBracket,
		// 		Symbol.terminalQuoteKeyword,
		// 		Symbol.nonterminalSExpression,
		// 		Symbol.terminalRightBracket,
		// 		'#quotedConstantWithQuoteKeyword'
		// 	],
		// 	60));
		// Productions.Add(new Production(Symbol.N_QuotedConst, new List<object>() { Symbol.T_LeftBracket, Symbol.T_QuoteKeyword, Symbol.N_SExpression, Symbol.T_RightBracket, '#quotedConstantWithQuoteKeyword' }, 62)); // Needed for macros.
		// Next number is 93.
	}

	public get languageName(): string {
		return 'LISP';
	}

	public override get defaultParser(): ParserSelector {
		return ParserSelector.SLR1;
	}

	public override get selectorsOfCompatibleParsers(): ParserSelector[] {
		return [ParserSelector.LR1, ParserSelector.SLR1, ParserSelector.LALR1];
	}

	public executeSemanticAction(semanticStack: SemanticStackType, action: string): void {
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

	public tokenToSymbol(token: IToken): GrammarSymbol {
		const tokenValueAsString: string = token.tokenValue as string;

		switch (token.tokenType) {
			case LexicalState.tokenEOF:
				return GrammarSymbol.terminalEOF;
			case LexicalState.tokenIntLit:
				return GrammarSymbol.terminalIntegerLiteral;
			case LexicalState.tokenFltLit:
				return GrammarSymbol.terminalFloatLiteral;
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
				return GrammarSymbol.terminalGreaterThan;
			case LexicalState.tokenApostrophe:
				return GrammarSymbol.terminalApostrophe;
			case LexicalState.tokenQuoteKeyword:
				return GrammarSymbol.terminalQuoteKeyword;

			case LexicalState.tokenIdent:
				switch (tokenValueAsString) {
					// Quoting never changes tokens with these values into IDs.
					case '.':
						return GrammarSymbol.terminalDot; // We could modify the tokenizer to generate TokenType.T_Dot in this case, to obviate this line.
					case 'quote':
						return GrammarSymbol.terminalQuoteKeyword;
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
					default:
						return GrammarSymbol.terminalID;
				}

			// break;

			default:
				break;
		}

		throw new GrammarException(
			`No grammar symbol matches token ${token.tokenType} ${token.tokenValue}`,
			token.line,
			token.column
		);
	}

	public pushTokenOntoSemanticStack(
		semanticStack: SemanticStackType,
		tokenAsSymbol: GrammarSymbol,
		token: IToken
	): void {
		const value = token.tokenValue;

		switch (tokenAsSymbol) {
			case GrammarSymbol.terminalID:
			case GrammarSymbol.terminalPrint:
			case GrammarSymbol.terminalPlus:
			case GrammarSymbol.terminalMinus:
			case GrammarSymbol.terminalMultiply:
			case GrammarSymbol.terminalDivide:
			case GrammarSymbol.terminalEquals:
			case GrammarSymbol.terminalLessThan:
			case GrammarSymbol.terminalGreaterThan:
			case GrammarSymbol.terminalCons:
			case GrammarSymbol.terminalCar:
			case GrammarSymbol.terminalCdr:
			case GrammarSymbol.terminalNumberPred:
			case GrammarSymbol.terminalSymbolPred:
			case GrammarSymbol.terminalListPred:
			case GrammarSymbol.terminalNullPred:
			case GrammarSymbol.terminalStringPred:
			case GrammarSymbol.terminalList:
			case GrammarSymbol.terminalRplaca:
			case GrammarSymbol.terminalRplacd:
			case GrammarSymbol.terminalRandom:
			case GrammarSymbol.terminalToString:
			case GrammarSymbol.terminalListToString:
			case GrammarSymbol.terminalStringToList:
			case GrammarSymbol.terminalStringToSymbol:
			case GrammarSymbol.terminalPow:
			case GrammarSymbol.terminalExp:
			case GrammarSymbol.terminalLn:
			case GrammarSymbol.terminalSin:
			case GrammarSymbol.terminalCos:
			case GrammarSymbol.terminalTan:
			case GrammarSymbol.terminalAtan2:
			case GrammarSymbol.terminalFloor:
			case GrammarSymbol.terminalThrow:
			case GrammarSymbol.terminalStringLessThan:
				semanticStack.push(new Name(value as string, token.line, token.column));
				break;

			case GrammarSymbol.terminalIntegerLiteral:
				semanticStack.push(new IntegerLiteral(value));
				break;

			case GrammarSymbol.terminalFloatLiteral:
				semanticStack.push(new FloatLiteral(value));
				break;

			case GrammarSymbol.terminalStringLiteral:
				semanticStack.push(new LISPString(value as string));
				break;

			case GrammarSymbol.terminalLeftBracket:
			case GrammarSymbol.terminalRightBracket:
			case GrammarSymbol.terminalApostrophe:
			case GrammarSymbol.terminalQuoteKeyword:
			case GrammarSymbol.terminalDefine:
			case GrammarSymbol.terminalIf:
			case GrammarSymbol.terminalWhile:
			case GrammarSymbol.terminalSet:
			case GrammarSymbol.terminalBegin:
			case GrammarSymbol.terminalEOF:
				break;

			default:
				throw new GrammarException(
					`pushTokenOntoSemanticStack() : Unexpected tokenAsSymbol ${GrammarSymbol[tokenAsSymbol]} (${tokenAsSymbol})`,
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
