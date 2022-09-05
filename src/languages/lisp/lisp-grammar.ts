// tom-weatherhead/thaw-grammar/src/languages/lisp/lisp-grammar.ts

// On macOS, to install a standard Lisp: $ brew install clisp

// From https://theory.stanford.edu/~amitp/yapps/yapps-doc/node2.html

// expr:   ID | STR | NUM | list
// list:   ( seq )
// seq:    lambda | expr seq

import { GrammarSymbol, IToken, LexicalState, SemanticStackType } from 'thaw-interpreter-types';

import { ArgumentException, createProduction, Name } from 'thaw-interpreter-core';

import { IExpression } from '../../common/domain-object-model/iexpression';
import { IVariable, Variable } from '../../common/domain-object-model/variable';

import { BeginUsage } from '../../common/domain-object-model/begin-usage';
import { CondUsage } from '../../common/domain-object-model/cond-usage';
import { FunctionDefinition } from '../../common/domain-object-model/function-definition';
import { IfUsage } from '../../common/domain-object-model/if-usage';
import { LetStarUsage } from '../../common/domain-object-model/let-star-usage';
import { LetUsage } from '../../common/domain-object-model/let-usage';
import { SetUsage } from '../../common/domain-object-model/set-usage';
import { WhileUsage } from '../../common/domain-object-model/while-usage';

import { GrammarBase, GrammarException } from 'thaw-interpreter-core';

import { FloatLiteral } from './domain-object-model/float-literal';
import { IntegerLiteral } from './domain-object-model/integer-literal';
import { ISExpression } from './domain-object-model/isexpression';
import { LISPOperatorUsage } from './domain-object-model/lisp-operator-usage';
import { LISPString } from './domain-object-model/lisp-string';
import { LISPSymbol } from './domain-object-model/lisp-symbol';
import { MacroDefinition } from './domain-object-model/macro-definition';
import { NullSExpression } from './domain-object-model/null-sexpression';
import { QuotedConstantWithApostrophe } from './domain-object-model/quoted-constant-with-apostrophe';
import { QuotedConstantWithQuoteKeyword } from './domain-object-model/quoted-constant-with-quote-keyword';
import { SExpressionList } from './domain-object-model/sexpression-list';

export class LISPGrammar extends GrammarBase {
	// The LISP grammar from Kamin (the book 'Programming Languages: An Interpreter-Based Approach')

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
		this.terminals.push(GrammarSymbol.terminalFloatLiteral);
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
		this.terminals.push(GrammarSymbol.terminalDefineMacro);
		this.terminals.push(GrammarSymbol.terminalQuoteKeyword);
		this.terminals.push(GrammarSymbol.terminalStringLiteral);
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
		this.terminals.push(GrammarSymbol.terminalStringLessThan);
		// this.terminals.push(GrammarSymbol.terminalRandom);
		this.terminals.push(GrammarSymbol.terminalThrow);
		this.terminals.push(GrammarSymbol.terminalLet);
		this.terminals.push(GrammarSymbol.terminalLetStar);
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
		this.nonTerminals.push(GrammarSymbol.nonterminalSExpressionListTail);
		this.nonTerminals.push(GrammarSymbol.nonterminalSymbol);
		this.nonTerminals.push(GrammarSymbol.nonterminalExprPairList);
		// this.nonTerminals.push(GrammarSymbol.nonterminalMacroDef);

		// this.nonTerminals.push(GrammarSymbol.nonterminalBracketedEntity);
		this.nonTerminals.push(GrammarSymbol.nonterminalBracketedInput);
		this.nonTerminals.push(GrammarSymbol.nonterminalUnbracketedInput);

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
			createProduction(
				GrammarSymbol.nonterminalStart,
				[GrammarSymbol.nonterminalInput, GrammarSymbol.terminalEOF],
				1
			)
		);

		// Input -> ( BracketedInput )
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalInput,
				[
					GrammarSymbol.terminalLeftBracket,
					GrammarSymbol.nonterminalBracketedInput,
					GrammarSymbol.terminalRightBracket
				],
				2
			)
		);

		// Input -> UnbracketedInput
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalInput,
				[GrammarSymbol.nonterminalUnbracketedInput],
				3
			)
		);

		// BracketedInput -> BracketedExpression
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalBracketedInput,
				[GrammarSymbol.nonterminalBracketedExpression],
				4
			)
		);

		// BracketedInput -> FunDef
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalBracketedInput,
				[GrammarSymbol.nonterminalFunDef],
				5
			)
		);

		// - UnbracketedInput -> Value
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalUnbracketedInput,
				[GrammarSymbol.nonterminalValue],
				6
			)
		);

		// - UnbracketedInput -> Variable
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalUnbracketedInput,
				[GrammarSymbol.nonterminalVariable],
				7
			)
		);

		// FunDef -> define Function ArgList Expression
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalFunDef,
				[
					GrammarSymbol.terminalDefine,
					GrammarSymbol.nonterminalFunction,
					GrammarSymbol.nonterminalArgList,
					GrammarSymbol.nonterminalExpression,
					'#functionDefinition'
				],
				8
			)
		);

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

		// BracketedExpression -> Optr ExpressionList
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalBracketedExpression,
				[
					GrammarSymbol.nonterminalOptr,
					GrammarSymbol.nonterminalExpressionList,
					'#operatorUsage'
				],
				19
			)
		);

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

		// Optr -> Function
		this.productions.push(
			createProduction(GrammarSymbol.nonterminalOptr, [GrammarSymbol.nonterminalFunction], 22)
		);

		// Optr -> Value-Op
		this.productions.push(
			createProduction(GrammarSymbol.nonterminalOptr, [GrammarSymbol.nonterminalValueOp], 23)
		);

		// Value -> Integer
		this.addProduction(GrammarSymbol.nonterminalValue, [GrammarSymbol.terminalIntegerLiteral]);

		this.addProduction(GrammarSymbol.nonterminalValue, [GrammarSymbol.terminalFloatLiteral]);

		this.addProduction(GrammarSymbol.nonterminalValue, [GrammarSymbol.terminalStringLiteral]);

		// Value-Op -> +
		this.addProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalPlus]);

		// Value-Op -> -
		this.addProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalMinus]);

		// Value-Op -> *
		this.addProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalMultiply]);

		// Value-Op -> /
		this.addProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalDivide]);

		// Value-Op -> =
		this.addProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalEquals]);

		// Value-Op -> <
		this.addProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalLessThan]);

		// Value-Op -> >
		this.addProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalGreaterThan]);

		// Value-Op -> print
		this.addProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalPrint]);

		this.addProduction(GrammarSymbol.nonterminalValueOp, [
			GrammarSymbol.terminalStringLessThan
		]);

		this.addProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalThrow]);

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
			createProduction(
				GrammarSymbol.nonterminalValue,
				[GrammarSymbol.nonterminalQuotedConst],
				35
			)
		);

		// Value-Op -> cons
		this.addProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalCons]);

		// Value-Op -> car
		this.addProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalCar]);

		// Value-Op -> cdr
		this.addProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalCdr]);

		// Value-Op -> number?
		this.addProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalNumberPred]);

		// Value-Op -> symbol?
		this.addProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalSymbolPred]);

		// Value-Op -> list?
		this.addProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalListPred]);

		// Value-Op -> null?
		this.addProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalNullPred]);

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

		// GrammarSymbol -> Name
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalSymbol,
				[GrammarSymbol.terminalID, '#symbol'],
				51
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

		// Value-Op -> list
		this.productions.push(
			createProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalList], 55)
		);

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

		// Old
		this.productions.push(
			createProduction(
				GrammarSymbol.nonterminalBracketedInput,
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

		// this.productions.push(createProduction(
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

	// eslint-disable-next-line @typescript-eslint/no-explicit-any
	public executeSemanticAction(semanticStack: SemanticStackType, action: string): void {
		// console.log(`LISPGrammar.executeSemanticAction() : action is ${typeof action} ${action}`);

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
			case '#functionDefinition':
				expression = semanticStack.pop() as IExpression<ISExpression>; // The function's body
				variableList = semanticStack.pop() as IVariable<ISExpression>[]; // The function's formal argument list
				name = semanticStack.pop() as Name; // The function name
				semanticStack.push(
					new FunctionDefinition<ISExpression>(name, variableList, expression)
				); // Add line and column?
				break;

			case '#variableList':
				variableList = semanticStack.pop() as IVariable<ISExpression>[];
				variable = semanticStack.pop() as Variable<ISExpression>;
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

			case '#operatorUsage':
				expressionList = semanticStack.pop() as IExpression<ISExpression>[];
				name = semanticStack.pop() as Name;

				if (name.value.length > 0 && name.value[0] === "'") {
					// TODO: Instead of handling the apostrophe like this here,
					// we should be using token.isQuoted in tokenToSymbol() below.
					name = new Name(name.value.substring(1));
				}

				semanticStack.push(new LISPOperatorUsage(name, expressionList));
				break;

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

			default:
				throw new GrammarException(`Unrecognized semantic action: ${action}`);
		}
	}

	public override tokenToSymbol(token: IToken): GrammarSymbol {
		const tokenValueAsString: string = token.tokenValue as string;

		if (
			token.isQuoted ||
			[LexicalState.tokenLessEqual, LexicalState.tokenGreaterEqual].indexOf(
				token.tokenType
			) >= 0
		) {
			token.tokenType = LexicalState.tokenIdent;
		}

		switch (token.tokenType) {
			case LexicalState.tokenQuoteKeyword:
				return GrammarSymbol.terminalQuoteKeyword;
			case LexicalState.tokenStrLit:
				return GrammarSymbol.terminalStringLiteral;

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
					case 'tostring':
						return GrammarSymbol.terminalToString;
					case 'listtostring':
						return GrammarSymbol.terminalListToString;
					case 'stringtolist':
						return GrammarSymbol.terminalStringToList;
					case 'stringtosymbol':
						return GrammarSymbol.terminalStringToSymbol;
					case 'atan2':
						return GrammarSymbol.terminalAtan2;
					case 'floor':
						return GrammarSymbol.terminalFloor;
					case 'throw':
						return GrammarSymbol.terminalThrow;
					case 'string<':
						return GrammarSymbol.terminalStringLessThan;
					// random, pow, exp, ln, sin, cos, tan
					default:
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
		tokenAsSymbol: GrammarSymbol,
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
			case GrammarSymbol.terminalStringPred:
			case GrammarSymbol.terminalList:
			case GrammarSymbol.terminalRplaca:
			case GrammarSymbol.terminalRplacd:
			case GrammarSymbol.terminalToString:
			case GrammarSymbol.terminalListToString:
			case GrammarSymbol.terminalStringToList:
			case GrammarSymbol.terminalStringToSymbol:
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

			case GrammarSymbol.terminalApostrophe:
			case GrammarSymbol.terminalDefineMacro:
			case GrammarSymbol.terminalQuoteKeyword:
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
