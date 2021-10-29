// tom-weatherhead/thaw-grammar/src/languages/clu/clu-grammar.ts

import {
	GrammarSymbol,
	IToken,
	// LanguageSelector,
	LexicalState,
	// ParserSelector,
	SemanticStackType
} from 'thaw-interpreter-types';

import { GrammarBase, GrammarException } from 'thaw-interpreter-core';

export class CluGrammar extends GrammarBase {
	// The CLU grammar from Kamin (the book 'Programming Languages: An Interpreter-Based Approach')

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
		// this.terminals.push(GrammarSymbol.terminalCond);
		// this.terminals.push(GrammarSymbol.terminalLet);
		// this.terminals.push(GrammarSymbol.terminalLetStar);

		// this.terminals.push(GrammarSymbol.terminalDoubleSubscripting);
		// this.terminals.push(GrammarSymbol.terminalFloatLiteral);
		// this.terminals.push(GrammarSymbol.terminalRandom);
		// this.terminals.push(GrammarSymbol.terminalPow);
		// this.terminals.push(GrammarSymbol.terminalExp);
		// this.terminals.push(GrammarSymbol.terminalLn);
		// this.terminals.push(GrammarSymbol.terminalSin);
		// this.terminals.push(GrammarSymbol.terminalCos);
		// this.terminals.push(GrammarSymbol.terminalTan);

		this.terminals.push(GrammarSymbol.terminalEOF);

		this.nonTerminals.push(GrammarSymbol.nonterminalStart);
		this.nonTerminals.push(GrammarSymbol.nonterminalInput);
		this.nonTerminals.push(GrammarSymbol.nonterminalBracketedInput);
		this.nonTerminals.push(GrammarSymbol.nonterminalUnbracketedInput);
		this.nonTerminals.push(GrammarSymbol.nonterminalExpression);
		this.nonTerminals.push(GrammarSymbol.nonterminalBracketedExpression);
		this.nonTerminals.push(GrammarSymbol.nonterminalValue);
		this.nonTerminals.push(GrammarSymbol.nonterminalVariable);
		this.nonTerminals.push(GrammarSymbol.nonterminalVariableList);
		this.nonTerminals.push(GrammarSymbol.nonterminalFunDef);
		this.nonTerminals.push(GrammarSymbol.nonterminalFunction);
		this.nonTerminals.push(GrammarSymbol.nonterminalArgList);
		this.nonTerminals.push(GrammarSymbol.nonterminalExpressionList);
		this.nonTerminals.push(GrammarSymbol.nonterminalOptr);
		this.nonTerminals.push(GrammarSymbol.nonterminalValueOp);
		// this.nonTerminals.push(GrammarSymbol.nonterminalExprPairList);
		// this.nonTerminals.push(GrammarSymbol.nonterminalLetKeyword);
		// this.nonTerminals.push(GrammarSymbol.nonterminalVarExprList);

		// This initial production needed to be added: Start -> Input EOF
		this.addProduction(GrammarSymbol.nonterminalStart, [
			GrammarSymbol.nonterminalInput,
			GrammarSymbol.terminalEOF
		]);

		// Input -> ( BracketedInput )
		this.addProduction(GrammarSymbol.nonterminalInput, [
			GrammarSymbol.terminalLeftBracket,
			GrammarSymbol.nonterminalBracketedInput,
			GrammarSymbol.terminalRightBracket
		]);

		// Input -> UnbracketedInput
		this.addProduction(GrammarSymbol.nonterminalInput, [
			GrammarSymbol.nonterminalUnbracketedInput
		]);

		// BracketedInput -> BracketedExpression
		this.addProduction(GrammarSymbol.nonterminalBracketedInput, [
			GrammarSymbol.nonterminalBracketedExpression
		]);

		// BracketedInput -> FunDef
		this.addProduction(GrammarSymbol.nonterminalBracketedInput, [
			GrammarSymbol.nonterminalFunDef
		]);

		// - UnbracketedInput -> Value
		this.addProduction(GrammarSymbol.nonterminalUnbracketedInput, [
			GrammarSymbol.nonterminalValue
		]);

		// - UnbracketedInput -> Variable
		this.addProduction(GrammarSymbol.nonterminalUnbracketedInput, [
			GrammarSymbol.nonterminalVariable
		]);

		// FunDef -> define Function ArgList Expression
		this.addProduction(GrammarSymbol.nonterminalFunDef, [
			GrammarSymbol.terminalDefine,
			GrammarSymbol.nonterminalFunction,
			GrammarSymbol.nonterminalArgList,
			GrammarSymbol.nonterminalExpression,
			'#functionDefinition'
		]);

		// Expression -> Value
		this.addProduction(GrammarSymbol.nonterminalExpression, [GrammarSymbol.nonterminalValue]);

		// Expression -> Variable
		this.addProduction(GrammarSymbol.nonterminalExpression, [
			GrammarSymbol.nonterminalVariable
		]);

		// Expression -> ( BracketedExpression )
		this.addProduction(GrammarSymbol.nonterminalExpression, [
			GrammarSymbol.terminalLeftBracket,
			GrammarSymbol.nonterminalBracketedExpression,
			GrammarSymbol.terminalRightBracket
		]);

		this.addProduction(GrammarSymbol.nonterminalArgList, [
			GrammarSymbol.terminalLeftBracket,
			GrammarSymbol.nonterminalVariableList,
			GrammarSymbol.terminalRightBracket
		]);

		this.addProduction(GrammarSymbol.nonterminalVariableList, [
			GrammarSymbol.nonterminalVariable,
			GrammarSymbol.nonterminalVariableList,
			'#variableList'
		]);

		this.addProduction(GrammarSymbol.nonterminalVariableList, [
			GrammarSymbol.Lambda,
			'#emptyVariableList'
		]);

		this.addProduction(GrammarSymbol.nonterminalBracketedExpression, [
			GrammarSymbol.terminalIf,
			GrammarSymbol.nonterminalExpression,
			GrammarSymbol.nonterminalExpression,
			GrammarSymbol.nonterminalExpression,
			'#if'
		]);

		this.addProduction(GrammarSymbol.nonterminalBracketedExpression, [
			GrammarSymbol.terminalWhile,
			GrammarSymbol.nonterminalExpression,
			GrammarSymbol.nonterminalExpression,
			'#while'
		]);

		this.addProduction(GrammarSymbol.nonterminalBracketedExpression, [
			GrammarSymbol.terminalSet,
			GrammarSymbol.nonterminalVariable,
			GrammarSymbol.nonterminalExpression,
			'#set'
		]);

		this.addProduction(GrammarSymbol.nonterminalBracketedExpression, [
			GrammarSymbol.terminalBegin,
			GrammarSymbol.nonterminalExpression,
			GrammarSymbol.nonterminalExpressionList,
			'#begin'
		]);

		this.addProduction(GrammarSymbol.nonterminalBracketedExpression, [
			GrammarSymbol.nonterminalOptr,
			GrammarSymbol.nonterminalExpressionList,
			'#operatorUsage'
		]);

		this.addProduction(GrammarSymbol.nonterminalExpressionList, [
			GrammarSymbol.nonterminalExpression,
			GrammarSymbol.nonterminalExpressionList,
			'#expressionList'
		]);

		this.addProduction(GrammarSymbol.nonterminalExpressionList, [
			GrammarSymbol.Lambda,
			'#emptyExpressionList'
		]);

		this.addProduction(GrammarSymbol.nonterminalOptr, [GrammarSymbol.nonterminalFunction]);

		this.addProduction(GrammarSymbol.nonterminalOptr, [GrammarSymbol.nonterminalValueOp]);

		this.addProduction(GrammarSymbol.nonterminalValue, [GrammarSymbol.terminalIntegerLiteral]);

		this.addProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalPlus]);

		this.addProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalMinus]);

		this.addProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalMultiply]);

		this.addProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalDivide]);

		this.addProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalEquals]);

		this.addProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalLessThan]);

		this.addProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalGreaterThan]);

		this.addProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalPrint]);

		this.addProduction(GrammarSymbol.nonterminalFunction, [GrammarSymbol.terminalID]);

		this.addProduction(GrammarSymbol.nonterminalVariable, [
			GrammarSymbol.terminalID,
			'#variable'
		]);

		// cond

		// Productions.Add(new Production(Symbol.N_BracketedExpression, new List<object>() {
		//     GrammarSymbol.terminalCond,
		//     GrammarSymbol.terminalLeftBracket,
		//     Symbol.N_Expression,
		//     Symbol.N_Expression,
		//     GrammarSymbol.terminalRightBracket,
		//     Symbol.N_ExprPairList, "#condUsage" }, 31));
		// Productions.Add(new Production(Symbol.N_ExprPairList, new List<object>() {
		//     GrammarSymbol.terminalLeftBracket,
		//     Symbol.N_Expression,
		//     Symbol.N_Expression,
		//     GrammarSymbol.terminalRightBracket,
		//     Symbol.N_ExprPairList, "#exprPairList" }, 32));
		// Productions.Add(new Production(Symbol.N_ExprPairList, new List<object>() { Symbol.Lambda, "#emptyExprPairList" }, 33));

		// let and let*

		// Productions.Add(new Production(Symbol.N_BracketedExpression, new List<object>() {
		//     Symbol.N_LetKeyword,
		//     GrammarSymbol.terminalLeftBracket,
		//     Symbol.N_VarExprList,
		//     GrammarSymbol.terminalRightBracket,
		//     Symbol.N_Expression, "#letUsage" }, 34));
		// Productions.Add(new Production(Symbol.N_LetKeyword, new List<object>() { GrammarSymbol.terminalLet }, 35));
		// Productions.Add(new Production(Symbol.N_LetKeyword, new List<object>() { GrammarSymbol.terminalLetStar }, 36));
		// Productions.Add(new Production(Symbol.N_VarExprList, new List<object>() {
		//     GrammarSymbol.terminalLeftBracket,
		//     Symbol.N_Variable,
		//     Symbol.N_Expression,
		//     GrammarSymbol.terminalRightBracket,
		//     Symbol.N_VarExprList, "#varExprList" }, 37));
		// Productions.Add(new Production(Symbol.N_VarExprList, new List<object>() { Symbol.Lambda, "#emptyVarExprList" }, 38));

		// CLU Productions

		// Productions.RemoveAt(18 - 1);   // Remove this production: Optr -> Function
		//
		// Terminals.UnionWith(new HashSet<Symbol>() {
		// 	Symbol.T_Cluster, Symbol.T_Rep, Symbol.T_Dollar, Symbol.T_Export });
		//
		// NonTerminals.UnionWith(new HashSet<Symbol>() {
		// 	Symbol.N_ClusterDef, /* Symbol.N_Cluster, */ Symbol.N_Rep,
		// 	Symbol.N_FunDefList, Symbol.N_OnePartName, Symbol.N_TwoPartName,
		// 	Symbol.N_ExportList, Symbol.N_OnePartNameList });
		//
		// Productions.Add(new Production(Symbol.N_Input, new List<object>() { Symbol.N_ClusterDef }, 39));
		// Productions.Add(new Production(Symbol.N_ClusterDef, new List<object>() {
		// 	Symbol.T_LeftBracket,
		// 	Symbol.T_Cluster,
		// 	Symbol.T_ID, // This T_ID is really a Symbol.N_Cluster
		// 	Symbol.N_ExportList,
		// 	Symbol.N_Rep,
		// 	Symbol.N_FunDef,
		// 	Symbol.N_FunDefList,
		// 	Symbol.T_RightBracket, "#clusterDefinition" }, 40));
		// Productions.Add(new Production(Symbol.N_Rep, new List<object>() {
		// 	Symbol.T_LeftBracket,
		// 	Symbol.T_Rep,
		// 	Symbol.N_Variable,
		// 	Symbol.N_VariableList,
		// 	Symbol.T_RightBracket, "#variableList" }, 41));
		// Productions.Add(new Production(Symbol.N_FunDefList, new List<object>() { Symbol.N_FunDef, Symbol.N_FunDefList, "#funDefList" }, 42));
		// Productions.Add(new Production(Symbol.N_FunDefList, new List<object>() { Symbol.Lambda, "#emptyFunDefList" }, 43));
		// Productions.Add(new Production(Symbol.N_Optr, new List<object>() { Symbol.N_OnePartName }, 44));
		// Productions.Add(new Production(Symbol.N_Optr, new List<object>() { Symbol.N_TwoPartName }, 45));
		// Productions.Add(new Production(Symbol.N_OnePartName, new List<object>() { Symbol.T_ID }, 46));
		// Productions.Add(new Production(Symbol.N_TwoPartName, new List<object>() {
		// 	Symbol.T_ID, // This T_ID is really a Symbol.N_Cluster
		// 	Symbol.T_Dollar,
		// 	Symbol.T_ID, "#makeTwoPartName" }, 47));
		// // SLR(1): There was a reduce-reduce conflict between N_OnePartName -> T_ID and N_Cluster -> T_ID.
		// //Productions.Add(new Production(Symbol.N_Cluster, new List<object>() { Symbol.T_ID }, 48));
		// Productions.Add(new Production(Symbol.N_ExportList, new List<object>() {
		// 	Symbol.T_LeftBracket,
		// 	Symbol.T_Export,
		// 	Symbol.N_OnePartName,
		// 	Symbol.N_OnePartNameList,
		// 	Symbol.T_RightBracket, "#exportList" }, 49));
		// Productions.Add(new Production(Symbol.N_OnePartNameList, new List<object>() { Symbol.N_OnePartName, Symbol.N_OnePartNameList, "#exportList" }, 50));
		// Productions.Add(new Production(Symbol.N_OnePartNameList, new List<object>() { Symbol.Lambda, "#emptyExportList" }, 51));
	}

	public get languageName(): string {
		return 'Clu';
	}

	// eslint-disable-next-line @typescript-eslint/no-explicit-any, @typescript-eslint/no-unused-vars
	public executeSemanticAction(semanticStack: SemanticStackType, action: string): void {
		throw new Error('CLUGrammar.executeSemanticAction() : Not yet implemented.');
	}

	// eslint-disable-next-line @typescript-eslint/no-unused-vars
	public tokenToSymbol(token: IToken): GrammarSymbol {
		const tokenValueAsString: string = token.tokenValue as string;

		switch (token.tokenType) {
			case LexicalState.tokenEOF:
				return GrammarSymbol.terminalEOF;
			case LexicalState.tokenLeftBracket:
				return GrammarSymbol.terminalLeftBracket;
			case LexicalState.tokenRightBracket:
				return GrammarSymbol.terminalRightBracket;
			case LexicalState.tokenEqual:
				return GrammarSymbol.terminalEquals;
			case LexicalState.tokenLess:
				return GrammarSymbol.terminalLessThan;
			case LexicalState.tokenGreater:
				return GrammarSymbol.terminalGreaterThan;
			case LexicalState.tokenIntLit:
				return GrammarSymbol.terminalIntegerLiteral;
			case LexicalState.tokenPlus:
				return GrammarSymbol.terminalPlus;
			case LexicalState.tokenMinus:
				return GrammarSymbol.terminalMinus;
			case LexicalState.tokenMult:
				return GrammarSymbol.terminalMultiply;
			case LexicalState.tokenDiv:
				return GrammarSymbol.terminalDivide;
			case LexicalState.tokenOctothorpe:
				return GrammarSymbol.terminalOctothorpe;
			case LexicalState.tokenDollar:
				return GrammarSymbol.terminalDollar;
			case LexicalState.tokenIdent:
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
					case '+':
						return GrammarSymbol.terminalPlus;
					case '-':
						return GrammarSymbol.terminalMinus;
					case '*':
						return GrammarSymbol.terminalMultiply;
					case '/':
						return GrammarSymbol.terminalDivide;
					case '=':
						return GrammarSymbol.terminalEquals;
					case '<':
						return GrammarSymbol.terminalLessThan;
					case 'cond':
						return GrammarSymbol.terminalCond;
					case 'let':
						return GrammarSymbol.terminalLet;
					case 'let*':
						return GrammarSymbol.terminalLetStar;
					default:
						return GrammarSymbol.terminalID;
				}

			default:
				throw new GrammarException(
					`CLUGrammar.tokenToSymbol() : No grammar symbol matches token ${
						token.tokenType
					} ${LexicalState[token.tokenType]} (value '${token.tokenValue}')`,
					token.line,
					token.column
				);
		}
	}

	/* eslint-disable @typescript-eslint/no-unused-vars */
	public pushTokenOntoSemanticStack(
		// eslint-disable-next-line @typescript-eslint/no-explicit-any
		semanticStack: SemanticStackType,
		tokenAsSymbol: number,
		token: IToken
	): void {
		throw new Error('CLUGrammar.pushTokenOntoSemanticStack() : Not yet implemented.');
	}
	/* eslint-enable @typescript-eslint/no-unused-vars */
}
