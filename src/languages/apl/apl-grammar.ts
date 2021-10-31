// tom-weatherhead/thaw-grammar/src/languages/apl/apl-grammar.ts

import { GrammarSymbol, IToken, LexicalState, SemanticStackType } from 'thaw-interpreter-types';

import { GrammarBase /*, GrammarException */ } from 'thaw-interpreter-core';

export class APLGrammar extends GrammarBase {
	// The APL grammar from Kamin (the book 'Programming Languages: An Interpreter-Based Approach')

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

		this.terminals.push(GrammarSymbol.terminalMax);
		this.terminals.push(GrammarSymbol.terminalOr);
		this.terminals.push(GrammarSymbol.terminalAnd);
		this.terminals.push(GrammarSymbol.terminalPlusSlash);
		this.terminals.push(GrammarSymbol.terminalMinusSlash);
		this.terminals.push(GrammarSymbol.terminalMultiplySlash);
		this.terminals.push(GrammarSymbol.terminalDivideSlash);
		this.terminals.push(GrammarSymbol.terminalMaxSlash);
		this.terminals.push(GrammarSymbol.terminalOrSlash);
		this.terminals.push(GrammarSymbol.terminalAndSlash);
		this.terminals.push(GrammarSymbol.terminalCompress);
		this.terminals.push(GrammarSymbol.terminalShape);
		this.terminals.push(GrammarSymbol.terminalRavel);
		this.terminals.push(GrammarSymbol.terminalRestruct);
		this.terminals.push(GrammarSymbol.terminalCat);
		this.terminals.push(GrammarSymbol.terminalIndx);
		this.terminals.push(GrammarSymbol.terminalTrans);
		this.terminals.push(GrammarSymbol.terminalSquareBrackets);
		this.terminals.push(GrammarSymbol.terminalApostrophe);
		this.terminals.push(GrammarSymbol.terminalAssign);
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

		this.nonTerminals.push(GrammarSymbol.nonterminalVectorConst);
		this.nonTerminals.push(GrammarSymbol.nonterminalIntegerLiteralList);
		// this.nonTerminals.push(GrammarSymbol.nonterminalFloatLiteralList);

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

		// APL Productions

		this.addProduction(GrammarSymbol.nonterminalValue, [GrammarSymbol.nonterminalVectorConst]);

		this.addProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalMax]);

		this.addProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalOr]);

		this.addProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalAnd]);

		this.addProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalPlusSlash]);

		this.addProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalMinusSlash]);

		this.addProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalMultiplySlash]);

		this.addProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalDivideSlash]);

		this.addProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalMaxSlash]);

		this.addProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalOrSlash]);

		this.addProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalAndSlash]);

		this.addProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalCompress]);

		this.addProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalShape]);

		this.addProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalRavel]);

		this.addProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalRestruct]);

		this.addProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalCat]);

		this.addProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalIndx]);

		this.addProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalTrans]);

		this.addProduction(GrammarSymbol.nonterminalValueOp, [
			GrammarSymbol.terminalSquareBrackets
		]);

		// An empty vector is an int vector.

		// Productions.Add(new Production(Symbol.N_VectorConst, new List<object>() { Symbol.T_Apostrophe, Symbol.T_LeftBracket, Symbol.N_IntegerLiteralList, Symbol.T_RightBracket, "#makeIntVector" }, 58));
		this.addProduction(GrammarSymbol.nonterminalVectorConst, [
			GrammarSymbol.terminalApostrophe,
			GrammarSymbol.terminalLeftBracket,
			GrammarSymbol.nonterminalIntegerLiteralList,
			GrammarSymbol.terminalRightBracket,
			'#makeIntVector'
		]);

		// Productions.Add(new Production(Symbol.N_VectorConst, new List<object>() { Symbol.T_Apostrophe, Symbol.T_LeftBracket, Symbol.T_FloatLiteral, Symbol.N_FloatLiteralList, Symbol.T_RightBracket, "#makeFloatVector" }, 59));

		// Productions.Add(new Production(Symbol.N_IntegerLiteralList, new List<object>() { Symbol.T_IntegerLiteral, Symbol.N_IntegerLiteralList, "#intList" }, 60));
		this.addProduction(GrammarSymbol.nonterminalIntegerLiteralList, [
			GrammarSymbol.terminalIntegerLiteral,
			GrammarSymbol.nonterminalIntegerLiteralList,
			'#intList'
		]);

		// Productions.Add(new Production(Symbol.N_IntegerLiteralList, new List<object>() { Symbol.Lambda, "#emptyIntList" }, 61));
		this.addProduction(GrammarSymbol.nonterminalIntegerLiteralList, [
			GrammarSymbol.Lambda,
			'#emptyIntList'
		]);

		// Productions.Add(new Production(Symbol.N_FloatLiteralList, new List<object>() { Symbol.T_FloatLiteral, Symbol.N_FloatLiteralList, "#floatList" }, 62));
		// Productions.Add(new Production(Symbol.N_FloatLiteralList, new List<object>() { Symbol.Lambda, "#emptyFloatList" }, 63));
		// Productions.Add(new Production(Symbol.N_BracketedExpression, new List<object>() { Symbol.T_Assign, Symbol.N_Variable, Symbol.N_Expression, Symbol.N_Expression, "#vecassign" }, 64));
		// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_DoubleSubscripting }, 65));
		// Productions.Add(new Production(Symbol.N_Value, new List<object>() { Symbol.T_FloatLiteral }, 66));
		// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Random }, 67));
		// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Pow }, 68));
		// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Exp }, 69));
		// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Ln }, 70));
		// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Sin }, 71));
		// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Cos }, 72));
		// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Tan }, 73));
	}

	public get languageName(): string {
		return 'APL';
	}

	// eslint-disable-next-line @typescript-eslint/no-explicit-any, @typescript-eslint/no-unused-vars
	public executeSemanticAction(semanticStack: SemanticStackType, action: string): void {
		throw new Error('APLGrammar.executeSemanticAction() : Not yet implemented.');
	}

	public override tokenToSymbol(token: IToken): GrammarSymbol {
		const tokenValueAsString: string = token.tokenValue as string;

		switch (token.tokenType) {
			// case LexicalState.tokenEOF:
			// 	return GrammarSymbol.terminalEOF;
			// case LexicalState.tokenLeftBracket:
			// 	return GrammarSymbol.terminalLeftBracket;
			// case LexicalState.tokenRightBracket:
			// 	return GrammarSymbol.terminalRightBracket;
			// case LexicalState.tokenIntLit:
			// 	return GrammarSymbol.terminalIntegerLiteral;
			// case LexicalState.tokenPlus:
			// 	return GrammarSymbol.terminalPlus;
			// case LexicalState.tokenMinus:
			// 	return GrammarSymbol.terminalMinus;
			// case LexicalState.tokenMult:
			// 	return GrammarSymbol.terminalMultiply;
			// case LexicalState.tokenDiv:
			// 	return GrammarSymbol.terminalDivide;
			// case LexicalState.tokenEqual:
			// 	return GrammarSymbol.terminalEquals;
			// case LexicalState.tokenLess:
			// 	return GrammarSymbol.terminalLessThan;
			// case LexicalState.tokenGreater:
			// 	return GrammarSymbol.terminalGreaterThan;
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
					// case '+':
					// 	return GrammarSymbol.terminalPlus;
					// case '-':
					// 	return GrammarSymbol.terminalMinus;
					// case '*':
					// 	return GrammarSymbol.terminalMultiply;
					// case '/':
					// 	return GrammarSymbol.terminalDivide;
					// case '=':
					// 	return GrammarSymbol.terminalEquals;
					// case '<':
					// 	return GrammarSymbol.terminalLessThan;

					// case 'cond':
					// 	return GrammarSymbol.terminalCond;
					// case 'let':
					// 	return GrammarSymbol.terminalLet;
					// case 'let*':
					// 	return GrammarSymbol.terminalLetStar;

					case 'max':
						return GrammarSymbol.terminalMax;
					case 'or':
						return GrammarSymbol.terminalOr;
					case 'and':
						return GrammarSymbol.terminalAnd;
					case '+/':
						return GrammarSymbol.terminalPlusSlash;
					case '-/':
						return GrammarSymbol.terminalMinusSlash;
					case '*/':
						return GrammarSymbol.terminalMultiplySlash;
					case '//':
						return GrammarSymbol.terminalDivideSlash;
					case 'max/':
						return GrammarSymbol.terminalMaxSlash;
					case 'or/':
						return GrammarSymbol.terminalOrSlash;
					case 'and/':
						return GrammarSymbol.terminalAndSlash;
					case 'compress':
						return GrammarSymbol.terminalCompress;
					case 'shape':
						return GrammarSymbol.terminalShape;
					case 'ravel':
						return GrammarSymbol.terminalRavel;
					case 'restruct':
						return GrammarSymbol.terminalRestruct;
					case 'cat':
						return GrammarSymbol.terminalCat;
					case 'indx':
						return GrammarSymbol.terminalIndx;
					case 'trans':
						return GrammarSymbol.terminalTrans;
					case '[]':
						return GrammarSymbol.terminalSquareBrackets;
					case "'":
						return GrammarSymbol.terminalApostrophe;
					case ':=':
						return GrammarSymbol.terminalAssign;
					case '[;]':
						return GrammarSymbol.terminalDoubleSubscripting;

					default:
						break;
					// 	return GrammarSymbol.terminalID;
				}

				break;

			default:
				break;
			// throw new GrammarException(
			// 	`APLGrammar.tokenToSymbol() : No grammar symbol matches token ${
			// 		token.tokenType
			// 	} ${LexicalState[token.tokenType]} (value '${token.tokenValue}')`,
			// 	token.line,
			// 	token.column
			// );
		}

		return super.tokenToSymbol(token);
	}

	/* eslint-disable @typescript-eslint/no-unused-vars */
	public pushTokenOntoSemanticStack(
		// eslint-disable-next-line @typescript-eslint/no-explicit-any
		semanticStack: SemanticStackType,
		tokenAsSymbol: number,
		token: IToken
	): void {
		throw new Error('APLGrammar.pushTokenOntoSemanticStack() : Not yet implemented.');
	}
	/* eslint-enable @typescript-eslint/no-unused-vars */
}
