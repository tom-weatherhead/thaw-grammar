// tom-weatherhead/thaw-grammar/src/languages/apl/apl-grammar.ts

import { GrammarSymbol, IToken, LexicalState, SemanticStackType } from 'thaw-interpreter-types';

import { GrammarBase, GrammarException } from 'thaw-interpreter-core';

import { Name } from 'thaw-interpreter-core';

// import { EnvironmentFrame } from '../../common/domain-object-model/environment-frame';

import { ExpressionList } from '../../common/domain-object-model/expression-list';

import { IExpression } from '../../common/domain-object-model/iexpression';

// import { Variable } from '../../common/domain-object-model/variable';

import { IAPLValue } from './domain-object-model/interfaces/ivalue';

import { APLValue } from './domain-object-model/data-types/value';

import { APLOperatorUsage } from './domain-object-model/operator-usage';

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

		this.addProduction(GrammarSymbol.nonterminalVectorConst, [
			GrammarSymbol.terminalApostrophe,
			GrammarSymbol.terminalLeftBracket,
			GrammarSymbol.nonterminalIntegerLiteralList,
			GrammarSymbol.terminalRightBracket,
			'#makeIntVector'
		]);

		// Productions.Add(new Production(Symbol.N_VectorConst, new List<object>() { Symbol.T_Apostrophe, Symbol.T_LeftBracket, Symbol.T_FloatLiteral, Symbol.N_FloatLiteralList, Symbol.T_RightBracket, "#makeFloatVector" }, 59));

		this.addProduction(GrammarSymbol.nonterminalIntegerLiteralList, [
			GrammarSymbol.terminalIntegerLiteral,
			GrammarSymbol.nonterminalIntegerLiteralList,
			'#intList'
		]);

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

	public executeSemanticAction(semanticStack: SemanticStackType, action: string): void {
		let name: Name;
		let expression: IExpression<IAPLValue>;
		let expressionList: ExpressionList<IAPLValue>;

		switch (action) {
			case '#operatorUsage':
				expressionList = semanticStack.pop() as ExpressionList<IAPLValue>;
				name = semanticStack.pop() as Name;
				semanticStack.push(new APLOperatorUsage(name, expressionList));
				break;

			case '#expressionList':
				expressionList = semanticStack.pop() as ExpressionList<IAPLValue>;
				expression = semanticStack.pop() as IExpression<IAPLValue>;
				expressionList.value.unshift(expression);
				semanticStack.push(expressionList);
				break;

			case '#emptyExpressionList':
				// semanticStack.push([] as IExpression<IAPLValue>[]);
				semanticStack.push(new ExpressionList<IAPLValue>());
				break;

			// case '#':
			// 	break;

			// case '#functionDefinition':
			// 	body = semanticStack.pop() as ISmalltalkExpression;
			// 	argumentList = semanticStack.pop() as ISmalltalkVariable[];
			//
			// 	functionName = semanticStack.pop() as Name;
			// 	semanticStack.push(
			// 		new SmalltalkFunctionDefinition(functionName.value, argumentList, body)
			// 	);
			// 	break;

			default:
				// base.ExecuteSemanticAction(semanticStack, action);
				// break;
				throw new GrammarException(`APL: Unrecognized semantic action: ${action}`);
		}
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
					// case 'define':
					// 	return GrammarSymbol.terminalDefine;
					// case 'if':
					// 	return GrammarSymbol.terminalIf;
					// case 'while':
					// 	return GrammarSymbol.terminalWhile;
					// case 'set':
					// 	return GrammarSymbol.terminalSet;
					// case 'begin':
					// 	return GrammarSymbol.terminalBegin;
					// case 'print':
					// 	return GrammarSymbol.terminalPrint;
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
			// case GrammarSymbol.terminalID:
			// case GrammarSymbol.terminalPrint:
			// case GrammarSymbol.terminalPlus:
			// case GrammarSymbol.terminalMinus:
			// case GrammarSymbol.terminalMultiply:
			// case GrammarSymbol.terminalDivide:
			// case GrammarSymbol.terminalEquals:
			// case GrammarSymbol.terminalLessThan:
			//case GrammarSymbol.terminalGreaterThan:
			// case GrammarSymbol.terminalLet:
			// case GrammarSymbol.terminalLetStar:
			// case GrammarSymbol.terminalRandom:
			// case GrammarSymbol.terminalPow:
			// case GrammarSymbol.terminalExp:
			// case GrammarSymbol.terminalLn:
			// case GrammarSymbol.terminalSin:
			// case GrammarSymbol.terminalCos:
			// case GrammarSymbol.terminalTan:

			case GrammarSymbol.terminalNumberPred:
			case GrammarSymbol.terminalSymbolPred:
			case GrammarSymbol.terminalStringPred:
			case GrammarSymbol.terminalObjectPred:
			case GrammarSymbol.terminalToString:
			case GrammarSymbol.terminalStringToSymbol:
			case GrammarSymbol.terminalAtan2:
			case GrammarSymbol.terminalFloor:
			case GrammarSymbol.terminalThrow:
			case GrammarSymbol.terminalStringLessThan:
			case GrammarSymbol.terminalStrlen:
			case GrammarSymbol.terminalSubstr:
			case GrammarSymbol.terminalTypename:
			// case GrammarSymbol.terminalHash:
			// case GrammarSymbol.terminalReferenceEquals:
			case GrammarSymbol.terminalStrcat:
			case GrammarSymbol.terminalNewArray:
			case GrammarSymbol.terminalArrayLength:
			case GrammarSymbol.terminalArrayGet:
			case GrammarSymbol.terminalArraySet:
			case GrammarSymbol.terminalArrayPred:
			case GrammarSymbol.terminalCharPred:
			case GrammarSymbol.terminalStringIndex:
				semanticStack.push(new Name(value as string, token.line, token.column));
				// Or: semanticStack.push(new Name(value.toString(), token.line, token.column));
				// Or: semanticStack.push(new Name(`${value}`, token.line, token.column));
				break;

			case GrammarSymbol.terminalIntegerLiteral:
				semanticStack.push(
					APLValue.createScalar(value as number /*, token.line, token.column */)
				);
				break;

			// case GrammarSymbol.terminalFloatLiteral:
			// 	semanticStack.push(new SmalltalkFloat(value, token.line, token.column));
			// 	break;
			//
			// case GrammarSymbol.terminalStringLiteral:
			// 	semanticStack.push(new SmalltalkString(value, token.line, token.column));
			// 	break;

			case GrammarSymbol.terminalLeftBracket:
			case GrammarSymbol.terminalRightBracket:
			case GrammarSymbol.terminalBegin:
			case GrammarSymbol.terminalCond:
			case GrammarSymbol.terminalDefine:
			case GrammarSymbol.terminalIf:
			case GrammarSymbol.terminalSet:
			case GrammarSymbol.terminalWhile:
			case GrammarSymbol.terminalEOF:
				// For these terminals, push nothing onto the semantic stack.
				break;

			default:
				super.pushTokenOntoSemanticStack(semanticStack, tokenAsSymbol, token);
		}
	}
}
