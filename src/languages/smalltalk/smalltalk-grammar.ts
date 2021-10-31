// tom-weatherhead/thaw-grammar/src/languages/smalltalk/smalltalk-grammar.ts

import { GrammarSymbol, IToken, LexicalState, SemanticStackType } from 'thaw-interpreter-types';

// import { ExpressionList }  from '../../common/domain-object-model/expression-list';
// import { IExpression }  from '../../common/domain-object-model/iexpression';
// import { Name }  from '../../common/domain-object-model/name';
// import { Variable }  from '../../common/domain-object-model/variable';
// import { VariableList }  from '../../common/domain-object-model/variable-list';

// import { BeginUsage }  from '../../common/domain-object-model/begin-usage';
// import { CondUsage }  from '../../common/domain-object-model/cond-usage';
// import { FunctionDefinition }  from '../../common/domain-object-model/function-definition';
// import { IfUsage }  from '../../common/domain-object-model/if-usage';
// import { LetStarUsage }  from '../../common/domain-object-model/let-star-usage';
// import { LetUsage }  from '../../common/domain-object-model/let-usage';
// import { OperatorUsage }  from '../../common/domain-object-model/operator-usage';
// import { SetUsage }  from '../../common/domain-object-model/set-usage';
// import { WhileUsage }  from '../../common/domain-object-model/while-usage';

// import { ArgumentException } from '../../common/exceptions/argument-exception';

import { GrammarBase, GrammarException } from 'thaw-interpreter-core';

export class SmalltalkGrammar extends GrammarBase {
	// The Smalltalk grammar from Kamin (the book 'Programming Languages: An Interpreter-Based Approach')

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
		this.terminals.push(GrammarSymbol.terminalPrint);
		this.terminals.push(GrammarSymbol.terminalID);
		this.terminals.push(GrammarSymbol.terminalIntegerLiteral);
		// this.terminals.push(GrammarSymbol.terminalCond);
		// this.terminals.push(GrammarSymbol.terminalLet);
		// this.terminals.push(GrammarSymbol.terminalLetStar);

		this.terminals.push(GrammarSymbol.terminalRandom);
		this.terminals.push(GrammarSymbol.terminalToString);
		this.terminals.push(GrammarSymbol.terminalStringToSymbol);
		this.terminals.push(GrammarSymbol.terminalPow);
		this.terminals.push(GrammarSymbol.terminalExp);
		this.terminals.push(GrammarSymbol.terminalLn);
		this.terminals.push(GrammarSymbol.terminalSin);
		this.terminals.push(GrammarSymbol.terminalCos);
		this.terminals.push(GrammarSymbol.terminalTan);
		this.terminals.push(GrammarSymbol.terminalFloor);
		this.terminals.push(GrammarSymbol.terminalAtan2);
		this.terminals.push(GrammarSymbol.terminalThrow);
		this.terminals.push(GrammarSymbol.terminalStringLessThan);
		this.terminals.push(GrammarSymbol.terminalStrlen);
		this.terminals.push(GrammarSymbol.terminalSubstr);
		this.terminals.push(GrammarSymbol.terminalTypename);
		// this.terminals.push(GrammarSymbol.terminalHash);
		// this.terminals.push(GrammarSymbol.terminalReferenceEquals);
		this.terminals.push(GrammarSymbol.terminalStrcat);
		this.terminals.push(GrammarSymbol.terminalNewArray);
		this.terminals.push(GrammarSymbol.terminalArrayLength);
		this.terminals.push(GrammarSymbol.terminalArrayGet);
		this.terminals.push(GrammarSymbol.terminalArraySet);
		this.terminals.push(GrammarSymbol.terminalDollar);
		this.terminals.push(GrammarSymbol.terminalCharPred);
		this.terminals.push(GrammarSymbol.terminalStringIndex);
		this.terminals.push(GrammarSymbol.terminalArrayPred);
		this.terminals.push(GrammarSymbol.terminalClass);
		this.terminals.push(GrammarSymbol.terminalOctothorpe);
		this.terminals.push(GrammarSymbol.terminalFloatLiteral);
		this.terminals.push(GrammarSymbol.terminalStringLiteral);
		this.terminals.push(GrammarSymbol.terminalNumberPred);
		this.terminals.push(GrammarSymbol.terminalSymbolPred);
		this.terminals.push(GrammarSymbol.terminalStringPred);
		this.terminals.push(GrammarSymbol.terminalObjectPred);

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

		this.nonTerminals.push(GrammarSymbol.nonterminalClassDef);
		this.nonTerminals.push(GrammarSymbol.nonterminalClass);
		this.nonTerminals.push(GrammarSymbol.nonterminalInstVars);
		this.nonTerminals.push(GrammarSymbol.nonterminalMethodDef);
		this.nonTerminals.push(GrammarSymbol.nonterminalMethodDefList);
		this.nonTerminals.push(GrammarSymbol.nonterminalSymbol);
		this.nonTerminals.push(GrammarSymbol.nonterminalLiteralList);

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

		// //Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_GreaterThan }, 27));
		// this.addProduction(GrammarSymbol.nonterminalValueOp, [
		// 	GrammarSymbol.terminalGreaterThan
		// ]);

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

		// Smalltalk Productions

		// Productions.Add(new Production(Symbol.N_Input, new List<object>() { Symbol.N_ClassDef }, 39));
		// Productions.Add(new Production(Symbol.N_ClassDef, new List<object>() {
		//     GrammarSymbol.terminalLeftBracket,
		//     GrammarSymbol.terminalClass,
		//     Symbol.N_Class,
		//     Symbol.N_Class,
		//     Symbol.N_InstVars,  // Actually the class variables; see Exercise 10 on page 347.
		//     Symbol.N_InstVars,
		//     Symbol.N_MethodDef,
		//     Symbol.N_MethodDefList,
		//     GrammarSymbol.terminalRightBracket, "#classDefinition" }, 40));
		this.addProduction(GrammarSymbol.nonterminalBracketedInput, [
			GrammarSymbol.terminalClass,
			GrammarSymbol.nonterminalClass,
			GrammarSymbol.nonterminalClass,
			GrammarSymbol.nonterminalInstVars,
			GrammarSymbol.nonterminalInstVars,
			GrammarSymbol.nonterminalMethodDef,
			GrammarSymbol.nonterminalMethodDefList,
			'#classDefinition'
		]);

		this.addProduction(GrammarSymbol.nonterminalClass, [GrammarSymbol.terminalID]);

		this.addProduction(GrammarSymbol.nonterminalInstVars, [
			GrammarSymbol.terminalLeftBracket,
			GrammarSymbol.nonterminalVariableList,
			GrammarSymbol.terminalRightBracket
		]);

		this.addProduction(GrammarSymbol.nonterminalMethodDefList, [
			GrammarSymbol.nonterminalMethodDef,
			GrammarSymbol.nonterminalMethodDefList,
			'#methodDefList'
		]);

		this.addProduction(GrammarSymbol.nonterminalMethodDefList, [
			GrammarSymbol.Lambda,
			'#emptyMethodDefList'
		]);

		this.addProduction(GrammarSymbol.nonterminalMethodDef, [
			GrammarSymbol.terminalLeftBracket,
			GrammarSymbol.nonterminalFunDef,
			GrammarSymbol.terminalRightBracket
		]);

		// Productions.Add(new Production(Symbol.N_Value, new List<object>() { Symbol.N_Symbol }, 45));

		// Productions.Add(new Production(Symbol.N_Symbol, new List<object>() { GrammarSymbol.terminalOctothorpe, GrammarSymbol.terminalID, "#symbol" }, 46));

		// // This next production allows us to redefine built-in value ops (such as +) in classes.
		// Productions.Add(new Production(Symbol.N_FunDef, new List<object>() {
		//     GrammarSymbol.terminalLeftBracket,
		//     GrammarSymbol.terminalDefine,
		//     Symbol.N_ValueOp,
		//     Symbol.N_ArgList,
		//     Symbol.N_Expression,
		//     GrammarSymbol.terminalRightBracket, "#functionDefinition" }, 48));
		//
		// Productions.Add(new Production(Symbol.N_Value, new List<object>() { GrammarSymbol.terminalFloatLiteral }, 49));
		// Productions.Add(new Production(Symbol.N_Value, new List<object>() { GrammarSymbol.terminalStringLiteral }, 50));
		// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { GrammarSymbol.terminalRandom }, 51));
		// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { GrammarSymbol.terminalToString }, 52));
		// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { GrammarSymbol.terminalStringToSymbol }, 53));
		// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { GrammarSymbol.terminalPow }, 54));
		// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { GrammarSymbol.terminalExp }, 55));
		// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { GrammarSymbol.terminalLn }, 56));
		// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { GrammarSymbol.terminalSin }, 57));
		// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { GrammarSymbol.terminalCos }, 58));
		// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { GrammarSymbol.terminalTan }, 59));
		// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { GrammarSymbol.terminalFloor }, 60));
		// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { GrammarSymbol.terminalAtan2 }, 61));
		// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { GrammarSymbol.terminalThrow }, 62));
		// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { GrammarSymbol.terminalStringLessThan }, 63));
		// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { GrammarSymbol.terminalNumberPred }, 64));
		// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { GrammarSymbol.terminalSymbolPred }, 65));
		// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { GrammarSymbol.terminalStringPred }, 66));
		// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { GrammarSymbol.terminalObjectPred }, 67));
		// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { GrammarSymbol.terminalStrlen }, 68));
		// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { GrammarSymbol.terminalSubstr }, 69));
		// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { GrammarSymbol.terminalTypename }, 70));
		// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { GrammarSymbol.terminalHash }, 71));
		// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { GrammarSymbol.terminalReferenceEquals }, 72));
		// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { GrammarSymbol.terminalStrcat }, 73));
		// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { GrammarSymbol.terminalNewArray }, 74));
		// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { GrammarSymbol.terminalArrayLength }, 75));
		// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { GrammarSymbol.terminalArrayGet }, 76));
		// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { GrammarSymbol.terminalArraySet }, 77));
		// Productions.Add(new Production(Symbol.N_Value, new List<object>() {
		//     GrammarSymbol.terminalOctothorpe, GrammarSymbol.terminalLeftBracket, Symbol.N_LiteralList, GrammarSymbol.terminalRightBracket, "#arrayLiteral" }, 78));
		// Productions.Add(new Production(Symbol.N_LiteralList, new List<object>() { Symbol.Lambda, "#emptyLiteralList" }, 79));
		// Productions.Add(new Production(Symbol.N_LiteralList, new List<object>() { Symbol.N_Value, Symbol.N_LiteralList, "#literalList" }, 80));
		// Productions.Add(new Production(Symbol.N_Value, new List<object>() { GrammarSymbol.terminalDollar, GrammarSymbol.terminalID, "#characterLiteral" }, 81));
		// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { GrammarSymbol.terminalCharPred }, 82));
		// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { GrammarSymbol.terminalStringIndex }, 83));
		// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { GrammarSymbol.terminalArrayPred }, 84));
	}

	public get languageName(): string {
		return 'Smalltalk';
	}

	// private ISmalltalkExpression CreateLetUsage(string letKeyword,
	//     List<KeyValuePair<SmalltalkVariable, ISmalltalkExpression>> varExprList, ISmalltalkExpression expression)
	// {
	//
	//     switch (letKeyword)
	//     {
	//         case "let":
	//             return new SmalltalkLetUsage(varExprList, expression);
	//
	//         case "let*":
	//             return new SmalltalkLetStarUsage(varExprList, expression);
	//
	//         default:
	//             throw new ArgumentException(string.Format("SmalltalkGrammar.CreateLetUsage() : Unknown 'let' keyword '{0}'.", letKeyword));
	//     }
	// }

	/* eslint-disable @typescript-eslint/no-unused-vars */
	public executeSemanticAction(semanticStack: SemanticStackType, action: string): void {
		throw new Error('SmalltalkGrammar.executeSemanticAction() : Not yet implemented.');

		//         Name name;
		//         Name functionName;
		//         Name superClassName;
		//         SmalltalkVariable variable;
		//         List<SmalltalkVariable> variableList;
		//         List<SmalltalkVariable> classVariableList;
		//         ISmalltalkExpression expression;
		//         ISmalltalkExpression expression2;
		//         List<ISmalltalkExpression> expressionList;
		//         SmalltalkFunctionDefinition funDef;
		//         List<SmalltalkFunctionDefinition> funDefList;
		//         List<KeyValuePair<ISmalltalkExpression, ISmalltalkExpression>> exprPairList;
		//         List<KeyValuePair<SmalltalkVariable, ISmalltalkExpression>> varExprList;
		//         List<ISmalltalkValue> literalList;
		//
		//         switch (action)
		//         {
		//             case '#functionDefinition':
		//                 var body = (ISmalltalkExpression)semanticStack.Pop();
		//                 var argList = (List<SmalltalkVariable>)semanticStack.Pop();
		//
		//                 functionName = (Name)semanticStack.Pop();
		//                 semanticStack.Push(new SmalltalkFunctionDefinition(functionName.Value, argList, body));
		//                 break;
		//
		//             case '#variableList':
		//                 variableList = (List<SmalltalkVariable>)semanticStack.Pop();
		//                 variable = (SmalltalkVariable)semanticStack.Pop();
		//                 variableList.Insert(0, variable);
		//                 semanticStack.Push(variableList);
		//                 break;
		//
		//             case '#emptyVariableList':
		//                 semanticStack.Push(new List<SmalltalkVariable>());
		//                 break;
		//
		// #if DEAD_CODE
		//             case '#if':
		//                 var expression3 = (ISmalltalkExpression)semanticStack.Pop();
		//
		//                 expression2 = (ISmalltalkExpression)semanticStack.Pop();
		//                 expression = (ISmalltalkExpression)semanticStack.Pop();
		//                 semanticStack.Push(new SmalltalkIfUsage(expression, expression2, expression3));
		//                 break;
		// #endif
		//
		//             case '#while':
		//                 expression2 = (ISmalltalkExpression)semanticStack.Pop();
		//                 expression = (ISmalltalkExpression)semanticStack.Pop();
		//                 semanticStack.Push(new SmalltalkWhileUsage(expression, expression2));
		//                 break;
		//
		//             case '#set':
		//                 expression = (ISmalltalkExpression)semanticStack.Pop();
		//                 variable = (SmalltalkVariable)semanticStack.Pop();
		//                 semanticStack.Push(new SmalltalkSetUsage(variable, expression));
		//                 break;
		//
		//             case '#begin':
		//                 expressionList = (List<ISmalltalkExpression>)semanticStack.Pop();
		//                 expression = (ISmalltalkExpression)semanticStack.Pop();
		//                 semanticStack.Push(new SmalltalkBeginUsage(expression, expressionList));
		//                 break;
		//
		//             case '#operatorUsage':
		//                 expressionList = (List<ISmalltalkExpression>)semanticStack.Pop();
		//                 name = (Name)semanticStack.Pop();
		//                 semanticStack.Push(new SmalltalkOperatorUsage(name /*.Value */, expressionList));
		//                 break;
		//
		//             case '#expressionList':
		//                 expressionList = (List<ISmalltalkExpression>)semanticStack.Pop();
		//                 expression = (ISmalltalkExpression)semanticStack.Pop();
		//                 expressionList.Insert(0, expression);
		//                 semanticStack.Push(expressionList);
		//                 break;
		//
		//             case '#emptyExpressionList':
		//                 semanticStack.Push(new List<ISmalltalkExpression>());
		//                 break;
		//
		//             case '#variable':
		//                 name = (Name)semanticStack.Pop();
		//                 semanticStack.Push(new SmalltalkVariable(name.Value));
		//                 break;
		//
		//             case '#methodDefList':
		//                 funDefList = (List<SmalltalkFunctionDefinition>)semanticStack.Pop();
		//                 funDef = (SmalltalkFunctionDefinition)semanticStack.Pop();
		//                 funDefList.Insert(0, funDef);
		//                 semanticStack.Push(funDefList);
		//                 break;
		//
		//             case '#emptyMethodDefList':
		//                 semanticStack.Push(new List<SmalltalkFunctionDefinition>());
		//                 break;
		//
		//             case '#classDefinition':
		//                 funDefList = (List<SmalltalkFunctionDefinition>)semanticStack.Pop();
		//                 funDef = (SmalltalkFunctionDefinition)semanticStack.Pop();
		//                 funDefList.Insert(0, funDef);
		//                 variableList = (List<SmalltalkVariable>)semanticStack.Pop();
		//                 classVariableList = (List<SmalltalkVariable>)semanticStack.Pop();
		//                 superClassName = (Name)semanticStack.Pop();
		//                 name = (Name)semanticStack.Pop();
		//                 semanticStack.Push(new SmalltalkClass(name, superClassName.Value, classVariableList, variableList, funDefList));
		//                 break;
		//
		//             case '#symbol':
		//                 name = (Name)semanticStack.Pop();
		//                 semanticStack.Push(new SmalltalkSymbolValue(name.Value));
		//                 break;
		//
		//             case '#condUsage':
		//                 exprPairList = (List<KeyValuePair<ISmalltalkExpression, ISmalltalkExpression>>)semanticStack.Pop();
		//                 expression2 = (ISmalltalkExpression)semanticStack.Pop();
		//                 expression = (ISmalltalkExpression)semanticStack.Pop();
		//                 exprPairList.Insert(0, new KeyValuePair<ISmalltalkExpression, ISmalltalkExpression>(expression, expression2));
		//                 semanticStack.Push(new SmalltalkCondUsage(exprPairList));
		//                 break;
		//
		//             case '#exprPairList':
		//                 exprPairList = (List<KeyValuePair<ISmalltalkExpression, ISmalltalkExpression>>)semanticStack.Pop();
		//                 expression2 = (ISmalltalkExpression)semanticStack.Pop();
		//                 expression = (ISmalltalkExpression)semanticStack.Pop();
		//                 exprPairList.Insert(0, new KeyValuePair<ISmalltalkExpression, ISmalltalkExpression>(expression, expression2));
		//                 semanticStack.Push(exprPairList);
		//                 break;
		//
		//             case '#emptyExprPairList':
		//                 semanticStack.Push(new List<KeyValuePair<ISmalltalkExpression, ISmalltalkExpression>>());
		//                 break;
		//
		//             case '#letUsage':
		//                 expression = (ISmalltalkExpression)semanticStack.Pop();
		//                 varExprList = (List<KeyValuePair<SmalltalkVariable, ISmalltalkExpression>>)semanticStack.Pop();
		//
		//                 var letName = (Name)semanticStack.Pop();
		//
		//                 semanticStack.Push(CreateLetUsage(letName.Value, varExprList, expression));
		//                 break;
		//
		//             case '#varExprList':
		//                 varExprList = (List<KeyValuePair<SmalltalkVariable, ISmalltalkExpression>>)semanticStack.Pop();
		//                 expression = (ISmalltalkExpression)semanticStack.Pop();
		//                 variable = (SmalltalkVariable)semanticStack.Pop();
		//                 varExprList.Insert(0, new KeyValuePair<SmalltalkVariable, ISmalltalkExpression>(variable, expression));
		//                 semanticStack.Push(varExprList);
		//                 break;
		//
		//             case '#emptyVarExprList':
		//                 semanticStack.Push(new List<KeyValuePair<SmalltalkVariable, ISmalltalkExpression>>());
		//                 break;
		//
		//             case '#emptyLiteralList':
		//                 semanticStack.Push(new List<ISmalltalkValue>());
		//                 break;
		//
		//             case '#literalList':
		//                 literalList = (List<ISmalltalkValue>)semanticStack.Pop();
		//
		//                 var literal = (ISmalltalkValue)semanticStack.Pop();
		//
		//                 literalList.Insert(0, literal);
		//                 semanticStack.Push(literalList);
		//                 break;
		//
		//             case '#arrayLiteral':
		//                 literalList = (List<ISmalltalkValue>)semanticStack.Pop();
		//
		//                 var array = new SmalltalkArrayValue(literalList.Count);
		//
		//                 for (var i = 0; i < literalList.Count; ++i)
		//                 {
		//                     array.Value[i] = literalList[i];
		//                 }
		//
		//                 semanticStack.Push(array);
		//                 break;
		//
		//             case '#characterLiteral':
		//                 name = (Name)semanticStack.Pop();
		//                 semanticStack.Push(new SmalltalkCharacterValue(name.Value[0]));
		//                 break;
		//
		//             default:
		//                 base.ExecuteSemanticAction(semanticStack, action);
		//                 break;
		//         }
	}
	/* eslint-enable @typescript-eslint/no-unused-vars */

	public tokenToSymbol(token: IToken): GrammarSymbol {
		const tokenValueAsString: string = token.tokenValue as string;

		switch (token.tokenType) {
			case LexicalState.tokenEOF:
				return GrammarSymbol.terminalEOF;
			case LexicalState.tokenLeftBracket:
				return GrammarSymbol.terminalLeftBracket;
			case LexicalState.tokenRightBracket:
				return GrammarSymbol.terminalRightBracket;
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
			case LexicalState.tokenEqual:
				return GrammarSymbol.terminalEquals;
			case LexicalState.tokenLess:
				return GrammarSymbol.terminalLessThan;
			// case LexicalState.tokenGreater:
			// 	return GrammarSymbol.terminalGreaterThan; ? or ID?
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
					// TODO (when we replace the keyword 'if' with a method) :
					// (if x y z) -> (x y z)
					// (true x y) -> x
					// (false x y) -> y
					// case 'if': return GrammarSymbol.terminalID;
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
					case 'cond':
						return GrammarSymbol.terminalCond;
					case 'let':
						return GrammarSymbol.terminalLet;
					case 'let*':
						return GrammarSymbol.terminalLetStar;
					// case 'list?':
					// 	return GrammarSymbol.terminalListPred;
					// case '=>':
					// 	return GrammarSymbol.terminalThickArrow;
					//case '>': return GrammarSymbol.terminalGreaterThan;
					case 'class':
						return GrammarSymbol.terminalClass;
					case 'number?':
						return GrammarSymbol.terminalNumberPred;
					case 'symbol?':
						return GrammarSymbol.terminalSymbolPred;
					case 'string?':
						return GrammarSymbol.terminalStringPred;
					case 'object?':
						return GrammarSymbol.terminalObjectPred;
					case 'random':
						return GrammarSymbol.terminalRandom;
					case 'tostring':
						return GrammarSymbol.terminalToString;
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
					case 'strlen':
						return GrammarSymbol.terminalStrlen;
					case 'substr':
						return GrammarSymbol.terminalSubstr;
					case 'typename':
						return GrammarSymbol.terminalTypename;
					// case 'hash':
					// 	return GrammarSymbol.terminalHash;
					// case 'ref=':
					// 	return GrammarSymbol.terminalReferenceEquals;
					case 'strcat':
						return GrammarSymbol.terminalStrcat;
					case 'newarray':
						return GrammarSymbol.terminalNewArray;
					case 'arraylength':
						return GrammarSymbol.terminalArrayLength;
					case 'arrayget':
						return GrammarSymbol.terminalArrayGet;
					case 'arrayset':
						return GrammarSymbol.terminalArraySet;
					case 'array?':
						return GrammarSymbol.terminalArrayPred;
					case 'char?':
						return GrammarSymbol.terminalCharPred;
					case 'stridx':
						return GrammarSymbol.terminalStringIndex;
					default:
						return GrammarSymbol.terminalID;
				}

			default:
				throw new GrammarException(
					`SmalltalkGrammar.tokenToSymbol() : No grammar symbol matches token ${
						token.tokenType
					} ${LexicalState[token.tokenType]} (value '${token.tokenValue}')`,
					token.line,
					token.column
				);
		}
	}

	/* eslint-disable @typescript-eslint/no-unused-vars */
	public pushTokenOntoSemanticStack(
		semanticStack: SemanticStackType,
		tokenAsSymbol: number,
		token: IToken
	): void {
		throw new Error('SmalltalkGrammar.pushTokenOntoSemanticStack() : Not yet implemented.');

		// const value = token.tokenValue;
		//
		// switch (tokenAsSymbol) {
		// case GrammarSymbol.terminalID:
		// case GrammarSymbol.terminalPrint:
		// case GrammarSymbol.terminalPlus:
		// case GrammarSymbol.terminalMinus:
		// case GrammarSymbol.terminalMultiply:
		// case GrammarSymbol.terminalDivide:
		// case GrammarSymbol.terminalEquals:
		// case GrammarSymbol.terminalLessThan:
		// //case GrammarSymbol.terminalGreaterThan:
		// case GrammarSymbol.terminalLet:
		// case GrammarSymbol.terminalLetStar:
		// case GrammarSymbol.terminalNumberPred:
		// case GrammarSymbol.terminalSymbolPred:
		// case GrammarSymbol.terminalStringPred:
		// case GrammarSymbol.terminalObjectPred:
		// case GrammarSymbol.terminalRandom:
		// case GrammarSymbol.terminalToString:
		// case GrammarSymbol.terminalStringToSymbol:
		// case GrammarSymbol.terminalPow:
		// case GrammarSymbol.terminalExp:
		// case GrammarSymbol.terminalLn:
		// case GrammarSymbol.terminalSin:
		// case GrammarSymbol.terminalCos:
		// case GrammarSymbol.terminalTan:
		// case GrammarSymbol.terminalAtan2:
		// case GrammarSymbol.terminalFloor:
		// case GrammarSymbol.terminalThrow:
		// case GrammarSymbol.terminalStringLessThan:
		// case GrammarSymbol.terminalStrlen:
		// case GrammarSymbol.terminalSubstr:
		// case GrammarSymbol.terminalTypename:
		// case GrammarSymbol.terminalHash:
		// case GrammarSymbol.terminalReferenceEquals:
		// case GrammarSymbol.terminalStrcat:
		// case GrammarSymbol.terminalNewArray:
		// case GrammarSymbol.terminalArrayLength:
		// case GrammarSymbol.terminalArrayGet:
		// case GrammarSymbol.terminalArraySet:
		// case GrammarSymbol.terminalArrayPred:
		// case GrammarSymbol.terminalCharPred:
		// case GrammarSymbol.terminalStringIndex:
		// 		semanticStack.Push(new Name(value as string, token.Line, token.Column));
		// 		break;
		//
		//     case GrammarSymbol.terminalIntegerLiteral:
		//         semanticStack.Push(new SmalltalkIntegerValue((int)value));
		//         break;
		//
		//     case GrammarSymbol.terminalFloatLiteral:
		//         semanticStack.Push(new SmalltalkFloatValue((double)value));
		//         break;
		//
		//     case GrammarSymbol.terminalStringLiteral:
		//         semanticStack.Push(new SmalltalkStringValue((string)value));
		//         break;
		//
		// 	default:
		// 		break;
		// }
	}
	/* eslint-enable @typescript-eslint/no-unused-vars */
}
