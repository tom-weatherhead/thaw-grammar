// tom-weatherhead/thaw-grammar/src/languages/smalltalk/smalltalk-grammar.ts

// 2021-10-31 : Note: For each language with value type IDOMValue, we can define this:

// interface IDOMValueConverter {
// 	toInteger(value: IDOMValue): number | undefined;
// 	toFloat(value: IDOMValue): number | undefined;
// 	toStringX(value: IDOMValue): string | undefined; // 'toStringX' so we don't conflict with 'toString'
//
// 	fromInteger(value: number): IDOMValue;
// 	fromFloat(value: number): IDOMValue;
// 	fromString(value: string): IDOMValue;
// }

// Then isInteger(value: IDOMValue) can be implemented as: typeof toInteger(value) !== 'undefined'

// ... and fromInteger(value: number) can be implemented as: new SmalltalkIntegerValue(value)

// ****

import { GrammarSymbol, IToken, LexicalState, SemanticStackType } from 'thaw-interpreter-types';

import { GrammarBase, GrammarException, Name } from 'thaw-interpreter-core';

// import { Name } from '../../common/domain-object-model/name';

import { ArgumentException } from '../../common/exceptions/argument-exception';

import {
	ISmalltalkExpression,
	ISmalltalkFunctionDefinition,
	// ISmalltalkValue,
	ISmalltalkVariable
} from './domain-object-model/interfaces/iexpression';

SmalltalkBeginUsage;
import { SmalltalkBeginUsage } from './domain-object-model/begin-usage';
import { SmalltalkClass } from './domain-object-model/class';
import { SmalltalkCondUsage } from './domain-object-model/cond-usage';
import { SmalltalkFunctionDefinition } from './domain-object-model/function-definition';
import { SmalltalkIfUsage } from './domain-object-model/if-usage';
import { SmalltalkIntegerValue } from './domain-object-model/integer';
import { SmalltalkLetStarUsage } from './domain-object-model/let-star-usage';
import { SmalltalkLetUsage } from './domain-object-model/let-usage';
import { SmalltalkOperatorUsage } from './domain-object-model/operator-usage';
import { SmalltalkSetUsage } from './domain-object-model/set-usage';
import { SmalltalkVariable } from './domain-object-model/variable';
import { SmalltalkWhileUsage } from './domain-object-model/while-usage';

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
		this.terminals.push(GrammarSymbol.terminalCond);
		this.terminals.push(GrammarSymbol.terminalLet);
		this.terminals.push(GrammarSymbol.terminalLetStar);

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
		this.nonTerminals.push(GrammarSymbol.nonterminalExprPairList);
		this.nonTerminals.push(GrammarSymbol.nonterminalLetKeyword);
		this.nonTerminals.push(GrammarSymbol.nonterminalVarExprList);

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

		this.addProduction(GrammarSymbol.nonterminalBracketedExpression, [
			GrammarSymbol.terminalCond,
			GrammarSymbol.terminalLeftBracket,
			GrammarSymbol.nonterminalExpression,
			GrammarSymbol.nonterminalExpression,
			GrammarSymbol.terminalRightBracket,
			GrammarSymbol.nonterminalExprPairList,
			'#condUsage'
		]);
		this.addProduction(GrammarSymbol.nonterminalExprPairList, [
			GrammarSymbol.terminalLeftBracket,
			GrammarSymbol.nonterminalExpression,
			GrammarSymbol.nonterminalExpression,
			GrammarSymbol.terminalRightBracket,
			GrammarSymbol.nonterminalExprPairList,
			'#exprPairList'
		]);
		this.addProduction(GrammarSymbol.nonterminalExprPairList, [
			GrammarSymbol.Lambda,
			'#emptyExprPairList'
		]);

		// let and let*

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
		//     GrammarSymbol.terminalRightBracket, '#classDefinition' }, 40));
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

	/* eslint-disable @typescript-eslint/no-unused-vars */
	private createLetUsage(
		letKeyword: string,
		varExprList: [ISmalltalkVariable, ISmalltalkExpression][],
		expression: ISmalltalkExpression
	): ISmalltalkExpression {
		// throw new Error('createLetUsage() : Not yet implemented.');

		switch (letKeyword) {
			case 'let':
				return new SmalltalkLetUsage(varExprList, expression);

			case 'let*':
				return new SmalltalkLetStarUsage(varExprList, expression);

			default:
				throw new ArgumentException(
					`SmalltalkGrammar.CreateLetUsage() : Unknown 'let' keyword '${letKeyword}'.`,
					'letKeyword'
				);
		}
	}
	/* eslint-enable @typescript-eslint/no-unused-vars */

	public executeSemanticAction(semanticStack: SemanticStackType, action: string): void {
		let name: Name;
		let functionName: Name;
		let superClassName: Name;
		let variable: ISmalltalkVariable;
		let variableList: ISmalltalkVariable[];
		let argumentList: ISmalltalkVariable[];
		let classVariableList: ISmalltalkVariable[];
		let body: ISmalltalkExpression;
		let expression: ISmalltalkExpression;
		let expression2: ISmalltalkExpression;
		let expression3: ISmalltalkExpression;
		let expressionList: ISmalltalkExpression[];
		let funDef: ISmalltalkFunctionDefinition;
		let funDefList: ISmalltalkFunctionDefinition[];
		let exprPairList: [ISmalltalkExpression, ISmalltalkExpression][];
		let varExprList: [ISmalltalkVariable, ISmalltalkExpression][];
		// let literalList: ISmalltalkValue[];

		switch (action) {
			case '#functionDefinition':
				body = semanticStack.pop() as ISmalltalkExpression;
				argumentList = semanticStack.pop() as ISmalltalkVariable[];

				functionName = semanticStack.pop() as Name;
				semanticStack.push(
					new SmalltalkFunctionDefinition(functionName.value, argumentList, body)
				);
				break;

			case '#variableList':
				variableList = semanticStack.pop() as ISmalltalkVariable[];
				variable = semanticStack.pop() as ISmalltalkVariable;
				variableList.unshift(variable);
				semanticStack.push(variableList);
				break;

			case '#emptyVariableList':
				semanticStack.push([] as ISmalltalkVariable[]);
				break;

			// #if DEAD_CODE
			case '#if':
				expression3 = semanticStack.pop() as ISmalltalkExpression;
				expression2 = semanticStack.pop() as ISmalltalkExpression;
				expression = semanticStack.pop() as ISmalltalkExpression;
				semanticStack.push(new SmalltalkIfUsage(expression, expression2, expression3));
				break;
			// #endif

			case '#while':
				expression2 = semanticStack.pop() as ISmalltalkExpression;
				expression = semanticStack.pop() as ISmalltalkExpression;
				semanticStack.push(new SmalltalkWhileUsage(expression, expression2));
				break;

			case '#set':
				expression = semanticStack.pop() as ISmalltalkExpression;
				variable = semanticStack.pop() as ISmalltalkVariable;
				semanticStack.push(new SmalltalkSetUsage(variable, expression));
				break;

			case '#begin':
				expressionList = semanticStack.pop() as ISmalltalkExpression[];
				expression = semanticStack.pop() as ISmalltalkExpression;
				semanticStack.push(new SmalltalkBeginUsage(expression, expressionList));
				break;

			case '#operatorUsage':
				expressionList = semanticStack.pop() as ISmalltalkExpression[];
				name = semanticStack.pop() as Name;
				semanticStack.push(new SmalltalkOperatorUsage(name, expressionList));
				break;

			case '#expressionList':
				expressionList = semanticStack.pop() as ISmalltalkExpression[];
				expression = semanticStack.pop() as ISmalltalkExpression;
				expressionList.unshift(expression);
				semanticStack.push(expressionList);
				break;

			case '#emptyExpressionList':
				semanticStack.push([] as ISmalltalkExpression[]);
				break;

			case '#variable':
				name = semanticStack.pop() as Name;
				semanticStack.push(new SmalltalkVariable(name.value, name.line, name.column));
				break;

			case '#methodDefList':
				funDefList = semanticStack.pop() as ISmalltalkFunctionDefinition[];
				funDef = semanticStack.pop() as ISmalltalkFunctionDefinition;
				funDefList.unshift(funDef);
				semanticStack.push(funDefList);
				break;

			case '#emptyMethodDefList':
				semanticStack.push([] as ISmalltalkFunctionDefinition[]);
				break;

			case '#classDefinition':
				funDefList = semanticStack.pop() as ISmalltalkFunctionDefinition[];
				funDef = semanticStack.pop() as ISmalltalkFunctionDefinition;
				funDefList.unshift(funDef);
				variableList = semanticStack.pop() as ISmalltalkVariable[];
				classVariableList = semanticStack.pop() as ISmalltalkVariable[];
				superClassName = semanticStack.pop() as Name;
				name = semanticStack.pop() as Name;
				semanticStack.push(
					new SmalltalkClass(
						name.value,
						superClassName.value,
						classVariableList,
						variableList,
						funDefList,
						name.line,
						name.column
					)
				);
				break;

			// case '#symbol':
			//     name = (Name)semanticStack.Pop();
			//     semanticStack.Push(new SmalltalkSymbolValue(name.Value));
			//     break;

			case '#condUsage':
				exprPairList = semanticStack.pop() as [
					ISmalltalkExpression,
					ISmalltalkExpression
				][];
				expression2 = semanticStack.pop() as ISmalltalkExpression;
				expression = semanticStack.pop() as ISmalltalkExpression;
				exprPairList.unshift([expression, expression2]);
				semanticStack.push(new SmalltalkCondUsage(exprPairList));
				break;

			case '#exprPairList':
				exprPairList = semanticStack.pop() as [
					ISmalltalkExpression,
					ISmalltalkExpression
				][];
				expression2 = semanticStack.pop() as ISmalltalkExpression;
				expression = semanticStack.pop() as ISmalltalkExpression;
				exprPairList.unshift([expression, expression2]);
				semanticStack.push(exprPairList);
				break;

			case '#emptyExprPairList':
				semanticStack.push([] as [ISmalltalkExpression, ISmalltalkExpression][]);
				break;

			case '#letUsage':
				expression = semanticStack.pop() as ISmalltalkExpression;
				varExprList = semanticStack.pop() as [ISmalltalkVariable, ISmalltalkExpression][];

				name = semanticStack.pop() as Name; // Either 'let' or 'let*'

				semanticStack.push(this.createLetUsage(name.value, varExprList, expression));
				break;

			case '#varExprList':
				varExprList = semanticStack.pop() as [ISmalltalkVariable, ISmalltalkExpression][];
				expression = semanticStack.pop() as ISmalltalkExpression;
				variable = semanticStack.pop() as ISmalltalkVariable;
				varExprList.unshift([variable, expression]);
				semanticStack.push(varExprList);
				break;

			case '#emptyVarExprList':
				semanticStack.push([] as [ISmalltalkVariable, ISmalltalkExpression][]);
				break;

			// case '#emptyLiteralList':
			//     semanticStack.Push(new List<ISmalltalkValue>());
			//     break;
			//
			// case '#literalList':
			//     literalList = (List<ISmalltalkValue>)semanticStack.Pop();
			//
			//     var literal = (ISmalltalkValue)semanticStack.Pop();
			//
			//     literalList.Insert(0, literal);
			//     semanticStack.Push(literalList);
			//     break;
			//
			// case '#arrayLiteral':
			//     literalList = (List<ISmalltalkValue>)semanticStack.Pop();
			//
			//     var array = new SmalltalkArrayValue(literalList.Count);
			//
			//     for (var i = 0; i < literalList.Count; ++i)
			//     {
			//         array.Value[i] = literalList[i];
			//     }
			//
			//     semanticStack.Push(array);
			//     break;
			//
			// case '#characterLiteral':
			//     name = (Name)semanticStack.Pop();
			//     semanticStack.Push(new SmalltalkCharacterValue(name.Value[0]));
			//     break;

			default:
				// base.ExecuteSemanticAction(semanticStack, action);
				// break;
				throw new GrammarException(`Smalltalk: Unrecognized semantic action: ${action}`);
		}
	}

	public override tokenToSymbol(token: IToken): GrammarSymbol {
		const tokenValueAsString: string = token.tokenValue as string;

		if (token.tokenType === LexicalState.tokenIdent) {
			// switch (token.tokenType) {
			// 	case LexicalState.tokenIdent:
			switch (tokenValueAsString) {
				// TODO (when we replace the keyword 'if' with a method) :
				// (if x y z) -> (x y z)
				// (true x y) -> x
				// (false x y) -> y
				// case 'if': return GrammarSymbol.terminalID;

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
					break;
			}

			// 		break;
			//
			// 	default:
			// 		break;
			// }
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
			case GrammarSymbol.terminalID:
			case GrammarSymbol.terminalPrint:
			case GrammarSymbol.terminalPlus:
			case GrammarSymbol.terminalMinus:
			case GrammarSymbol.terminalMultiply:
			case GrammarSymbol.terminalDivide:
			case GrammarSymbol.terminalEquals:
			case GrammarSymbol.terminalLessThan:
			//case GrammarSymbol.terminalGreaterThan:
			case GrammarSymbol.terminalLet:
			case GrammarSymbol.terminalLetStar:

			case GrammarSymbol.terminalNumberPred:
			case GrammarSymbol.terminalSymbolPred:
			case GrammarSymbol.terminalStringPred:
			case GrammarSymbol.terminalObjectPred:
			case GrammarSymbol.terminalRandom:
			case GrammarSymbol.terminalToString:
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
				break;

			case GrammarSymbol.terminalIntegerLiteral:
				semanticStack.push(new SmalltalkIntegerValue(value));
				break;

			// case GrammarSymbol.terminalFloatLiteral:
			//     semanticStack.push(new SmalltalkFloatValue((double)value));
			//     break;
			//
			// case GrammarSymbol.terminalStringLiteral:
			//     semanticStack.push(new SmalltalkStringValue((string)value));
			//     break;

			case GrammarSymbol.terminalLeftBracket:
			case GrammarSymbol.terminalRightBracket:
			case GrammarSymbol.terminalBegin:
			case GrammarSymbol.terminalClass:
			case GrammarSymbol.terminalCond:
			case GrammarSymbol.terminalDefine:
			case GrammarSymbol.terminalIf:
			case GrammarSymbol.terminalSet:
			case GrammarSymbol.terminalWhile:
			case GrammarSymbol.terminalEOF:
				// For these terminals, push nothing onto the semantic stack.
				break;

			default:
				// break;
				// throw new GrammarException(
				// 	`pushTokenOntoSemanticStack() : Unexpected tokenAsSymbol ${GrammarSymbol[tokenAsSymbol]} (${tokenAsSymbol})`,
				// 	token.line,
				// 	token.column
				// );
				super.pushTokenOntoSemanticStack(semanticStack, tokenAsSymbol, token);
		}
	}
}
