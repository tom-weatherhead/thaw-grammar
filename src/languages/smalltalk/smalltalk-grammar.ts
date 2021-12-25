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

// ... and fromInteger(value: number) can be implemented as: new SmalltalkInteger(value)

// ****

import { GrammarSymbol, IToken, LexicalState, SemanticStackType } from 'thaw-interpreter-types';

import { ArgumentException, GrammarBase, GrammarException, Name } from 'thaw-interpreter-core';

import { BeginUsage } from '../../common/domain-object-model/begin-usage';

import { IfUsage } from '../../common/domain-object-model/if-usage';

import { CondUsage } from '../../common/domain-object-model/cond-usage';

import { FunctionDefinition } from '../../common/domain-object-model/function-definition';

import { LetStarUsage } from '../../common/domain-object-model/let-star-usage';

import { LetUsage } from '../../common/domain-object-model/let-usage';

import { WhileUsage } from '../../common/domain-object-model/while-usage';

import {
	ISmalltalkExpression,
	ISmalltalkFunctionDefinition,
	ISmalltalkValue,
	ISmalltalkVariable
} from './domain-object-model/interfaces/iexpression';

import { SmalltalkArray } from './domain-object-model/data-types/array';

// import { SmalltalkBeginUsage } from './domain-object-model/begin-usage';
import { SmalltalkCharacter } from './domain-object-model/data-types/character';
import { SmalltalkClass } from './domain-object-model/class';
// import { SmalltalkCondUsage } from './domain-object-model/cond-usage';
import { SmalltalkFloat } from './domain-object-model/data-types/float';
// import { SmalltalkFunctionDefinition } from './domain-object-model/function-definition';
// import { SmalltalkIfUsage } from './domain-object-model/if-usage';
import { SmalltalkInteger } from './domain-object-model/data-types/integer';
// import { SmalltalkLetStarUsage } from './domain-object-model/let-star-usage';
// import { SmalltalkLetUsage } from './domain-object-model/let-usage';
import { SmalltalkOperatorUsage } from './domain-object-model/operator-usage';
import { SmalltalkSetUsage } from './domain-object-model/set-usage';
import { SmalltalkString } from './domain-object-model/data-types/string';
import { SmalltalkSymbol } from './domain-object-model/data-types/symbol';
import { SmalltalkVariable } from './domain-object-model/variable';
// import { SmalltalkWhileUsage } from './domain-object-model/while-usage';

export class SmalltalkGrammar extends GrammarBase {
	// The Smalltalk grammar from Kamin (the book 'Programming Languages: An Interpreter-Based Approach')

	constructor() {
		super(GrammarSymbol.nonterminalStart);

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

		this.addProduction(GrammarSymbol.nonterminalBracketedInput, [
			GrammarSymbol.terminalClass,
			GrammarSymbol.nonterminalClass,
			GrammarSymbol.nonterminalClass,
			GrammarSymbol.nonterminalInstVars, // Actually the class variables; see Exercise 10 on page 347.
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
		this.addProduction(GrammarSymbol.nonterminalValue, [GrammarSymbol.nonterminalSymbol]);

		// Productions.Add(new Production(Symbol.N_Symbol, new List<object>() { GrammarSymbol.terminalOctothorpe, GrammarSymbol.terminalID, "#symbol" }, 46));
		this.addProduction(GrammarSymbol.nonterminalSymbol, [
			GrammarSymbol.terminalOctothorpe,
			GrammarSymbol.terminalID,
			'#symbol'
		]);

		// // This next production allows us to redefine built-in value ops (such as +) in classes.
		// Productions.Add(new Production(Symbol.N_FunDef, new List<object>() {
		//     GrammarSymbol.terminalLeftBracket,
		//     GrammarSymbol.terminalDefine,
		//     Symbol.N_ValueOp,
		//     Symbol.N_ArgList,
		//     Symbol.N_Expression,
		//     GrammarSymbol.terminalRightBracket, "#functionDefinition" }, 48));

		// Productions.Add(new Production(Symbol.N_Value, new List<object>() { GrammarSymbol.terminalFloatLiteral }, 49));
		this.addProduction(GrammarSymbol.nonterminalValue, [GrammarSymbol.terminalFloatLiteral]);

		// Productions.Add(new Production(Symbol.N_Value, new List<object>() { GrammarSymbol.terminalStringLiteral }, 50));
		this.addProduction(GrammarSymbol.nonterminalValue, [GrammarSymbol.terminalStringLiteral]);

		// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { GrammarSymbol.terminalRandom }, 51));
		this.addProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalRandom]);

		// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { GrammarSymbol.terminalToString }, 52));
		// this.addProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminal...]);

		// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { GrammarSymbol.terminalStringToSymbol }, 53));
		// this.addProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminal...]);

		// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { GrammarSymbol.terminalPow }, 54));
		this.addProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalPow]);

		// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { GrammarSymbol.terminalExp }, 55));
		this.addProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalExp]);

		// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { GrammarSymbol.terminalLn }, 56));
		this.addProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalLn]);

		// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { GrammarSymbol.terminalSin }, 57));
		this.addProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalSin]);

		// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { GrammarSymbol.terminalCos }, 58));
		this.addProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalCos]);

		// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { GrammarSymbol.terminalTan }, 59));
		this.addProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalTan]);

		// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { GrammarSymbol.terminalFloor }, 60));
		// this.addProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminal...]);

		// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { GrammarSymbol.terminalAtan2 }, 61));
		// this.addProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminal...]);

		// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { GrammarSymbol.terminalThrow }, 62));
		// this.addProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminal...]);

		// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { GrammarSymbol.terminalStringLessThan }, 63));
		// this.addProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminal...]);

		// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { GrammarSymbol.terminalNumberPred }, 64));
		// this.addProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminal...]);

		// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { GrammarSymbol.terminalSymbolPred }, 65));
		// this.addProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminal...]);

		// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { GrammarSymbol.terminalStringPred }, 66));
		// this.addProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminal...]);

		// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { GrammarSymbol.terminalObjectPred }, 67));
		// this.addProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminal...]);

		// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { GrammarSymbol.terminalStrlen }, 68));
		// this.addProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminal...]);

		// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { GrammarSymbol.terminalSubstr }, 69));
		// this.addProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminal...]);

		// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { GrammarSymbol.terminalTypename }, 70));
		// this.addProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminal...]);

		// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { GrammarSymbol.terminalHash }, 71));
		// this.addProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminal...]);

		// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { GrammarSymbol.terminalReferenceEquals }, 72));
		// this.addProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminal...]);

		// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { GrammarSymbol.terminalStrcat }, 73));
		// this.addProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminal...]);

		// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { GrammarSymbol.terminalNewArray }, 74));
		// this.addProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminal...]);

		// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { GrammarSymbol.terminalArrayLength }, 75));
		// this.addProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminal...]);

		// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { GrammarSymbol.terminalArrayGet }, 76));
		// this.addProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminal...]);

		// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { GrammarSymbol.terminalArraySet }, 77));
		// this.addProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminal...]);

		// Productions.Add(new Production(Symbol.N_Value, new List<object>() {
		//     GrammarSymbol.terminalOctothorpe, GrammarSymbol.terminalLeftBracket, Symbol.N_LiteralList, GrammarSymbol.terminalRightBracket, "#arrayLiteral" }, 78));

		// Productions.Add(new Production(Symbol.N_LiteralList, new List<object>() { Symbol.Lambda, "#emptyLiteralList" }, 79));

		// Productions.Add(new Production(Symbol.N_LiteralList, new List<object>() { Symbol.N_Value, Symbol.N_LiteralList, "#literalList" }, 80));

		// Productions.Add(new Production(Symbol.N_Value, new List<object>() { GrammarSymbol.terminalDollar, GrammarSymbol.terminalID, "#characterLiteral" }, 81));
		this.addProduction(GrammarSymbol.nonterminalValue, [
			GrammarSymbol.terminalDollar,
			GrammarSymbol.terminalID,
			'#characterLiteral'
		]);

		// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { GrammarSymbol.terminalCharPred }, 82));
		// this.addProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalCharPred]);

		// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { GrammarSymbol.terminalStringIndex }, 83));
		// this.addProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalStringIndex]);

		// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { GrammarSymbol.terminalArrayPred }, 84));
		// this.addProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalArrayPred]);
	}

	public get languageName(): string {
		return 'Smalltalk';
	}

	private createLetUsage(
		letKeyword: string,
		varExprList: [ISmalltalkVariable, ISmalltalkExpression][],
		expression: ISmalltalkExpression
	): ISmalltalkExpression {
		// throw new Error('createLetUsage() : Not yet implemented.');

		switch (letKeyword) {
			case 'let':
				return new LetUsage<ISmalltalkValue>(varExprList, expression);

			case 'let*':
				return new LetStarUsage<ISmalltalkValue>(varExprList, expression);

			default:
				throw new ArgumentException(
					`SmalltalkGrammar.CreateLetUsage() : Unknown 'let' keyword '${letKeyword}'.`,
					'letKeyword'
				);
		}
	}

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
		let literal: ISmalltalkValue;
		let literalList: ISmalltalkValue[];
		let array: SmalltalkArray;

		switch (action) {
			case '#functionDefinition':
				body = semanticStack.pop() as ISmalltalkExpression;
				argumentList = semanticStack.pop() as ISmalltalkVariable[];

				functionName = semanticStack.pop() as Name;
				semanticStack.push(
					new FunctionDefinition<ISmalltalkValue>(functionName, argumentList, body)
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
				// semanticStack.push(new SmalltalkIfUsage(expression, expression2, expression3));
				semanticStack.push(
					new IfUsage<ISmalltalkValue>(expression, expression2, expression3)
				);
				break;
			// #endif

			case '#while':
				expression2 = semanticStack.pop() as ISmalltalkExpression;
				expression = semanticStack.pop() as ISmalltalkExpression;
				// semanticStack.push(new SmalltalkWhileUsage(expression, expression2));
				semanticStack.push(new WhileUsage<ISmalltalkValue>(expression, expression2));
				break;

			case '#set':
				expression = semanticStack.pop() as ISmalltalkExpression;
				variable = semanticStack.pop() as ISmalltalkVariable;
				semanticStack.push(new SmalltalkSetUsage(variable, expression));
				break;

			case '#begin':
				expressionList = semanticStack.pop() as ISmalltalkExpression[];
				expression = semanticStack.pop() as ISmalltalkExpression;
				// semanticStack.push(new SmalltalkBeginUsage(expression, expressionList));
				semanticStack.push(new BeginUsage<ISmalltalkValue>(expression, expressionList));
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

			case '#symbol':
				name = semanticStack.pop() as Name;
				semanticStack.push(new SmalltalkSymbol(name.value, name.line, name.column));
				break;

			case '#condUsage':
				exprPairList = semanticStack.pop() as [
					ISmalltalkExpression,
					ISmalltalkExpression
				][];
				expression2 = semanticStack.pop() as ISmalltalkExpression;
				expression = semanticStack.pop() as ISmalltalkExpression;
				exprPairList.unshift([expression, expression2]);
				// semanticStack.push(new SmalltalkCondUsage(exprPairList));
				semanticStack.push(new CondUsage<ISmalltalkValue>(exprPairList));
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

			case '#emptyLiteralList':
				semanticStack.push([] as ISmalltalkValue[]);
				break;

			case '#literalList':
				literalList = semanticStack.pop() as ISmalltalkValue[];
				literal = semanticStack.pop() as ISmalltalkValue;
				literalList.unshift(literal);
				semanticStack.push(literalList);
				break;

			case '#arrayLiteral':
				literalList = semanticStack.pop() as ISmalltalkValue[];

				array = new SmalltalkArray(literalList.length);

				for (let i = 0; i < literalList.length; ++i) {
					array.value[i] = literalList[i];
				}

				semanticStack.push(array);
				break;

			case '#characterLiteral':
				name = semanticStack.pop() as Name;
				semanticStack.push(new SmalltalkCharacter(name.value[0]));
				break;

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
				// case 'random':
				// 	return GrammarSymbol.terminalRandom;
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
				semanticStack.push(new SmalltalkInteger(value, token.line, token.column));
				break;

			case GrammarSymbol.terminalFloatLiteral:
				semanticStack.push(new SmalltalkFloat(value, token.line, token.column));
				break;

			case GrammarSymbol.terminalStringLiteral:
				semanticStack.push(new SmalltalkString(value, token.line, token.column));
				break;

			case GrammarSymbol.terminalClass:
			case GrammarSymbol.terminalDollar:
			case GrammarSymbol.terminalOctothorpe:
				// For these terminals, push nothing onto the semantic stack.
				break;

			default:
				super.pushTokenOntoSemanticStack(semanticStack, tokenAsSymbol, token);
		}
	}
}
