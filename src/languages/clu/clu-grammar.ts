// tom-weatherhead/thaw-grammar/src/languages/clu/clu-grammar.ts

import {
	GrammarSymbol,
	IToken,
	// LanguageSelector,
	LexicalState,
	ParserSelector,
	SemanticStackType
} from 'thaw-interpreter-types';

import { GrammarBase, GrammarException, Name } from 'thaw-interpreter-core';

// import { ExpressionList } from '../../common/domain-object-model/expression-list';

import { ICLUExpression /*, ICLUValue */ } from './domain-object-model/interfaces/ivalue';

import { CLUPrimitiveValue } from './domain-object-model/data-types/primitive-value';

import { CLUOperatorUsage } from './domain-object-model/operator-usage';

import { OnePartFunctionName } from './domain-object-model/one-part-function-name';

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

		this.terminals.push(GrammarSymbol.terminalCluster);
		this.terminals.push(GrammarSymbol.terminalRep);
		this.terminals.push(GrammarSymbol.terminalDollar);
		this.terminals.push(GrammarSymbol.terminalExport);

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

		this.nonTerminals.push(GrammarSymbol.nonterminalClusterDef);
		// this.nonTerminals.push(GrammarSymbol.nonterminalCluster);
		this.nonTerminals.push(GrammarSymbol.nonterminalRep);
		this.nonTerminals.push(GrammarSymbol.nonterminalFunDefList);
		this.nonTerminals.push(GrammarSymbol.nonterminalOnePartName);
		// this.nonTerminals.push(GrammarSymbol.nonterminalTwoPartName);
		this.nonTerminals.push(GrammarSymbol.nonterminalOnePartNameTail);
		this.nonTerminals.push(GrammarSymbol.nonterminalExportList);
		this.nonTerminals.push(GrammarSymbol.nonterminalOnePartNameList);

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

		// this.addProduction(GrammarSymbol.nonterminalOptr, [GrammarSymbol.nonterminalFunction]);

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

		// Productions.Add(new Production(Symbol.N_Input, new List<object>() { Symbol.N_ClusterDef }, 39));
		this.addProduction(GrammarSymbol.nonterminalBracketedInput, [
			GrammarSymbol.nonterminalClusterDef
		]);

		// Productions.Add(new Production(Symbol.N_ClusterDef, new List<object>() {
		// 	Symbol.T_LeftBracket,
		// 	Symbol.T_Cluster,
		// 	Symbol.T_ID, // This T_ID is really a Symbol.N_Cluster
		// 	Symbol.N_ExportList,
		// 	Symbol.N_Rep,
		// 	Symbol.N_FunDef,
		// 	Symbol.N_FunDefList,
		// 	Symbol.T_RightBracket, "#clusterDefinition" }, 40));
		this.addProduction(GrammarSymbol.nonterminalClusterDef, [
			GrammarSymbol.terminalCluster,
			GrammarSymbol.terminalID,
			GrammarSymbol.nonterminalExportList,
			GrammarSymbol.nonterminalRep,
			GrammarSymbol.terminalLeftBracket,
			GrammarSymbol.nonterminalFunDef,
			GrammarSymbol.terminalRightBracket,
			GrammarSymbol.nonterminalFunDefList,
			'#clusterDefinition'
		]);

		// Productions.Add(new Production(Symbol.N_Rep, new List<object>() {
		// 	Symbol.T_LeftBracket,
		// 	Symbol.T_Rep,
		// 	Symbol.N_Variable,
		// 	Symbol.N_VariableList,
		// 	Symbol.T_RightBracket, "#variableList" }, 41));
		this.addProduction(GrammarSymbol.nonterminalRep, [
			GrammarSymbol.terminalLeftBracket,
			GrammarSymbol.terminalRep,
			GrammarSymbol.nonterminalVariable,
			GrammarSymbol.nonterminalVariableList,
			GrammarSymbol.terminalRightBracket,
			'#variableList'
		]);

		// Productions.Add(new Production(Symbol.N_FunDefList, new List<object>() { Symbol.N_FunDef, Symbol.N_FunDefList, "#funDefList" }, 42));
		this.addProduction(GrammarSymbol.nonterminalFunDefList, [
			GrammarSymbol.terminalLeftBracket,
			GrammarSymbol.nonterminalFunDef,
			GrammarSymbol.terminalRightBracket,
			GrammarSymbol.nonterminalFunDefList,
			'#funDefList'
		]);

		// Productions.Add(new Production(Symbol.N_FunDefList, new List<object>() { Symbol.Lambda, "#emptyFunDefList" }, 43));
		this.addProduction(GrammarSymbol.nonterminalFunDefList, [
			GrammarSymbol.Lambda,
			'#emptyFunDefList'
		]);

		/*
		// Productions.Add(new Production(Symbol.N_Optr, new List<object>() { Symbol.N_OnePartName }, 44));
		this.addProduction(GrammarSymbol.nonterminalOptr, [GrammarSymbol.nonterminalOnePartName]);

		// Productions.Add(new Production(Symbol.N_Optr, new List<object>() { Symbol.N_TwoPartName }, 45));
		this.addProduction(GrammarSymbol.nonterminalOptr, [GrammarSymbol.nonterminalTwoPartName]);

		// 2021-10-30 : Grammar error:
		// LL(1) ParserException at line 0 column 0: Error in FillParseTable() : Table entry not unique;
		// p.lhs = 149 nonterminalOptr; t = 15 terminalID;
		// p1 = 39: nonterminalOptr -> nonterminalOnePartName;
		// p2 = 40: nonterminalOptr -> nonterminalTwoPartName

		// Productions.Add(new Production(Symbol.N_OnePartName, new List<object>() { Symbol.T_ID }, 46));
		this.addProduction(GrammarSymbol.nonterminalOnePartName, [GrammarSymbol.terminalID]);

		// Productions.Add(new Production(Symbol.N_TwoPartName, new List<object>() {
		// 	Symbol.T_ID, // This T_ID is really a Symbol.N_Cluster
		// 	Symbol.T_Dollar,
		// 	Symbol.T_ID, "#makeTwoPartName" }, 47));
		this.addProduction(GrammarSymbol.nonterminalTwoPartName, [
			GrammarSymbol.terminalID,
			GrammarSymbol.terminalDollar,
			GrammarSymbol.terminalID, '#makeTwoPartName'
		]);
		 */

		this.addProduction(GrammarSymbol.nonterminalOptr, [
			GrammarSymbol.nonterminalOnePartName,
			GrammarSymbol.nonterminalOnePartNameTail
		]);

		this.addProduction(GrammarSymbol.nonterminalOnePartName, [GrammarSymbol.terminalID]);

		this.addProduction(GrammarSymbol.nonterminalOnePartNameTail, [GrammarSymbol.Lambda]);

		this.addProduction(GrammarSymbol.nonterminalOnePartNameTail, [
			GrammarSymbol.terminalDollar,
			GrammarSymbol.terminalID,
			'#makeTwoPartName'
		]);

		// // SLR(1): There was a reduce-reduce conflict between N_OnePartName -> T_ID and N_Cluster -> T_ID.
		// //Productions.Add(new Production(Symbol.N_Cluster, new List<object>() { Symbol.T_ID }, 48));

		// Productions.Add(new Production(Symbol.N_ExportList, new List<object>() {
		// 	Symbol.T_LeftBracket,
		// 	Symbol.T_Export,
		// 	Symbol.N_OnePartName,
		// 	Symbol.N_OnePartNameList,
		// 	Symbol.T_RightBracket, "#exportList" }, 49));
		this.addProduction(GrammarSymbol.nonterminalExportList, [
			GrammarSymbol.terminalLeftBracket,
			GrammarSymbol.terminalExport,
			GrammarSymbol.nonterminalOnePartName,
			GrammarSymbol.nonterminalOnePartNameList,
			GrammarSymbol.terminalRightBracket,
			'#exportList'
		]);

		// Productions.Add(new Production(Symbol.N_OnePartNameList, new List<object>() { Symbol.N_OnePartName, Symbol.N_OnePartNameList, "#exportList" }, 50));
		this.addProduction(GrammarSymbol.nonterminalOnePartNameList, [
			GrammarSymbol.nonterminalOnePartName,
			GrammarSymbol.nonterminalOnePartNameList,
			'#exportList'
		]);

		// Productions.Add(new Production(Symbol.N_OnePartNameList, new List<object>() { Symbol.Lambda, "#emptyExportList" }, 51));
		this.addProduction(GrammarSymbol.nonterminalOnePartNameList, [
			GrammarSymbol.Lambda,
			'#emptyExportList'
		]);
	}

	public get languageName(): string {
		return 'CLU';
	}

	public override get defaultParser(): ParserSelector {
		return ParserSelector.SLR1;
	}

	// eslint-disable-next-line @typescript-eslint/no-explicit-any, @typescript-eslint/no-unused-vars
	public executeSemanticAction(semanticStack: SemanticStackType, action: string): void {
		// throw new Error('CLUGrammar.executeSemanticAction() : Not yet implemented.');

		let name: Name;
		// let functionName: Name;
		let expression: ICLUExpression;
		// let expression2: IAPLExpression;
		// let expression3: IAPLExpression;
		let expressionList: ICLUExpression[];
		// let exprPairList: [IAPLExpression, IAPLExpression][];
		// let intList: number[];
		// let intScalar: IAPLValue;
		// let variable: Variable<IAPLValue>;
		// let variableList: VariableList<IAPLValue>;
		// let argumentList: VariableList<IAPLValue>;
		// let body: IAPLExpression;

		switch (action) {
			case '#operatorUsage':
				// TODO: Handle TwoPartFunctionName as well as OnePartFunctionName
				expressionList = semanticStack.pop() as ICLUExpression[];
				name = semanticStack.pop() as Name;
				semanticStack.push(
					new CLUOperatorUsage(new OnePartFunctionName(name.value), expressionList)
				);
				break;

			case '#expressionList':
				expressionList = semanticStack.pop() as ICLUExpression[];
				expression = semanticStack.pop() as ICLUExpression;
				expressionList.unshift(expression);
				semanticStack.push(expressionList);
				break;

			case '#emptyExpressionList':
				semanticStack.push([] as ICLUExpression[]);
				break;

			// case '#makeIntVector':
			// 	intList = semanticStack.pop() as number[];
			// 	semanticStack.push(APLValue.createVector1(intList));
			// 	break;
			//
			// case '#intList':
			// 	intList = semanticStack.pop() as number[];
			// 	intScalar = semanticStack.pop() as IAPLValue;
			// 	intList.unshift(intScalar.getFirstScalar());
			// 	semanticStack.push(intList);
			// 	break;
			//
			// case '#emptyIntList':
			// 	semanticStack.push([] as number[]);
			// 	break;
			//
			// case '#functionDefinition':
			// 	body = semanticStack.pop() as IAPLExpression;
			// 	argumentList = semanticStack.pop() as VariableList<IAPLValue>;
			// 	functionName = semanticStack.pop() as Name;
			// 	semanticStack.push(
			// 		new FunctionDefinition<IAPLValue>(functionName, argumentList, body)
			// 	);
			// 	break;
			//
			// case '#variable':
			// 	name = semanticStack.pop() as Name;
			// 	semanticStack.push(new Variable<IAPLValue>(name.value, name.line, name.column));
			// 	break;
			//
			// case '#variableList':
			// 	variableList = semanticStack.pop() as VariableList<IAPLValue>;
			// 	variable = semanticStack.pop() as Variable<IAPLValue>;
			// 	variableList.value.unshift(variable);
			// 	semanticStack.push(variableList);
			// 	break;
			//
			// case '#emptyVariableList':
			// 	semanticStack.push(new VariableList<IAPLValue>());
			// 	break;
			//
			// case '#vecassign':
			// 	expression2 = semanticStack.pop() as IAPLExpression;
			// 	expression = semanticStack.pop() as IAPLExpression;
			// 	variable = semanticStack.pop() as Variable<IAPLValue>;
			// 	semanticStack.push(new VectorAssignmentUsage(variable, expression, expression2));
			// 	break;
			//
			// case '#set':
			// 	expression = semanticStack.pop() as IAPLExpression;
			// 	variable = semanticStack.pop() as Variable<IAPLValue>;
			// 	semanticStack.push(new SetUsage<IAPLValue>(variable, expression));
			// 	break;
			//
			// case '#if':
			// 	expression3 = semanticStack.pop() as IAPLExpression;
			// 	expression2 = semanticStack.pop() as IAPLExpression;
			// 	expression = semanticStack.pop() as IAPLExpression;
			// 	semanticStack.push(new APLIfUsage(expression, expression2, expression3));
			// 	break;
			//
			// case '#while':
			// 	expression2 = semanticStack.pop() as IAPLExpression;
			// 	expression = semanticStack.pop() as IAPLExpression;
			// 	semanticStack.push(new APLWhileUsage(expression, expression2));
			// 	break;
			//
			// case '#begin':
			// 	expressionList = semanticStack.pop() as ExpressionList<IAPLValue>;
			// 	expression = semanticStack.pop() as IAPLExpression;
			// 	semanticStack.push(new BeginUsage<IAPLValue>(expression, expressionList));
			// 	break;

			// From C#:

			// case '#makeFloatVector':
			// 	floatList = (List<double>)semanticStack.Pop();
			// 	floatScalar = (APLValue<double>)semanticStack.Pop();
			// 	floatList.Insert(0, floatScalar.GetFirstScalar());
			// 	semanticStack.Push(APLValue<double>.CreateVector(floatList));
			// 	break;
			//
			// case '#floatList':
			// 	floatList = (List<double>)semanticStack.Pop();
			// 	floatScalar = (APLValue<double>)semanticStack.Pop();
			// 	floatList.Insert(0, floatScalar.GetFirstScalar());
			// 	semanticStack.Push(floatList);
			// 	break;
			//
			// case '#emptyFloatList':
			// 	semanticStack.Push(new List<double>());
			// 	break;

			// case '#condUsage':
			// 	exprPairList = semanticStack.pop() as [IAPLExpression, IAPLExpression][];
			// 	expression2 = semanticStack.pop() as IAPLExpression;
			// 	expression = semanticStack.pop() as IAPLExpression;
			// 	exprPairList.unshift([expression, expression2]);
			// 	semanticStack.push(new APLCondUsage(exprPairList));
			// 	break;
			//
			// case '#exprPairList':
			// 	exprPairList = semanticStack.pop() as [IAPLExpression, IAPLExpression][];
			// 	expression2 = semanticStack.pop() as IAPLExpression;
			// 	expression = semanticStack.pop() as IAPLExpression;
			// 	exprPairList.unshift([expression, expression2]);
			// 	semanticStack.push(exprPairList);
			// 	break;
			//
			// case '#emptyExprPairList':
			// 	semanticStack.push([] as [IAPLExpression, IAPLExpression][]);
			// 	break;

			// case '#letUsage':
			// 	expression = (IAPLExpression)semanticStack.Pop();
			// 	varExprList = (List<KeyValuePair<Variable<IAPLValue>, IAPLExpression>>)semanticStack.Pop();
			//
			// 	var letName = (Name)semanticStack.Pop();
			//
			// 	semanticStack.Push(CreateLetUsage(letName.Value, varExprList, expression));
			// 	break;
			//
			// case '#varExprList':
			// 	varExprList = (List<KeyValuePair<Variable<IAPLValue>, IAPLExpression>>)semanticStack.Pop();
			// 	expression = (IAPLExpression)semanticStack.Pop();
			// 	variable = (Variable<IAPLValue>)semanticStack.Pop();
			// 	varExprList.Insert(0, new KeyValuePair<Variable<IAPLValue>, IAPLExpression>(variable, expression));
			// 	semanticStack.Push(varExprList);
			// 	break;
			//
			// case '#emptyVarExprList':
			// 	semanticStack.Push(new List<KeyValuePair<Variable<IAPLValue>, IAPLExpression>>());
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
			case LexicalState.tokenGreater:
				return GrammarSymbol.terminalGreaterThan;
			// case LexicalState.tokenOctothorpe:
			// 	return GrammarSymbol.terminalOctothorpe;
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
					// CLU-specific:
					case 'cluster':
						return GrammarSymbol.terminalCluster;
					case 'export':
						return GrammarSymbol.terminalExport;
					case 'rep':
						return GrammarSymbol.terminalRep;
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
	// public override pushTokenOntoSemanticStack(
	// 	// eslint-disable-next-line @typescript-eslint/no-explicit-any
	// 	semanticStack: SemanticStackType,
	// 	tokenAsSymbol: number,
	// 	token: IToken
	// ): void {
	// 	throw new Error('CLUGrammar.pushTokenOntoSemanticStack() : Not yet implemented.');
	// }
	/* eslint-enable @typescript-eslint/no-unused-vars */

	public override pushTokenOntoSemanticStack(
		semanticStack: SemanticStackType,
		tokenAsSymbol: GrammarSymbol,
		token: IToken
	): void {
		const value = token.tokenValue;

		switch (tokenAsSymbol) {
			case GrammarSymbol.terminalMax:
				// case GrammarSymbol.terminalOr:
				// case GrammarSymbol.terminalAnd:
				// case GrammarSymbol.terminalPlusSlash:
				// case GrammarSymbol.terminalMinusSlash:
				// case GrammarSymbol.terminalMultiplySlash:
				// case GrammarSymbol.terminalDivideSlash:
				// case GrammarSymbol.terminalMaxSlash:
				// case GrammarSymbol.terminalOrSlash:
				// case GrammarSymbol.terminalAndSlash:
				// case GrammarSymbol.terminalCompress:
				// case GrammarSymbol.terminalShape:
				// case GrammarSymbol.terminalRavel:
				// case GrammarSymbol.terminalRestruct:
				// case GrammarSymbol.terminalCat:
				// case GrammarSymbol.terminalIndx:
				// case GrammarSymbol.terminalTrans:
				// case GrammarSymbol.terminalSquareBrackets:
				// case GrammarSymbol.terminalDoubleSubscripting:
				// Handled by base class:
				// case GrammarSymbol.terminalRandom:
				// case GrammarSymbol.terminalPow:
				// case GrammarSymbol.terminalExp:
				// case GrammarSymbol.terminalLn:
				// case GrammarSymbol.terminalSin:
				// case GrammarSymbol.terminalCos:
				// case GrammarSymbol.terminalTan:
				semanticStack.push(new Name(value as string, token.line, token.column));
				// Or: semanticStack.push(new Name(value.toString(), token.line, token.column));
				// Or: semanticStack.push(new Name(`${value}`, token.line, token.column));
				break;

			case GrammarSymbol.terminalIntegerLiteral:
				semanticStack.push(
					new CLUPrimitiveValue(value as number /*, token.line, token.column */)
				);
				break;

			// case GrammarSymbol.terminalFloatLiteral:
			// 	semanticStack.push(new SmalltalkFloat(value, token.line, token.column));
			// 	break;
			//
			// case GrammarSymbol.terminalStringLiteral:
			// 	semanticStack.push(new SmalltalkString(value, token.line, token.column));
			// 	break;

			// case GrammarSymbol.terminalApostrophe:
			// case GrammarSymbol.terminalAssign:
			// 	// For these terminals, push nothing onto the semantic stack.
			// 	break;

			default:
				super.pushTokenOntoSemanticStack(semanticStack, tokenAsSymbol, token);
		}
	}
}
