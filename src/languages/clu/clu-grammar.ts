// tom-weatherhead/thaw-grammar/src/languages/clu/clu-grammar.ts

import {
	GrammarSymbol,
	IToken,
	LexicalState,
	ParserSelector,
	SemanticStackType
} from 'thaw-interpreter-types';

import { ArgumentException, GrammarBase, GrammarException, Name } from 'thaw-interpreter-core';

import {
	ICLUExpression,
	ICLUFunctionDefinition,
	// ICLUValue,
	ICLUVariable
} from './domain-object-model/interfaces/ivalue';

import { CLUPrimitiveValue } from './domain-object-model/data-types/primitive-value';

import { Cluster } from './domain-object-model/cluster';

import { CLUCondUsage } from './domain-object-model/cond-usage';

import { CLUBeginUsage } from './domain-object-model/begin-usage';

import { CLUIfUsage } from './domain-object-model/if-usage';

import { CLULetUsage } from './domain-object-model/let-usage';

import { CLULetStarUsage } from './domain-object-model/let-star-usage';

import { CLUNormalFunctionDefinition } from './domain-object-model/normal-function-definition';

import { CLUOperatorUsage } from './domain-object-model/operator-usage';

import { OnePartFunctionName } from './domain-object-model/one-part-function-name';

import { CLUSetUsage } from './domain-object-model/set-usage';

import {
	isTwoPartFunctionName,
	TwoPartFunctionName
} from './domain-object-model/two-part-function-name';

import { CLUVariable } from './domain-object-model/variable';

import { CLUWhileUsage } from './domain-object-model/while-usage';

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
		this.terminals.push(GrammarSymbol.terminalCond);
		this.terminals.push(GrammarSymbol.terminalLet);
		this.terminals.push(GrammarSymbol.terminalLetStar);

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
		this.nonTerminals.push(GrammarSymbol.nonterminalExprPairList);
		this.nonTerminals.push(GrammarSymbol.nonterminalLetKeyword);
		this.nonTerminals.push(GrammarSymbol.nonterminalVarExprList);

		this.nonTerminals.push(GrammarSymbol.nonterminalClusterDef);
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

		// CLU Productions

		this.addProduction(GrammarSymbol.nonterminalBracketedInput, [
			GrammarSymbol.nonterminalClusterDef
		]);

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

		this.addProduction(GrammarSymbol.nonterminalRep, [
			GrammarSymbol.terminalLeftBracket,
			GrammarSymbol.terminalRep,
			GrammarSymbol.nonterminalVariable,
			GrammarSymbol.nonterminalVariableList,
			GrammarSymbol.terminalRightBracket,
			'#variableList'
		]);

		this.addProduction(GrammarSymbol.nonterminalFunDefList, [
			GrammarSymbol.terminalLeftBracket,
			GrammarSymbol.nonterminalFunDef,
			GrammarSymbol.terminalRightBracket,
			GrammarSymbol.nonterminalFunDefList,
			'#funDefList'
		]);

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

		this.addProduction(GrammarSymbol.nonterminalExportList, [
			GrammarSymbol.terminalLeftBracket,
			GrammarSymbol.terminalExport,
			GrammarSymbol.nonterminalOnePartName,
			GrammarSymbol.nonterminalOnePartNameList,
			GrammarSymbol.terminalRightBracket,
			'#exportList'
		]);

		this.addProduction(GrammarSymbol.nonterminalOnePartNameList, [
			GrammarSymbol.nonterminalOnePartName,
			GrammarSymbol.nonterminalOnePartNameList,
			'#exportList'
		]);

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

	public executeSemanticAction(semanticStack: SemanticStackType, action: string): void {
		let name: Name;
		let functionName: Name;
		let letName: Name;
		let clusterName: Name;
		let expression: ICLUExpression;
		let expression2: ICLUExpression;
		let expression3: ICLUExpression;
		let expressionList: ICLUExpression[];
		let variable: ICLUVariable;
		let variableList: ICLUVariable[];
		let argumentList: ICLUVariable[];
		let body: ICLUExpression;
		let funDef: ICLUFunctionDefinition;
		let funDefList: ICLUFunctionDefinition[];
		let exportSet: string[];
		let varExprList: [ICLUVariable, ICLUExpression][];
		let exprPairList: [ICLUExpression, ICLUExpression][];

		switch (action) {
			case '#operatorUsage':
				expressionList = semanticStack.pop() as ICLUExpression[];

				const operatorNameAsObject: Name | TwoPartFunctionName = semanticStack.pop(); // TODO: This may be a one-part name or a two-part name.

				if (isTwoPartFunctionName(operatorNameAsObject)) {
					semanticStack.push(new CLUOperatorUsage(operatorNameAsObject, expressionList));
				} else {
					name = operatorNameAsObject as Name;
					semanticStack.push(
						new CLUOperatorUsage(new OnePartFunctionName(name.value), expressionList)
					);
				}

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

			case '#makeTwoPartName':
				functionName = semanticStack.pop() as Name;
				clusterName = semanticStack.pop() as Name;
				semanticStack.push(new TwoPartFunctionName(clusterName.value, functionName.value));
				break;

			case '#variable':
				name = semanticStack.pop() as Name;
				semanticStack.push(new CLUVariable(name.value));
				break;

			case '#variableList':
				variableList = semanticStack.pop() as ICLUVariable[];
				variable = semanticStack.pop() as ICLUVariable;
				variableList.unshift(variable);
				semanticStack.push(variableList);
				break;

			case '#emptyVariableList':
				semanticStack.push([] as ICLUVariable[]);
				break;

			case '#functionDefinition':
				body = semanticStack.pop() as ICLUExpression;
				argumentList = semanticStack.pop() as ICLUVariable[];
				functionName = semanticStack.pop() as Name;
				semanticStack.push(
					new CLUNormalFunctionDefinition(functionName.value, argumentList, body)
				);
				break;

			case '#funDefList':
				funDefList = semanticStack.pop() as ICLUFunctionDefinition[];
				funDef = semanticStack.pop() as ICLUFunctionDefinition;
				funDefList.unshift(funDef);
				semanticStack.push(funDefList);
				break;

			case '#emptyFunDefList':
				semanticStack.push([] as ICLUFunctionDefinition[]);
				break;

			case '#exportList':
				exportSet = semanticStack.pop() as string[];
				name = semanticStack.pop() as Name;

				if (exportSet.indexOf(name.value) < 0) {
					exportSet.push(name.value);
				}

				semanticStack.push(exportSet);
				break;

			case '#emptyExportList':
				semanticStack.push([] as string[]);
				break;

			case '#clusterDefinition':
				funDefList = semanticStack.pop() as ICLUFunctionDefinition[];
				funDef = semanticStack.pop() as ICLUFunctionDefinition;
				funDefList.unshift(funDef);
				variableList = semanticStack.pop() as ICLUVariable[];
				exportSet = semanticStack.pop() as string[];
				name = semanticStack.pop() as Name;
				semanticStack.push(new Cluster(name.value, exportSet, variableList, funDefList));
				break;

			case '#if':
				expression3 = semanticStack.pop() as ICLUExpression;
				expression2 = semanticStack.pop() as ICLUExpression;
				expression = semanticStack.pop() as ICLUExpression;
				semanticStack.push(new CLUIfUsage(expression, expression2, expression3));
				break;

			case '#while':
				expression2 = semanticStack.pop() as ICLUExpression;
				expression = semanticStack.pop() as ICLUExpression;
				semanticStack.push(new CLUWhileUsage(expression, expression2));
				break;

			case '#set':
				expression = semanticStack.pop() as ICLUExpression;
				variable = semanticStack.pop() as ICLUVariable;
				semanticStack.push(new CLUSetUsage(variable, expression));
				break;

			case '#begin':
				expressionList = semanticStack.pop() as ICLUExpression[];
				expression = semanticStack.pop() as ICLUExpression;
				semanticStack.push(new CLUBeginUsage(expression, expressionList));
				break;

			case '#condUsage':
				exprPairList = semanticStack.pop() as [ICLUExpression, ICLUExpression][];
				expression2 = semanticStack.pop() as ICLUExpression;
				expression = semanticStack.pop() as ICLUExpression;
				exprPairList.unshift([expression, expression2]);
				semanticStack.push(new CLUCondUsage(exprPairList));
				break;

			case '#exprPairList':
				exprPairList = semanticStack.pop() as [ICLUExpression, ICLUExpression][];
				expression2 = semanticStack.pop() as ICLUExpression;
				expression = semanticStack.pop() as ICLUExpression;
				exprPairList.unshift([expression, expression2]);
				semanticStack.push(exprPairList);
				break;

			case '#emptyExprPairList':
				semanticStack.push([] as [ICLUExpression, ICLUExpression][]);
				break;

			case '#letUsage':
				expression = semanticStack.pop() as ICLUExpression;
				varExprList = semanticStack.pop() as [ICLUVariable, ICLUExpression][];
				letName = semanticStack.pop() as Name;
				semanticStack.push(this.createLetUsage(letName, varExprList, expression));
				break;

			case '#varExprList':
				varExprList = semanticStack.pop() as [ICLUVariable, ICLUExpression][];
				expression = semanticStack.pop() as ICLUExpression;
				variable = semanticStack.pop() as ICLUVariable;
				varExprList.unshift([variable, expression]);
				semanticStack.push(varExprList);
				break;

			case '#emptyVarExprList':
				semanticStack.push([] as [ICLUVariable, ICLUExpression][]);
				break;

			default:
				throw new GrammarException(`APL: Unrecognized semantic action: ${action}`);
		}
	}

	public override tokenToSymbol(token: IToken): GrammarSymbol {
		switch (token.tokenType) {
			case LexicalState.tokenDollar:
				return GrammarSymbol.terminalDollar;
			case LexicalState.tokenGreaterEqual:
				return GrammarSymbol.terminalID;
			case LexicalState.tokenIdent:
				switch (token.tokenValue as string) {
					// CLU-specific:
					case 'cluster':
						return GrammarSymbol.terminalCluster;
					case 'export':
						return GrammarSymbol.terminalExport;
					case 'rep':
						return GrammarSymbol.terminalRep;
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
			// case GrammarSymbol.terminalMax:
			// 	semanticStack.push(new Name(value as string, token.line, token.column));
			// 	// Or: semanticStack.push(new Name(value.toString(), token.line, token.column));
			// 	// Or: semanticStack.push(new Name(`${value}`, token.line, token.column));
			// 	break;

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

			case GrammarSymbol.terminalCluster:
			case GrammarSymbol.terminalDollar:
			case GrammarSymbol.terminalExport:
			case GrammarSymbol.terminalRep:
				// For these terminals, push nothing onto the semantic stack.
				break;

			default:
				super.pushTokenOntoSemanticStack(semanticStack, tokenAsSymbol, token);
		}
	}

	protected createLetUsage(
		letName: Name,
		varExprList: [ICLUVariable, ICLUExpression][],
		expression: ICLUExpression
	): ICLUExpression {
		switch (letName.value) {
			case 'let':
				return new CLULetUsage(varExprList, expression);

			case 'let*':
				return new CLULetStarUsage(varExprList, expression);

			default:
				throw new ArgumentException(
					`CLUGrammar.createLetUsage() : Unknown 'let' keyword '${letName.value}.`,
					'letName',
					letName.line,
					letName.column
				);
		}
	}
}
