// tom-weatherhead/thaw-grammar/src/languages/micro/micro-grammar.ts

'use strict';

import { Stack } from 'thaw-common-utilities.ts';

import {
	// LexicalState,
	Token
} from 'thaw-lexical-analyzer';

// import { ExpressionList }  from '../../common/domain-object-model/expression-list';
// import { IExpression }  from '../../common/domain-object-model/iexpression';
// import { Name }  from '../../common/domain-object-model/name';
// import { Variable }  from '../../common/domain-object-model/variable';
// import { VariableList }  from '../../common/domain-object-model/variable-list';

// import { ArgumentException } from '../../common/exceptions/argument-exception';
// import { GrammarException } from '../../common/exceptions/grammar-exception';

import { GrammarBase } from '../../common/grammar-base';
import { ParserSelector } from '../../common/parser-selectors';
// import { Production }  from '../../common/production';
import { Symbol } from '../../common/symbol';

// export function dummyMicroGrammar() : string {
// 	return 'dummyMicroGrammar';
// }

export class MicroGrammar extends GrammarBase {
	constructor() {
		super(Symbol.nonterminalStart);

		// ...
	}

	public get languageName(): string {
		return 'Micro';
	}

	public get selectorsOfCompatibleParsers(): number[] {
		return [ParserSelector.LL1];
	}

	public executeSemanticAction(
		semanticStack: Stack<any>,
		action: string
	): void {}

	public tokenToSymbol(token: Token): number {
		// Returns Symbol
		return Symbol.UndefinedSymbol;
	}

	public pushTokenOntoSemanticStack(
		semanticStack: Stack<any>,
		tokenAsSymbol: number,
		token: Token
	): void {}
}

// export class MicroGrammar extends GrammarBase {
// // The "Micro" grammar from Fischer and LeBlanc (the book 'Crafting a compiler in C', the textbook
// // for the Compiler Construction (CS 444?) course at the University of Waterloo;
// // I took this course in January - April 1994.)

// 	constructor() {
// 		super(Symbol.nonterminalStart);

//         Terminals.Add(Symbol.T_Begin);
//         Terminals.Add(Symbol.T_End);
//         Terminals.Add(Symbol.T_Assign);
//         Terminals.Add(Symbol.T_Semicolon);
//         Terminals.Add(Symbol.T_Read);
//         Terminals.Add(Symbol.T_Write);
//         Terminals.Add(Symbol.T_LeftBracket);
//         Terminals.Add(Symbol.T_RightBracket);
//         Terminals.Add(Symbol.T_Comma);
//         Terminals.Add(Symbol.T_ID);
//         Terminals.Add(Symbol.T_IntegerLiteral);
//         Terminals.Add(Symbol.T_Plus);
//         Terminals.Add(Symbol.T_Minus);
//         Terminals.Add(Symbol.T_EOF);

//         NonTerminals.Add(Symbol.N_Start);
//         NonTerminals.Add(Symbol.N_Program);
//         NonTerminals.Add(Symbol.N_StatementList);
//         NonTerminals.Add(Symbol.N_StatementTail);
//         NonTerminals.Add(Symbol.N_Statement);
//         NonTerminals.Add(Symbol.N_IDList);
//         NonTerminals.Add(Symbol.N_IDTail);
//         NonTerminals.Add(Symbol.N_ExprList);
//         NonTerminals.Add(Symbol.N_ExprTail);
//         NonTerminals.Add(Symbol.N_Expression);
//         NonTerminals.Add(Symbol.N_PrimaryTail);
//         NonTerminals.Add(Symbol.N_Primary);
//         NonTerminals.Add(Symbol.N_AddOp);

//         // See Fischer and LeBlanc, page 113
//         Productions.Add(new Production(Symbol.N_Program, new List<object>() { Symbol.T_Begin, Symbol.N_StatementList, Symbol.T_End }, 1)); // 1
//         Productions.Add(new Production(Symbol.N_StatementList, new List<object>() { Symbol.N_Statement, Symbol.N_StatementTail }, 2)); // 2
//         Productions.Add(new Production(Symbol.N_StatementTail, new List<object>() { Symbol.N_Statement, Symbol.N_StatementTail }, 3)); // 3
//         Productions.Add(new Production(Symbol.N_StatementTail, new List<object>() { Symbol.Lambda }, 4)); // 4
//         Productions.Add(new Production(Symbol.N_Statement, new List<object>() { Symbol.T_ID, Symbol.T_Assign, Symbol.N_Expression, Symbol.T_Semicolon }, 5)); // 5
//         Productions.Add(new Production(Symbol.N_Statement, new List<object>() { Symbol.T_Read, Symbol.T_LeftBracket, Symbol.N_IDList, Symbol.T_RightBracket, Symbol.T_Semicolon }, 6)); // 6
//         Productions.Add(new Production(Symbol.N_Statement, new List<object>() { Symbol.T_Write, Symbol.T_LeftBracket, Symbol.N_ExprList, Symbol.T_RightBracket, Symbol.T_Semicolon }, 7)); // 7
//         Productions.Add(new Production(Symbol.N_IDList, new List<object>() { Symbol.T_ID, Symbol.N_IDTail }, 8)); // 8
//         Productions.Add(new Production(Symbol.N_IDTail, new List<object>() { Symbol.T_Comma, Symbol.T_ID, Symbol.N_IDTail }, 9)); // 9
//         Productions.Add(new Production(Symbol.N_IDTail, new List<object>() { Symbol.Lambda }, 10)); // 10
//         Productions.Add(new Production(Symbol.N_ExprList, new List<object>() { Symbol.N_Expression, Symbol.N_ExprTail }, 11)); // 11
//         Productions.Add(new Production(Symbol.N_ExprTail, new List<object>() { Symbol.T_Comma, Symbol.N_Expression, Symbol.N_ExprTail }, 12)); // 12
//         Productions.Add(new Production(Symbol.N_ExprTail, new List<object>() { Symbol.Lambda }, 13)); // 13
//         Productions.Add(new Production(Symbol.N_Expression, new List<object>() { Symbol.N_Primary, Symbol.N_PrimaryTail }, 14)); // 14
//         Productions.Add(new Production(Symbol.N_PrimaryTail, new List<object>() { Symbol.N_AddOp, Symbol.N_Primary, Symbol.N_PrimaryTail }, 15)); // 15
//         Productions.Add(new Production(Symbol.N_PrimaryTail, new List<object>() { Symbol.Lambda }, 16)); // 16
//         Productions.Add(new Production(Symbol.N_Primary, new List<object>() { Symbol.T_LeftBracket, Symbol.N_Expression, Symbol.T_RightBracket }, 17)); // 17
//         Productions.Add(new Production(Symbol.N_Primary, new List<object>() { Symbol.T_ID }, 18)); // 18
//         Productions.Add(new Production(Symbol.N_Primary, new List<object>() { Symbol.T_IntegerLiteral }, 19)); // 19
//         Productions.Add(new Production(Symbol.N_AddOp, new List<object>() { Symbol.T_Plus }, 20)); // 20
//         Productions.Add(new Production(Symbol.N_AddOp, new List<object>() { Symbol.T_Minus }, 21)); // 21
//         Productions.Add(new Production(Symbol.N_Start, new List<object>() { Symbol.N_Program, Symbol.T_EOF }, 22)); // 22
//     }

//     public override void ExecuteSemanticAction(Stack<object> semanticStack, string action)
//     {
//         throw new NotImplementedException("MicroGrammar.ExecuteSemanticAction()");
//     }

//     public override Symbol TokenToSymbol(Token token)
//     {
//         string tokenValueAsString = token.TokenValue.ToString();

//         switch (token.TokenType)
//         {
//             case TokenType.T_Ident:

//                 switch (tokenValueAsString)
//                 {
//                     case "begin": return Symbol.T_Begin;
//                     case "end": return Symbol.T_End;
//                     case "read": return Symbol.T_Read;
//                     case "write": return Symbol.T_Write;
//                     default: break;
//                 }

//                 break;

//             case TokenType.T_Assign: return Symbol.T_Assign;
//             case TokenType.T_Semicolon: return Symbol.T_Semicolon;
//             case TokenType.T_Minus: return Symbol.T_Minus;

//             default:
//                 break;
//         }

//         return base.TokenToSymbol(token);
//     }

//     public override void PushTokenOntoSemanticStack(Stack<object> semanticStack, Symbol tokenAsSymbol, Token token)
//     {
//         throw new NotImplementedException("MicroGrammar.PushTokenOntoSemanticStack()");
//     }
// }
