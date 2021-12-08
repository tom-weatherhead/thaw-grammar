// tom-weatherhead/thaw-grammar/src/languages/inference/inference-grammar.ts

// import { Stack } from 'thaw-common-utilities.ts';

import {
	GrammarSymbol,
	IToken,
	// LexicalState,
	// ParserSelector,
	SemanticStackType
} from 'thaw-interpreter-types';

// import { Token } from 'thaw-lexical-analyzer';

import { GrammarBase } from 'thaw-interpreter-core';
// import { ParserSelector } from '../../common/parser-selectors';
// import { Symbol } from '../../common/symbol';

export class InferenceGrammar extends GrammarBase {
	constructor() {
		super(GrammarSymbol.nonterminalStart);
	}

	public get languageName(): string {
		return 'Inference';
	}

	// eslint-disable-next-line @typescript-eslint/no-unused-vars
	public executeSemanticAction(semanticStack: SemanticStackType, action: string): void {}

	// eslint-disable-next-line @typescript-eslint/no-unused-vars
	public override tokenToSymbol(token: IToken): GrammarSymbol {
		return GrammarSymbol.UndefinedSymbol;
	}

	/* eslint-disable @typescript-eslint/no-unused-vars */
	public override pushTokenOntoSemanticStack(
		semanticStack: SemanticStackType,
		tokenAsSymbol: GrammarSymbol,
		token: IToken
	): void {}
	/* eslint-enable @typescript-eslint/no-unused-vars */
}

// export class InferenceGrammar extends GrammarBase {

// 	constructor() {
// 		super(Symbol.nonterminalStart);

//         Terminals.Add(Symbol.T_LeftBracket);
//         Terminals.Add(Symbol.T_RightBracket);
//         Terminals.Add(Symbol.T_Comma);
//         Terminals.Add(Symbol.T_ID);
//         Terminals.Add(Symbol.T_BoolID);
//         Terminals.Add(Symbol.T_SkolemID);
//         Terminals.Add(Symbol.T_IntegerLiteral);
//         Terminals.Add(Symbol.T_StringLiteral);
//         Terminals.Add(Symbol.T_Variable);
//         Terminals.Add(Symbol.T_Exclamation);
//         Terminals.Add(Symbol.T_2Ampersand);
//         Terminals.Add(Symbol.T_2OrBar);
//         Terminals.Add(Symbol.T_Arrow);
//         Terminals.Add(Symbol.T_ForAll);
//         Terminals.Add(Symbol.T_Exists);
//         Terminals.Add(Symbol.T_EOF);

//         NonTerminals.Add(Symbol.N_Start);
//         NonTerminals.Add(Symbol.N_BoolExpr);
//         NonTerminals.Add(Symbol.N_BoolExprTail);
//         NonTerminals.Add(Symbol.N_JunctionTail);
//         NonTerminals.Add(Symbol.N_BoolAtom);
//         NonTerminals.Add(Symbol.N_ArgumentList);
//         NonTerminals.Add(Symbol.N_ArgumentTail);
//         NonTerminals.Add(Symbol.N_Argument);
//         NonTerminals.Add(Symbol.N_ArgIdentTail);

//         Productions.Add(new Production(Symbol.N_Start, new List<object>() { Symbol.N_BoolExpr, Symbol.T_EOF }, 1));
//         Productions.Add(new Production(Symbol.N_BoolExpr, new List<object>() { Symbol.N_BoolAtom, Symbol.N_BoolExprTail }, 2));
//         Productions.Add(new Production(Symbol.N_BoolExprTail, new List<object>() { Symbol.N_JunctionTail, Symbol.N_BoolExprTail }, 3));
//         Productions.Add(new Production(Symbol.N_JunctionTail, new List<object>() { Symbol.T_2Ampersand, Symbol.N_BoolAtom, "#makeConj" }, 4));
//         Productions.Add(new Production(Symbol.N_JunctionTail, new List<object>() { Symbol.T_2OrBar, Symbol.N_BoolAtom, "#makeDisj" }, 5));
//         Productions.Add(new Production(Symbol.N_BoolExprTail, new List<object>() { Symbol.T_Arrow, Symbol.N_BoolAtom, "#makeArrow" }, 6));
//         Productions.Add(new Production(Symbol.N_BoolExprTail, new List<object>() { Symbol.Lambda }, 7));
//         Productions.Add(new Production(Symbol.N_BoolAtom, new List<object>() { Symbol.T_LeftBracket, Symbol.N_BoolExpr, Symbol.T_RightBracket }, 8));
//         Productions.Add(new Production(Symbol.N_BoolAtom, new List<object>() { Symbol.T_Exclamation, Symbol.N_BoolAtom, "#makeNeg" }, 9));
//         Productions.Add(new Production(Symbol.N_BoolAtom, new List<object>() { Symbol.T_ForAll, Symbol.T_Variable, Symbol.T_LeftBracket, Symbol.N_BoolExpr, Symbol.T_RightBracket, "#makeForAll" }, 10));
//         Productions.Add(new Production(Symbol.N_BoolAtom, new List<object>() { Symbol.T_Exists, Symbol.T_Variable, Symbol.T_LeftBracket, Symbol.N_BoolExpr, Symbol.T_RightBracket, "#makeExists" }, 11));
//         Productions.Add(new Production(Symbol.N_BoolAtom, new List<object>() { Symbol.T_BoolID, Symbol.T_LeftBracket, Symbol.N_ArgumentList, Symbol.T_RightBracket, "#makePred" }, 12));
//         Productions.Add(new Production(Symbol.N_ArgumentList, new List<object>() { Symbol.N_Argument, Symbol.N_ArgumentTail, "#makeArgList" }, 13));
//         Productions.Add(new Production(Symbol.N_ArgumentList, new List<object>() { Symbol.Lambda, "#null" }, 14));
//         Productions.Add(new Production(Symbol.N_ArgumentTail, new List<object>() { Symbol.T_Comma, Symbol.N_Argument, Symbol.N_ArgumentTail, "#makeArgList" }, 15));
//         Productions.Add(new Production(Symbol.N_ArgumentTail, new List<object>() { Symbol.Lambda, "#null" }, 16));
//         Productions.Add(new Production(Symbol.N_Argument, new List<object>() { Symbol.T_IntegerLiteral }, 17));
//         Productions.Add(new Production(Symbol.N_Argument, new List<object>() { Symbol.T_StringLiteral }, 18));
//         Productions.Add(new Production(Symbol.N_Argument, new List<object>() { Symbol.T_Variable }, 19));
//         Productions.Add(new Production(Symbol.N_Argument, new List<object>() { Symbol.T_SkolemID, Symbol.T_LeftBracket, Symbol.N_ArgumentList, Symbol.T_RightBracket, "#makeSkolemFunc" }, 20));
//         Productions.Add(new Production(Symbol.N_Argument, new List<object>() { Symbol.T_ID, Symbol.N_ArgIdentTail }, 21));
//         Productions.Add(new Production(Symbol.N_ArgIdentTail, new List<object>() { Symbol.T_LeftBracket, Symbol.N_ArgumentList, Symbol.T_RightBracket, "#makeFunc" }, 22));
//         Productions.Add(new Production(Symbol.N_ArgIdentTail, new List<object>() { Symbol.Lambda }, 23));
//     }

//     public override void ExecuteSemanticAction(Stack<object> semanticStack, string action)
//     {
//         Variable v = null;
//         ArgumentList argList = null;
//         IBooleanExpression expr1 = null;
//         IBooleanExpression expr2 = null;

//         switch (action)
//         {
//             case "#null":
//                 //Console.WriteLine("Semantic stack: pushing null...");
//                 semanticStack.Push(null);
//                 break;

//             case "#makeArgList":
//                 //Console.WriteLine("Semantic stack: making argument list...");

//                 ArgumentList argList2 = semanticStack.Pop() as ArgumentList;
//                 IArgument arg1 = semanticStack.Pop() as IArgument;

//                 semanticStack.Push(new ArgumentList(arg1, argList2));
//                 break;

//             case "#makeFunc":
//                 //Console.WriteLine("Semantic stack: making function...");
//                 argList = semanticStack.Pop() as ArgumentList;

//                 Constant c = semanticStack.Pop() as Constant;

//                 semanticStack.Push(new Function(c, argList));
//                 break;

//             case "#makeSkolemFunc":
//                 //Console.WriteLine("Semantic stack: making Skolem function...");
//                 argList = semanticStack.Pop() as ArgumentList;

//                 string name = semanticStack.Pop() as string;

//                 semanticStack.Push(new SkolemFunction(name, argList));
//                 break;

//             case "#makePred":
//                 //Console.WriteLine("Semantic stack: making predicate...");
//                 argList = semanticStack.Pop() as ArgumentList;

//                 BooleanConstant bc = semanticStack.Pop() as BooleanConstant;

//                 semanticStack.Push(new Predicate(bc, argList));
//                 break;

//             case "#makeNeg":
//                 //Console.WriteLine("Semantic stack: making negation...");
//                 expr1 = semanticStack.Pop() as IBooleanExpression;
//                 semanticStack.Push(new Negation(expr1));
//                 break;

//             case "#makeConj":
//                 //Console.WriteLine("Semantic stack: making conjunction...");
//                 expr2 = semanticStack.Pop() as IBooleanExpression;
//                 expr1 = semanticStack.Pop() as IBooleanExpression;
//                 semanticStack.Push(new Conjunction(expr1, expr2));
//                 break;

//             case "#makeDisj":
//                 //Console.WriteLine("Semantic stack: making disjunction...");
//                 expr2 = semanticStack.Pop() as IBooleanExpression;
//                 expr1 = semanticStack.Pop() as IBooleanExpression;
//                 semanticStack.Push(new Disjunction(expr1, expr2));
//                 break;

//             case "#makeArrow":
//                 //Console.WriteLine("Semantic stack: making arrow...");
//                 expr2 = semanticStack.Pop() as IBooleanExpression;
//                 expr1 = semanticStack.Pop() as IBooleanExpression;
//                 semanticStack.Push(new Disjunction(new Negation(expr1), expr2));
//                 break;

//             case "#makeForAll":
//                 //Console.WriteLine("Semantic stack: making for all...");
//                 expr1 = semanticStack.Pop() as IBooleanExpression;
//                 v = semanticStack.Pop() as Variable;
//                 semanticStack.Push(new ForAll(v, expr1));
//                 break;

//             case "#makeExists":
//                 //Console.WriteLine("Semantic stack: making exists...");
//                 expr1 = semanticStack.Pop() as IBooleanExpression;
//                 v = semanticStack.Pop() as Variable;
//                 semanticStack.Push(new Exists(v, expr1));
//                 break;

//             default:
//                 throw new ArgumentException(string.Format("Unrecognized semantic action: {0}", action), "action");
//         }
//     }

//     public override Symbol TokenToSymbol(Token token)
//     {
//         string tokenValueAsString = token.TokenValue.ToString();

//         switch (token.TokenType)
//         {
//             case TokenType.T_Ident:

//                 switch (tokenValueAsString)
//                 {
//                     case "A": return Symbol.T_ForAll;
//                     case "E": return Symbol.T_Exists;
//                     default: break;
//                 }

//                 break;

//             case TokenType.T_BoolIdent: return Symbol.T_BoolID;
//             case TokenType.T_SkolemIdent: return Symbol.T_SkolemID;
//             case TokenType.T_Exclamation: return Symbol.T_Exclamation;
//             case TokenType.T_Variable: return Symbol.T_Variable;
//             case TokenType.T_2Ampersand: return Symbol.T_2Ampersand;
//             case TokenType.T_2OrBar: return Symbol.T_2OrBar;
//             case TokenType.T_Arrow: return Symbol.T_Arrow;
//             case TokenType.T_StrLit: return Symbol.T_StringLiteral;
//             default: break;
//         }

//         return base.TokenToSymbol(token);
//     }

//     public override void PushTokenOntoSemanticStack(Stack<object> semanticStack, Symbol tokenAsSymbol, Token token)
//     {
//         var value = token.TokenValue;

//         switch (tokenAsSymbol)
//         {
//             case Symbol.T_ID:
//                 semanticStack.Push(new Constant(value as string));
//                 //Console.WriteLine("Pushed Constant {0} onto the semantic stack.", value);
//                 break;

//             case Symbol.T_IntegerLiteral:
//                 semanticStack.Push(new IntegerLiteral(value));
//                 //Console.WriteLine("Pushed IntegerLiteral {0} onto the semantic stack.", value);
//                 break;

//             case Symbol.T_StringLiteral:
//                 semanticStack.Push(new StringLiteral(value));
//                 //Console.WriteLine("Pushed StringLiteral {0} onto the semantic stack.", value);
//                 break;

//             case Symbol.T_BoolID:
//                 semanticStack.Push(new BooleanConstant(value as string));
//                 //Console.WriteLine("Pushed BooleanConstant {0} onto the semantic stack.", value);
//                 break;

//             case Symbol.T_SkolemID:
//                 //semanticStack.Push(new SkolemConstant(value as string));
//                 semanticStack.Push(value as string);
//                 //Console.WriteLine("Pushed SkolemID {0} onto the semantic stack as a string.", value);
//                 break;

//             case Symbol.T_Variable:
//                 semanticStack.Push(new Variable(value as string));
//                 //Console.WriteLine("Pushed Variable {0} onto the semantic stack.", value);
//                 break;

//             default:
//                 break;
//         }
//     }
// }
