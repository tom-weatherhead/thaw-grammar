// tom-weatherhead/thaw-grammar/src/languages/lambda-calculus/lambda-calculus-grammar.ts

import { Stack } from 'thaw-common-utilities.ts';

import { LexicalState, Token } from 'thaw-lexical-analyzer';

// import { ExpressionList } from '../../common/domain-object-model/expression-list';
// import { IExpression } from '../../common/domain-object-model/iexpression';
import { Name } from '../../common/domain-object-model/name';
// import { Variable } from '../../common/domain-object-model/variable';
// import { VariableList } from '../../common/domain-object-model/variable-list';

// import { ArgumentException } from '../../common/exceptions/argument-exception';
import { GrammarException } from '../../common/exceptions/grammar-exception';

import { GrammarBase } from '../../common/grammar-base';
import { ParserSelector } from '../../common/parser-selectors';
import { Production } from '../../common/production';
import { Symbol } from '../../common/symbol';

// export interface ILCValue {}
// export class LCLambdaExpression {} // TODO: Name it 'LCLambdaExpression' or 'LCFunction' ?
// export class LCFunctionCall {}

// From https://opendsa.cs.vt.edu/ODSA/Books/PL/html/Syntax.html :
//
// 	A complete BNF grammar for the lambda calculus:
//
// 	< λexp > ::= < var >
// 		| λ < var > . < λexp >
// 		| ( < λexp > < λexp > )

export class LambdaCalculusGrammar extends GrammarBase {
	constructor() {
		super(Symbol.nonterminalStart);

		this.terminals.push(Symbol.terminalLeftBracket);
		this.terminals.push(Symbol.terminalRightBracket);
		this.terminals.push(Symbol.terminalID);
		this.terminals.push(Symbol.terminalFn); // === 'λ'
		this.terminals.push(Symbol.terminalDot);
		this.terminals.push(Symbol.terminalEOF);

		this.nonTerminals.push(Symbol.nonterminalStart);
		this.nonTerminals.push(Symbol.nonterminalInput);
		this.nonTerminals.push(Symbol.nonterminalExpression);
		this.nonTerminals.push(Symbol.nonterminalVariable);
		this.nonTerminals.push(Symbol.nonterminalLambdaExpression);
		this.nonTerminals.push(Symbol.nonterminalFunctionCall);

		// This initial production needed to be added: Start -> Input EOF
		this.productions.push(
			new Production(
				Symbol.nonterminalStart,
				[Symbol.nonterminalInput, Symbol.terminalEOF],
				1
			)
		);

		// Input -> Expression
		this.productions.push(
			new Production(Symbol.nonterminalInput, [Symbol.nonterminalExpression], 2)
		);

		// Expression -> Variable
		this.productions.push(
			new Production(Symbol.nonterminalExpression, [Symbol.nonterminalVariable], 3)
		);

		// Expression -> Lambda Expression
		this.productions.push(
			new Production(Symbol.nonterminalExpression, [Symbol.nonterminalLambdaExpression], 4)
		);

		// Expression -> Function Call
		this.productions.push(
			new Production(Symbol.nonterminalExpression, [Symbol.nonterminalFunctionCall], 5)
		);

		// Variable -> Name
		this.productions.push(
			new Production(Symbol.nonterminalVariable, [Symbol.terminalID, '#variable'], 6)
		);

		// Lambda Expression -> λ Variable . Expression
		this.productions.push(
			new Production(
				Symbol.nonterminalLambdaExpression,
				[
					// Symbol.terminalLeftBracket,
					Symbol.terminalFn,
					Symbol.nonterminalVariable,
					Symbol.terminalDot,
					Symbol.nonterminalExpression,
					// Symbol.terminalRightBracket,
					'#lambdaExpression'
				],
				7
			)
		);

		// Function Call -> ( Expression Expression )
		this.productions.push(
			new Production(
				Symbol.nonterminalFunctionCall,
				[
					Symbol.terminalLeftBracket,
					Symbol.nonterminalExpression,
					Symbol.nonterminalExpression,
					Symbol.terminalRightBracket,
					'#functionCall'
				],
				8
			)
		);
	}

	public get languageName(): string {
		return 'The Lambda Calculus';
	}

	public get selectorsOfCompatibleParsers(): number[] {
		return [ParserSelector.LL1];
	}

	// eslint-disable-next-line @typescript-eslint/no-explicit-any
	public executeSemanticAction(semanticStack: Stack<any>, action: string): void {
		// console.log(`LISPGrammar.executeSemanticAction() : action is ${typeof action} ${action}`);

		throw new Error('LambdaCalculusGrammar.executeSemanticAction() : Not yet implemented');

		// let name: Name;
		// let variable: Variable<ISExpression>;
		// let variableList: VariableList<ISExpression>;
		// let expression: IExpression<ISExpression>;
		// let expression2: IExpression<ISExpression>;
		// let expression3: IExpression<ISExpression>;
		// let expressionList: ExpressionList<ISExpression>;
		// let sexpression: ISExpression;
		// let head: ISExpression;
		// let tail: ISExpression;
		// let varExprList: [Variable<ISExpression>, IExpression<ISExpression>][];
		// let exprPairList: [IExpression<ISExpression>, IExpression<ISExpression>][];
		//
		// switch (action) {
		// 	case '#variable':
		// 		name = semanticStack.pop() as Name;
		// 		semanticStack.push(new Variable<ILCValue>(name.value, name.line, name.column));
		// 		break;
		//
		// 	case '#lambdaExpression':
		// 		expression = semanticStack.pop() as IExpression<ILCValue>; // The function's body
		// 		variable = semanticStack.pop() as Variable<ILCValue>; // The function's formal argument list
		// 		name = semanticStack.pop() as Name; // The function name
		// 		semanticStack.push(
		// 			// new LambdaExpression<ILCValue>([variable], expression)
		// 			new LCLambdaExpression(variable, expression)
		// 		); // Add line and column?
		// 		break;
		//
		// 	case  '#functionCall': // '#operatorUsage':
		// 		expressionList = semanticStack.pop() as ExpressionList<ISExpression>;
		// 		name = semanticStack.pop() as Name;
		// 		semanticStack.push(new LCFunctionCall(name, expressionList));
		// 		break;
		//
		// 	default:
		// 		throw new GrammarException(`Unrecognized semantic action: ${action}`);
		// }
	}

	public tokenToSymbol(token: Token): number {
		// Returns Symbol
		const tokenValueAsString: string = token.tokenValue as string;

		switch (token.tokenType) {
			case LexicalState.tokenEOF:
				return Symbol.terminalEOF;
			// case LexicalState.tokenStrLit:
			// 	return Symbol.terminalStringLiteral;
			// case LexicalState.tokenIdent: return Symbol.terminalID;
			case LexicalState.tokenLeftBracket:
				return Symbol.terminalLeftBracket;
			case LexicalState.tokenRightBracket:
				return Symbol.terminalRightBracket;

			case LexicalState.tokenIdent:
				switch (tokenValueAsString) {
					case '.':
						return Symbol.terminalDot; // We could modify the tokenizer to generate TokenType.T_Dot in this case, to obviate this line.
					case 'λ':
						return Symbol.terminalFn;
					default:
						return Symbol.terminalID;
				}

			default:
				break;
		}

		throw new GrammarException(
			`No grammar symbol matches token ${token.tokenType} ${
				LexicalState[token.tokenType]
			} (value '${token.tokenValue}')`,
			token.line,
			token.column
		);
	}

	public pushTokenOntoSemanticStack(
		// eslint-disable-next-line @typescript-eslint/no-explicit-any
		semanticStack: Stack<any>,
		tokenAsSymbol: number,
		token: Token
	): void {
		const value = token.tokenValue;

		switch (tokenAsSymbol) {
			case Symbol.terminalID:
				semanticStack.push(new Name(value as string, token.line, token.column));
				break;

			case Symbol.terminalLeftBracket:
			case Symbol.terminalRightBracket:
			case Symbol.terminalFn:
			case Symbol.terminalDot:
			case Symbol.terminalEOF:
				break;

			default:
				throw new GrammarException(
					`pushTokenOntoSemanticStack() : Unexpected tokenAsSymbol ${Symbol[tokenAsSymbol]} (${tokenAsSymbol})`,
					token.line,
					token.column
				);
		}
	}
}
