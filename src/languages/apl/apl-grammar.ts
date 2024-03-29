// tom-weatherhead/thaw-grammar/src/languages/apl/apl-grammar.ts

// Operators that have been implemented:

// Plus
// Minus
// Multiply
// Divide (truncating integer division)
// Max
// Or
// And
// PlusSlash
// MinusSlash
// MultiplySlash
// DivideSlash
// MaxSlash
// OrSlash
// AndSlash
// Shape
// Ravel
// Restruct
// Indx

// Operators TODO:

// Equals
// LessThan
// GreaterThan
// Print
// Compress
// Cat
// Trans
// SquareBrackets (subscripting)
// Assign
// DoubleSubscripting

// - APL symbols: From http://xahlee.info/comp/unicode_APL_symbols.html :
//
// ¯ × ÷ ∘ ∣ ∼ ≠ ≤ ≥ ≬ ⌶ ⋆ ⌾ ⍟ ⌽ ⍉ ⍝ ⍦ ⍧ ⍪ ⍫ ⍬ ⍭ ← ↑ → ↓ ∆ ∇ ∧ ∨ ∩ ∪ ⌈ ⌊ ⊤ ⊥ ⊂ ⊃ ⌿ ⍀ ⍅ ⍆ ⍏ ⍖ ⍊ ⍑ ⍋ ⍒ ⍎ ⍕ ⍱ ⍲ ○
// ⍳ ⍴ ⍵ ⍺
// ⍶ ⍷ ⍸ ⍹ ⍘ ⍙ ⍚ ⍛ ⍜ ⍮
// ¨ ⍡ ⍢ ⍣ ⍤ ⍥ ⍨ ⍩
// ⎕ ⍞ ⍠ ⍯ ⍰ ⍌ ⍍ ⍐ ⍓ ⍔ ⍗ ⌷ ⌸ ⌹ ⌺ ⌻ ⌼ ⍁ ⍂ ⍃ ⍄ ⍇ ⍈

import { GrammarSymbol, IToken, LexicalState, SemanticStackType } from 'thaw-interpreter-types';

import { ArgumentException, GrammarBase, GrammarException } from 'thaw-interpreter-core';

import { Name } from 'thaw-interpreter-core';

import { BeginUsage } from '../../common/domain-object-model/begin-usage';

import { CondUsage } from '../../common/domain-object-model/cond-usage';

// import { ExpressionList } from '../../common/domain-object-model/expression-list';

import { FunctionDefinition } from '../../common/domain-object-model/function-definition';

import { IExpression } from '../../common/domain-object-model/iexpression';

import { IfUsage } from '../../common/domain-object-model/if-usage';

import { LetUsage } from '../../common/domain-object-model/let-usage';

import { LetStarUsage } from '../../common/domain-object-model/let-star-usage';

import { SetUsage } from '../../common/domain-object-model/set-usage';

import { IVariable, Variable } from '../../common/domain-object-model/variable';

// import { VariableList } from '../../common/domain-object-model/variable-list';

import { WhileUsage } from '../../common/domain-object-model/while-usage';

import { IAPLExpression, IAPLValue } from './domain-object-model/interfaces/ivalue';

import { APLValue } from './domain-object-model/data-types/value';

// import { APLIfUsage } from './domain-object-model/if-usage';

// import { APLCondUsage } from './domain-object-model/cond-usage';

import { APLOperatorUsage } from './domain-object-model/operator-usage';

import { VectorAssignmentUsage } from './domain-object-model/vector-assignment-usage';

// import { APLWhileUsage } from './domain-object-model/while-usage';

export class APLGrammar extends GrammarBase {
	// The APL grammar from Kamin (the book 'Programming Languages: An Interpreter-Based Approach')

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

		this.addProduction(GrammarSymbol.nonterminalValueOp, [
			GrammarSymbol.terminalDoubleSubscripting
		]);

		this.addProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalRandom]);

		this.addProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalPow]);
		this.addProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalExp]);
		this.addProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalLn]);
		this.addProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalSin]);
		this.addProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalCos]);
		this.addProduction(GrammarSymbol.nonterminalValueOp, [GrammarSymbol.terminalTan]);

		// An empty vector is an int vector.

		this.addProduction(GrammarSymbol.nonterminalVectorConst, [
			GrammarSymbol.terminalApostrophe,
			GrammarSymbol.terminalLeftBracket,
			GrammarSymbol.nonterminalIntegerLiteralList,
			GrammarSymbol.terminalRightBracket,
			'#makeIntVector'
		]);

		// Productions.Add(new Production(Symbol.N_VectorConst, new List<object>() { Symbol.T_Apostrophe, Symbol.T_LeftBracket, Symbol.T_FloatLiteral, Symbol.N_FloatLiteralList, Symbol.T_RightBracket, '#makeFloatVector' }, 59));

		this.addProduction(GrammarSymbol.nonterminalIntegerLiteralList, [
			GrammarSymbol.terminalIntegerLiteral,
			GrammarSymbol.nonterminalIntegerLiteralList,
			'#intList'
		]);

		this.addProduction(GrammarSymbol.nonterminalIntegerLiteralList, [
			GrammarSymbol.Lambda,
			'#emptyIntList'
		]);

		// Productions.Add(new Production(Symbol.N_FloatLiteralList, new List<object>() { Symbol.T_FloatLiteral, Symbol.N_FloatLiteralList, '#floatList' }, 62));

		// Productions.Add(new Production(Symbol.N_FloatLiteralList, new List<object>() { Symbol.Lambda, '#emptyFloatList' }, 63));

		this.addProduction(GrammarSymbol.nonterminalBracketedExpression, [
			GrammarSymbol.terminalAssign,
			GrammarSymbol.nonterminalVariable,
			GrammarSymbol.nonterminalExpression,
			GrammarSymbol.nonterminalExpression,
			'#vecassign'
		]);

		// Productions.Add(new Production(Symbol.N_Value, new List<object>() { Symbol.T_FloatLiteral }, 66));
	}

	public get languageName(): string {
		return 'APL';
	}

	public executeSemanticAction(semanticStack: SemanticStackType, action: string): void {
		let name: Name;
		let functionName: Name;
		let letName: Name;
		let expression: IAPLExpression;
		let expression2: IAPLExpression;
		let expression3: IAPLExpression;
		let expressionList: IExpression<IAPLValue>[];
		let exprPairList: [IAPLExpression, IAPLExpression][];
		let intList: number[];
		let intScalar: IAPLValue;
		let variable: IVariable<IAPLValue>;
		let variableList: IVariable<IAPLValue>[];
		let argumentList: IVariable<IAPLValue>[];
		let body: IAPLExpression;
		let varExprList: [IVariable<IAPLValue>, IAPLExpression][];

		switch (action) {
			case '#operatorUsage':
				expressionList = semanticStack.pop() as IExpression<IAPLValue>[];
				name = semanticStack.pop() as Name;
				semanticStack.push(new APLOperatorUsage(name, expressionList));
				break;

			case '#expressionList':
				expressionList = semanticStack.pop() as IExpression<IAPLValue>[];
				expression = semanticStack.pop() as IAPLExpression;
				expressionList.unshift(expression);
				semanticStack.push(expressionList);
				break;

			case '#emptyExpressionList':
				semanticStack.push([] as IExpression<IAPLValue>[]);
				break;

			case '#makeIntVector':
				intList = semanticStack.pop() as number[];
				semanticStack.push(APLValue.createVector1(intList));
				break;

			case '#intList':
				intList = semanticStack.pop() as number[];
				intScalar = semanticStack.pop() as IAPLValue;
				intList.unshift(intScalar.getFirstScalar());
				semanticStack.push(intList);
				break;

			case '#emptyIntList':
				semanticStack.push([] as number[]);
				break;

			case '#functionDefinition':
				body = semanticStack.pop() as IAPLExpression;
				argumentList = semanticStack.pop() as IVariable<IAPLValue>[];
				functionName = semanticStack.pop() as Name;
				semanticStack.push(
					new FunctionDefinition<IAPLValue>(functionName, argumentList, body)
				);
				break;

			case '#variable':
				name = semanticStack.pop() as Name;
				semanticStack.push(new Variable<IAPLValue>(name.value, name.line, name.column));
				break;

			case '#variableList':
				variableList = semanticStack.pop() as IVariable<IAPLValue>[];
				variable = semanticStack.pop() as IVariable<IAPLValue>;
				variableList.unshift(variable);
				semanticStack.push(variableList);
				break;

			case '#emptyVariableList':
				semanticStack.push([] as IVariable<IAPLValue>[]);
				break;

			case '#vecassign':
				expression2 = semanticStack.pop() as IAPLExpression;
				expression = semanticStack.pop() as IAPLExpression;
				variable = semanticStack.pop() as IVariable<IAPLValue>;
				semanticStack.push(new VectorAssignmentUsage(variable, expression, expression2));
				break;

			case '#set':
				expression = semanticStack.pop() as IAPLExpression;
				variable = semanticStack.pop() as Variable<IAPLValue>;
				semanticStack.push(new SetUsage<IAPLValue>(variable, expression));
				break;

			case '#if':
				expression3 = semanticStack.pop() as IAPLExpression;
				expression2 = semanticStack.pop() as IAPLExpression;
				expression = semanticStack.pop() as IAPLExpression;
				// semanticStack.push(new APLIfUsage(expression, expression2, expression3));
				semanticStack.push(new IfUsage<IAPLValue>(expression, expression2, expression3));
				break;

			case '#while':
				expression2 = semanticStack.pop() as IAPLExpression;
				expression = semanticStack.pop() as IAPLExpression;
				// semanticStack.push(new APLWhileUsage(expression, expression2));
				semanticStack.push(new WhileUsage<IAPLValue>(expression, expression2));
				break;

			case '#begin':
				expressionList = semanticStack.pop() as IExpression<IAPLValue>[];
				expression = semanticStack.pop() as IAPLExpression;
				semanticStack.push(new BeginUsage<IAPLValue>(expression, expressionList));
				break;

			case '#condUsage':
				exprPairList = semanticStack.pop() as [IAPLExpression, IAPLExpression][];
				expression2 = semanticStack.pop() as IAPLExpression;
				expression = semanticStack.pop() as IAPLExpression;
				exprPairList.unshift([expression, expression2]);
				// semanticStack.push(new APLCondUsage(exprPairList));
				semanticStack.push(new CondUsage<IAPLValue>(exprPairList));
				break;

			case '#exprPairList':
				exprPairList = semanticStack.pop() as [IAPLExpression, IAPLExpression][];
				expression2 = semanticStack.pop() as IAPLExpression;
				expression = semanticStack.pop() as IAPLExpression;
				exprPairList.unshift([expression, expression2]);
				semanticStack.push(exprPairList);
				break;

			case '#emptyExprPairList':
				semanticStack.push([] as [IAPLExpression, IAPLExpression][]);
				break;

			case '#letUsage':
				expression = semanticStack.pop() as IAPLExpression;
				varExprList = semanticStack.pop() as [IVariable<IAPLValue>, IAPLExpression][];
				letName = semanticStack.pop() as Name;
				semanticStack.push(this.createLetUsage(letName, varExprList, expression));
				break;

			case '#varExprList':
				varExprList = semanticStack.pop() as [Variable<IAPLValue>, IAPLExpression][];
				expression = semanticStack.pop() as IAPLExpression;
				variable = semanticStack.pop() as Variable<IAPLValue>;
				varExprList.unshift([variable, expression]);
				semanticStack.push(varExprList);
				break;

			case '#emptyVarExprList':
				semanticStack.push([] as [Variable<IAPLValue>, IAPLExpression][]);
				break;

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

			default:
				throw new GrammarException(`APL: Unrecognized semantic action: ${action}`);
		}
	}

	public override tokenToSymbol(token: IToken): GrammarSymbol {
		if (token.tokenType === LexicalState.tokenAssign) {
			return GrammarSymbol.terminalAssign;
		} else if (token.tokenType === LexicalState.tokenIdent) {
			switch (token.tokenValue as string) {
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
				// case ':=':
				// 	return GrammarSymbol.terminalAssign;
				case '[;]':
					return GrammarSymbol.terminalDoubleSubscripting;
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
			case GrammarSymbol.terminalMax:
			case GrammarSymbol.terminalOr:
			case GrammarSymbol.terminalAnd:
			case GrammarSymbol.terminalPlusSlash:
			case GrammarSymbol.terminalMinusSlash:
			case GrammarSymbol.terminalMultiplySlash:
			case GrammarSymbol.terminalDivideSlash:
			case GrammarSymbol.terminalMaxSlash:
			case GrammarSymbol.terminalOrSlash:
			case GrammarSymbol.terminalAndSlash:
			case GrammarSymbol.terminalCompress:
			case GrammarSymbol.terminalShape:
			case GrammarSymbol.terminalRavel:
			case GrammarSymbol.terminalRestruct:
			case GrammarSymbol.terminalCat:
			case GrammarSymbol.terminalIndx:
			case GrammarSymbol.terminalTrans:
			case GrammarSymbol.terminalSquareBrackets:
			case GrammarSymbol.terminalDoubleSubscripting:
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

			case GrammarSymbol.terminalApostrophe:
			case GrammarSymbol.terminalAssign:
				// For these terminals, push nothing onto the semantic stack.
				break;

			default:
				super.pushTokenOntoSemanticStack(semanticStack, tokenAsSymbol, token);
		}
	}

	protected createLetUsage(
		letName: Name,
		varExprList: [IVariable<IAPLValue>, IAPLExpression][],
		expression: IAPLExpression
	): IAPLExpression {
		switch (letName.value) {
			case 'let':
				return new LetUsage<IAPLValue>(varExprList, expression);

			case 'let*':
				return new LetStarUsage<IAPLValue>(varExprList, expression);

			default:
				throw new ArgumentException(
					`APLGrammar.createLetUsage() : Unknown 'let' keyword '${letName.value}.`,
					'letName',
					letName.line,
					letName.column
				);
		}
	}
}
