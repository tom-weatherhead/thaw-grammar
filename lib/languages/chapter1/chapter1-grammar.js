// tom-weatherhead/thaw-grammar/src/languages/chapter1/chapter1-grammar.ts
'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
exports.Chapter1Grammar = void 0;
const thaw_lexical_analyzer_1 = require("thaw-lexical-analyzer");
const expression_list_1 = require("../../common/domain-object-model/expression-list");
const name_1 = require("../../common/domain-object-model/name");
const variable_1 = require("../../common/domain-object-model/variable");
const variable_list_1 = require("../../common/domain-object-model/variable-list");
const begin_usage_1 = require("../../common/domain-object-model/begin-usage");
const function_definition_1 = require("../../common/domain-object-model/function-definition");
const if_usage_1 = require("../../common/domain-object-model/if-usage");
const set_usage_1 = require("../../common/domain-object-model/set-usage");
const while_usage_1 = require("../../common/domain-object-model/while-usage");
const grammar_exception_1 = require("../../common/exceptions/grammar-exception");
const grammar_base_1 = require("../../common/grammar-base");
const parser_selectors_1 = require("../../common/parser-selectors");
const production_1 = require("../../common/production");
const symbol_1 = require("../../common/symbol");
const integer_literal_1 = require("./domain-object-model/integer-literal");
const operator_usage_1 = require("./domain-object-model/operator-usage");
class Chapter1Grammar extends grammar_base_1.GrammarBase {
    // The grammar from Chapter 1 of Kamin's book: "Programming Languages: An Interpreter-Based Approach" (?)
    constructor() {
        super(symbol_1.Symbol.nonterminalStart);
        this.terminals.push(symbol_1.Symbol.terminalLeftBracket);
        this.terminals.push(symbol_1.Symbol.terminalRightBracket);
        this.terminals.push(symbol_1.Symbol.terminalDefine);
        this.terminals.push(symbol_1.Symbol.terminalIf);
        this.terminals.push(symbol_1.Symbol.terminalWhile);
        this.terminals.push(symbol_1.Symbol.terminalSet);
        this.terminals.push(symbol_1.Symbol.terminalBegin);
        this.terminals.push(symbol_1.Symbol.terminalPlus);
        this.terminals.push(symbol_1.Symbol.terminalMinus);
        this.terminals.push(symbol_1.Symbol.terminalMultiply);
        this.terminals.push(symbol_1.Symbol.terminalDivide);
        this.terminals.push(symbol_1.Symbol.terminalEquals);
        this.terminals.push(symbol_1.Symbol.terminalLessThan);
        this.terminals.push(symbol_1.Symbol.terminalGreaterThan);
        this.terminals.push(symbol_1.Symbol.terminalPrint);
        this.terminals.push(symbol_1.Symbol.terminalID);
        this.terminals.push(symbol_1.Symbol.terminalIntegerLiteral);
        this.terminals.push(symbol_1.Symbol.terminalRandom);
        this.terminals.push(symbol_1.Symbol.terminalThrow);
        this.terminals.push(symbol_1.Symbol.terminalEOF);
        this.nonTerminals.push(symbol_1.Symbol.nonterminalStart);
        this.nonTerminals.push(symbol_1.Symbol.nonterminalInput);
        this.nonTerminals.push(symbol_1.Symbol.nonterminalExpression);
        this.nonTerminals.push(symbol_1.Symbol.nonterminalFunDef);
        this.nonTerminals.push(symbol_1.Symbol.nonterminalFunction);
        this.nonTerminals.push(symbol_1.Symbol.nonterminalArgList);
        this.nonTerminals.push(symbol_1.Symbol.nonterminalVariableList);
        this.nonTerminals.push(symbol_1.Symbol.nonterminalVariable);
        this.nonTerminals.push(symbol_1.Symbol.nonterminalValue);
        this.nonTerminals.push(symbol_1.Symbol.nonterminalBracketedExpression);
        this.nonTerminals.push(symbol_1.Symbol.nonterminalExpressionList);
        this.nonTerminals.push(symbol_1.Symbol.nonterminalOptr);
        this.nonTerminals.push(symbol_1.Symbol.nonterminalValueOp);
        this.nonTerminals.push(symbol_1.Symbol.nonterminalBracketedEntity);
        // From Chapter 1 of Kamin:
        // Input -> Expression
        // Input -> FunDef
        // FunDef -> ( define Function ArgList Expression )
        // ArgList -> ( VariableList )
        // VariableList -> Variable VariableList
        // VariableList -> Lambda
        // Expression -> Value
        // Expression -> Variable
        // Expression -> ( BracketedExpression )
        // BracketedExpression -> if Expression Expression Expression
        // BracketedExpression -> while Expression Expression
        // BracketedExpression -> set Variable Expression
        // BracketedExpression -> begin Expression ExpressionList
        // BracketedExpression -> Optr ExpressionList
        // ExpressionList -> Expression ExpressionList
        // ExpressionList -> Lambda
        // Optr -> Function
        // Optr -> Value-Op
        // Value -> Integer
        // Value-Op -> +
        // Value-Op -> -
        // Value-Op -> *
        // Value-Op -> /
        // Value-Op -> =
        // Value-Op -> <
        // Value-Op -> >
        // Value-Op -> print
        // Function -> Name
        // Variable -> Name
        // Integer -> ...
        // Name -> ...
        // We prevent function definitions from being considered as expressions.
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalStart, [symbol_1.Symbol.nonterminalInput, symbol_1.Symbol.terminalEOF], 1));
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalInput, [symbol_1.Symbol.nonterminalValue], 2));
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalInput, [
            symbol_1.Symbol.terminalLeftBracket,
            symbol_1.Symbol.nonterminalBracketedEntity,
            symbol_1.Symbol.terminalRightBracket
        ], 3));
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalBracketedEntity, [symbol_1.Symbol.nonterminalBracketedExpression], 4));
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalBracketedEntity, [symbol_1.Symbol.nonterminalFunDef], 5));
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalFunDef, [
            symbol_1.Symbol.terminalDefine,
            symbol_1.Symbol.nonterminalFunction,
            symbol_1.Symbol.nonterminalArgList,
            symbol_1.Symbol.nonterminalExpression,
            '#functionDefinition'
        ], 6));
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalArgList, [
            symbol_1.Symbol.terminalLeftBracket,
            symbol_1.Symbol.nonterminalVariableList,
            symbol_1.Symbol.terminalRightBracket
        ], 7));
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalVariableList, [
            symbol_1.Symbol.nonterminalVariable,
            symbol_1.Symbol.nonterminalVariableList,
            '#variableList'
        ], 8));
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalVariableList, [symbol_1.Symbol.Lambda, '#emptyVariableList'], 9));
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalExpression, [symbol_1.Symbol.nonterminalValue], 10));
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalExpression, [symbol_1.Symbol.nonterminalVariable], 11));
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalExpression, [
            symbol_1.Symbol.terminalLeftBracket,
            symbol_1.Symbol.nonterminalBracketedExpression,
            symbol_1.Symbol.terminalRightBracket
        ], 12));
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalBracketedExpression, [
            symbol_1.Symbol.terminalIf,
            symbol_1.Symbol.nonterminalExpression,
            symbol_1.Symbol.nonterminalExpression,
            symbol_1.Symbol.nonterminalExpression,
            '#if'
        ], 13));
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalBracketedExpression, [
            symbol_1.Symbol.terminalWhile,
            symbol_1.Symbol.nonterminalExpression,
            symbol_1.Symbol.nonterminalExpression,
            '#while'
        ], 14));
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalBracketedExpression, [
            symbol_1.Symbol.terminalSet,
            symbol_1.Symbol.nonterminalVariable,
            symbol_1.Symbol.nonterminalExpression,
            '#set'
        ], 15));
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalBracketedExpression, [
            symbol_1.Symbol.terminalBegin,
            symbol_1.Symbol.nonterminalExpression,
            symbol_1.Symbol.nonterminalExpressionList,
            '#begin'
        ], 16));
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalBracketedExpression, [
            symbol_1.Symbol.nonterminalOptr,
            symbol_1.Symbol.nonterminalExpressionList,
            '#operatorUsage'
        ], 17));
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalExpressionList, [
            symbol_1.Symbol.nonterminalExpression,
            symbol_1.Symbol.nonterminalExpressionList,
            '#expressionList'
        ], 18));
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalExpressionList, [symbol_1.Symbol.Lambda, '#emptyExpressionList'], 19));
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalOptr, [symbol_1.Symbol.nonterminalFunction], 20));
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalOptr, [symbol_1.Symbol.nonterminalValueOp], 21));
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalValue, [symbol_1.Symbol.terminalIntegerLiteral], 22));
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalValueOp, [symbol_1.Symbol.terminalPlus], 23));
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalValueOp, [symbol_1.Symbol.terminalMinus], 24));
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalValueOp, [symbol_1.Symbol.terminalMultiply], 25));
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalValueOp, [symbol_1.Symbol.terminalDivide], 26));
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalValueOp, [symbol_1.Symbol.terminalEquals], 27));
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalValueOp, [symbol_1.Symbol.terminalLessThan], 28));
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalValueOp, [symbol_1.Symbol.terminalGreaterThan], 29));
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalValueOp, [symbol_1.Symbol.terminalPrint], 30));
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalFunction, [symbol_1.Symbol.terminalID], 31));
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalVariable, [symbol_1.Symbol.terminalID, '#variable'], 32));
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalValueOp, [symbol_1.Symbol.terminalRandom], 33));
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalValueOp, [symbol_1.Symbol.terminalThrow], 34));
    }
    get languageName() {
        return 'Chapter 1';
    }
    get selectorsOfCompatibleParsers() {
        return [parser_selectors_1.ParserSelector.LL1];
    }
    executeSemanticAction(semanticStack, action) {
        // console.log(`Chapter1Grammar.executeSemanticAction() : action is ${typeof action} ${action}`);
        let name;
        let variable;
        let variableList;
        let expression;
        let expression2;
        let expression3;
        let expressionList;
        switch (action) {
            case '#functionDefinition':
                expression = semanticStack.pop(); // The function's body
                variableList = semanticStack.pop(); // The function's formal argument list
                name = semanticStack.pop(); // The function name
                semanticStack.push(new function_definition_1.FunctionDefinition(name, variableList, expression)); // Add line and column?
                break;
            case '#variableList':
                variableList = semanticStack.pop();
                variable = semanticStack.pop();
                variableList.value.unshift(variable);
                semanticStack.push(variableList);
                break;
            case '#emptyVariableList':
                semanticStack.push(new variable_list_1.VariableList()); // Add line and column?
                break;
            case '#if':
                expression3 = semanticStack.pop();
                expression2 = semanticStack.pop();
                expression = semanticStack.pop();
                semanticStack.push(new if_usage_1.IfUsage(expression, expression2, expression3)); // Add line and column?
                break;
            case '#while':
                expression2 = semanticStack.pop();
                expression = semanticStack.pop();
                semanticStack.push(new while_usage_1.WhileUsage(expression, expression2)); // Add line and column?
                break;
            case '#set':
                expression = semanticStack.pop();
                variable = semanticStack.pop();
                semanticStack.push(new set_usage_1.SetUsage(variable, expression)); // Add line and column?
                break;
            case '#begin':
                expressionList = semanticStack.pop();
                expression = semanticStack.pop();
                semanticStack.push(new begin_usage_1.BeginUsage(expression, expressionList)); // Add line and column?
                break;
            case '#operatorUsage':
                expressionList = semanticStack.pop();
                name = semanticStack.pop();
                semanticStack.push(new operator_usage_1.Chapter1OperatorUsage(name, expressionList)); // Add line and column?
                break;
            case '#expressionList':
                expressionList = semanticStack.pop();
                expression = semanticStack.pop();
                expressionList.value.unshift(expression);
                semanticStack.push(expressionList);
                break;
            case '#emptyExpressionList':
                semanticStack.push(new expression_list_1.ExpressionList());
                break;
            case '#variable':
                name = semanticStack.pop();
                semanticStack.push(new variable_1.Variable(name.value, name.line, name.column));
                break;
            default:
                throw new grammar_exception_1.GrammarException(`Unrecognized semantic action: ${action}`);
        }
    }
    tokenToSymbol(token) {
        // Returns Symbol
        // const tokenValueAsString: string = token.tokenValue as string;
        switch (token.tokenType) {
            case thaw_lexical_analyzer_1.LexicalState.tokenEOF:
                return symbol_1.Symbol.terminalEOF;
            case thaw_lexical_analyzer_1.LexicalState.tokenIntLit:
                return symbol_1.Symbol.terminalIntegerLiteral;
            case thaw_lexical_analyzer_1.LexicalState.tokenFltLit:
                return symbol_1.Symbol.terminalFloatLiteral;
            case thaw_lexical_analyzer_1.LexicalState.tokenStrLit:
                return symbol_1.Symbol.terminalStringLiteral;
            case thaw_lexical_analyzer_1.LexicalState.tokenLeftBracket:
                return symbol_1.Symbol.terminalLeftBracket;
            case thaw_lexical_analyzer_1.LexicalState.tokenRightBracket:
                return symbol_1.Symbol.terminalRightBracket;
            case thaw_lexical_analyzer_1.LexicalState.tokenPlus:
                return symbol_1.Symbol.terminalPlus;
            case thaw_lexical_analyzer_1.LexicalState.tokenMinus:
                return symbol_1.Symbol.terminalMinus;
            case thaw_lexical_analyzer_1.LexicalState.tokenMult:
                return symbol_1.Symbol.terminalMultiply;
            case thaw_lexical_analyzer_1.LexicalState.tokenDiv:
                return symbol_1.Symbol.terminalDivide;
            case thaw_lexical_analyzer_1.LexicalState.tokenEqual:
                return symbol_1.Symbol.terminalEquals;
            case thaw_lexical_analyzer_1.LexicalState.tokenLess:
                return symbol_1.Symbol.terminalLessThan;
            case thaw_lexical_analyzer_1.LexicalState.tokenGreater:
                return symbol_1.Symbol.terminalGreaterThan;
            case thaw_lexical_analyzer_1.LexicalState.tokenIdent:
                switch (token.tokenValue) {
                    case 'define':
                        return symbol_1.Symbol.terminalDefine;
                    case 'if':
                        return symbol_1.Symbol.terminalIf;
                    case 'while':
                        return symbol_1.Symbol.terminalWhile;
                    case 'set':
                        return symbol_1.Symbol.terminalSet;
                    case 'begin':
                        return symbol_1.Symbol.terminalBegin;
                    case 'print':
                        return symbol_1.Symbol.terminalPrint;
                    // case 'random': return Symbol.terminalRandom;
                    // case 'throw': return Symbol.terminalThrow;
                    default:
                        return symbol_1.Symbol.terminalID;
                }
                break;
            default:
                break;
        }
        throw new grammar_exception_1.GrammarException(`No grammar symbol matches token ${token.tokenType} ${token.tokenValue}`, token.line, token.column);
    }
    pushTokenOntoSemanticStack(semanticStack, tokenAsSymbol, token) {
        const value = token.tokenValue;
        switch (tokenAsSymbol) {
            case symbol_1.Symbol.terminalID:
            case symbol_1.Symbol.terminalPrint:
            case symbol_1.Symbol.terminalPlus:
            case symbol_1.Symbol.terminalMinus:
            case symbol_1.Symbol.terminalMultiply:
            case symbol_1.Symbol.terminalDivide:
            case symbol_1.Symbol.terminalEquals:
            case symbol_1.Symbol.terminalLessThan:
            case symbol_1.Symbol.terminalGreaterThan:
            case symbol_1.Symbol.terminalRandom:
            case symbol_1.Symbol.terminalThrow:
                semanticStack.push(new name_1.Name(value, token.line, token.column));
                break;
            case symbol_1.Symbol.terminalIntegerLiteral:
                semanticStack.push(new integer_literal_1.IntegerLiteral(value, token.line, token.column));
                break;
            case symbol_1.Symbol.terminalLeftBracket:
            case symbol_1.Symbol.terminalRightBracket:
            case symbol_1.Symbol.terminalDefine:
            case symbol_1.Symbol.terminalIf:
            case symbol_1.Symbol.terminalWhile:
            case symbol_1.Symbol.terminalSet:
            case symbol_1.Symbol.terminalBegin:
            case symbol_1.Symbol.terminalPrint:
            case symbol_1.Symbol.terminalEOF:
                break;
            default:
                throw new grammar_exception_1.GrammarException(`pushTokenOntoSemanticStack() : Unexpected tokenAsSymbol ${symbol_1.Symbol[tokenAsSymbol]} ${tokenAsSymbol}`, token.line, token.column);
        }
    }
}
exports.Chapter1Grammar = Chapter1Grammar;
