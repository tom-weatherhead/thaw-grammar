// tom-weatherhead/thaw-grammar/src/languages/minimal/minimal-language-grammar.ts
// A minimal grammar that supports the input: (+ 2 3)
// I.e. Tokens: LeftBracket, Plus, IntegerLiteral_2, IntegerLiteral_3, RightBracket, EOF.
'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
exports.MinimalLanguageGrammar = void 0;
const thaw_lexical_analyzer_1 = require("thaw-lexical-analyzer");
const expression_list_1 = require("../../common/domain-object-model/expression-list");
const name_1 = require("../../common/domain-object-model/name");
const grammar_exception_1 = require("../../common/exceptions/grammar-exception");
const grammar_base_1 = require("../../common/grammar-base");
const parser_selectors_1 = require("../../common/parser-selectors");
const production_1 = require("../../common/production");
const symbol_1 = require("../../common/symbol");
const integer_literal_1 = require("./domain-object-model/integer-literal");
const operator_usage_1 = require("./domain-object-model/operator-usage");
class MinimalLanguageGrammar extends grammar_base_1.GrammarBase {
    constructor() {
        super(symbol_1.Symbol.nonterminalStart);
        this.terminals.push(symbol_1.Symbol.terminalLeftBracket);
        this.terminals.push(symbol_1.Symbol.terminalRightBracket);
        this.terminals.push(symbol_1.Symbol.terminalPlus);
        this.terminals.push(symbol_1.Symbol.terminalIntegerLiteral);
        this.terminals.push(symbol_1.Symbol.terminalEOF);
        this.nonTerminals.push(symbol_1.Symbol.nonterminalStart);
        this.nonTerminals.push(symbol_1.Symbol.nonterminalExpression);
        this.nonTerminals.push(symbol_1.Symbol.nonterminalBracketedExpression);
        this.nonTerminals.push(symbol_1.Symbol.nonterminalExpressionList);
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalStart, [symbol_1.Symbol.nonterminalExpression, symbol_1.Symbol.terminalEOF], 1));
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalExpression, [symbol_1.Symbol.terminalIntegerLiteral], 2));
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalExpression, [
            symbol_1.Symbol.terminalLeftBracket,
            symbol_1.Symbol.nonterminalBracketedExpression,
            symbol_1.Symbol.terminalRightBracket
        ], 3));
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalBracketedExpression, [
            symbol_1.Symbol.terminalPlus,
            symbol_1.Symbol.nonterminalExpressionList,
            '#operatorUsage'
        ], 4));
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalExpressionList, [
            symbol_1.Symbol.nonterminalExpression,
            symbol_1.Symbol.nonterminalExpressionList,
            '#expressionList'
        ], 5));
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalExpressionList, [symbol_1.Symbol.Lambda, '#emptyExpressionList'], 6));
    }
    get languageName() {
        // This is a 'get' accessor.
        return 'The minimal language';
    }
    get selectorsOfCompatibleParsers() {
        return [parser_selectors_1.ParserSelector.LL1];
    }
    executeSemanticAction(semanticStack, action) {
        // console.log(`MinimalLanguageGrammar.executeSemanticAction() : action is ${typeof action} ${action}`);
        let name;
        let expression;
        let expressionList;
        switch (action) {
            case '#operatorUsage':
                expressionList = semanticStack.pop();
                name = semanticStack.pop();
                semanticStack.push(new operator_usage_1.OperatorUsage(name, expressionList));
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
            case thaw_lexical_analyzer_1.LexicalState.tokenLeftBracket:
                return symbol_1.Symbol.terminalLeftBracket;
            case thaw_lexical_analyzer_1.LexicalState.tokenRightBracket:
                return symbol_1.Symbol.terminalRightBracket;
            case thaw_lexical_analyzer_1.LexicalState.tokenPlus:
                return symbol_1.Symbol.terminalPlus;
            default:
                break;
        }
        throw new grammar_exception_1.GrammarException(`No grammar symbol matches token ${token.tokenType} ${token.tokenValue}`, token.line, token.column);
    }
    pushTokenOntoSemanticStack(semanticStack, tokenAsSymbol, token) {
        switch (tokenAsSymbol) {
            case symbol_1.Symbol.terminalIntegerLiteral:
                // console.log(`Pushing IntegerLiteral ${token.tokenValue as number} onto the semanticStack`);
                semanticStack.push(new integer_literal_1.IntegerLiteral(token.tokenValue));
                break;
            case symbol_1.Symbol.terminalPlus:
                // console.log(`Pushing Name '${token.tokenValue as string}' onto the semanticStack`);
                semanticStack.push(new name_1.Name(token.tokenValue, token.line, token.column /*, false */));
                break;
            case symbol_1.Symbol.terminalLeftBracket:
            case symbol_1.Symbol.terminalRightBracket:
            case symbol_1.Symbol.terminalEOF:
                break;
            default:
                throw new grammar_exception_1.GrammarException(`pushTokenOntoSemanticStack() : Unexpected tokenAsSymbol ${tokenAsSymbol}`, token.line, token.column);
        }
    }
}
exports.MinimalLanguageGrammar = MinimalLanguageGrammar;
