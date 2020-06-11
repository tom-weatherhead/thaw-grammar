// tom-weatherhead/thaw-grammar/src/languages/clu/clu-grammar.ts
'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
exports.CluGrammar = void 0;
// import { ExpressionList }  from '../../common/domain-object-model/expression-list';
// import { IExpression }  from '../../common/domain-object-model/iexpression';
// import { Name }  from '../../common/domain-object-model/name';
// import { Variable }  from '../../common/domain-object-model/variable';
// import { VariableList }  from '../../common/domain-object-model/variable-list';
// import { BeginUsage }  from '../../common/domain-object-model/begin-usage';
// import { CondUsage }  from '../../common/domain-object-model/cond-usage';
// import { FunctionDefinition }  from '../../common/domain-object-model/function-definition';
// import { IfUsage }  from '../../common/domain-object-model/if-usage';
// import { LetStarUsage }  from '../../common/domain-object-model/let-star-usage';
// import { LetUsage }  from '../../common/domain-object-model/let-usage';
// import { OperatorUsage }  from '../../common/domain-object-model/operator-usage';
// import { SetUsage }  from '../../common/domain-object-model/set-usage';
// import { WhileUsage }  from '../../common/domain-object-model/while-usage';
// import { ArgumentException } from '../../common/exceptions/argument-exception';
// import { GrammarException } from '../../common/exceptions/grammar-exception';
const grammar_base_1 = require("../../common/grammar-base");
const parser_selectors_1 = require("../../common/parser-selectors");
// import { Production }  from '../../common/production';
const symbol_1 = require("../../common/symbol");
class CluGrammar extends grammar_base_1.GrammarBase {
    // The Scheme grammar from Kamin (the book 'Programming Languages: An Interpreter-Based Approach')
    constructor() {
        super(symbol_1.Symbol.nonterminalStart);
        // ...
    }
    get languageName() {
        return 'Clu';
    }
    get selectorsOfCompatibleParsers() {
        return [parser_selectors_1.ParserSelector.LL1];
    }
    executeSemanticAction(semanticStack, action) { }
    tokenToSymbol(token) {
        // Returns Symbol
        return symbol_1.Symbol.UndefinedSymbol;
    }
    pushTokenOntoSemanticStack(semanticStack, tokenAsSymbol, token) { }
}
exports.CluGrammar = CluGrammar;
