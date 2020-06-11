// tom-weatherhead/thaw-grammar/src/languages/micro/micro-grammar.ts
'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
exports.MicroGrammar = void 0;
// import { ExpressionList }  from '../../common/domain-object-model/expression-list';
// import { IExpression }  from '../../common/domain-object-model/iexpression';
// import { Name }  from '../../common/domain-object-model/name';
// import { Variable }  from '../../common/domain-object-model/variable';
// import { VariableList }  from '../../common/domain-object-model/variable-list';
// import { ArgumentException } from '../../common/exceptions/argument-exception';
// import { GrammarException } from '../../common/exceptions/grammar-exception';
const grammar_base_1 = require("../../common/grammar-base");
const parser_selectors_1 = require("../../common/parser-selectors");
// import { Production }  from '../../common/production';
const symbol_1 = require("../../common/symbol");
// export function dummyMicroGrammar() : string {
// 	return 'dummyMicroGrammar';
// }
class MicroGrammar extends grammar_base_1.GrammarBase {
    constructor() {
        super(symbol_1.Symbol.nonterminalStart);
        // ...
    }
    get languageName() {
        return 'Micro';
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
exports.MicroGrammar = MicroGrammar;
