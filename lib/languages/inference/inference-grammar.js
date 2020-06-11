// tom-weatherhead/thaw-grammar/src/languages/inference/inference-grammar.ts
'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
exports.InferenceGrammar = void 0;
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
// export function dummyInferenceGrammar() : string {
// 	return 'dummyInferenceGrammar';
// }
class InferenceGrammar extends grammar_base_1.GrammarBase {
    constructor() {
        super(symbol_1.Symbol.nonterminalStart);
    }
    get languageName() {
        return 'Inference';
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
exports.InferenceGrammar = InferenceGrammar;
