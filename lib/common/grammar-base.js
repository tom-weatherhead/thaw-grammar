// tom-weatherhead/thaw-grammar/src/common/grammar-base.ts
'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
exports.GrammarBase = void 0;
const grammar_exception_1 = require("./exceptions/grammar-exception");
const production_1 = require("./production");
const symbol_1 = require("./symbol");
// TODO? :
// export abstract class GrammarBase<T> implements IGrammar { ... }
// where T is the base value type for the language; i.e.
// T = number for MinimalLanguage and Chapter1
// T = ISExpression for LISP
// Then semanticStack: Stack<IExpression<T>>
class GrammarBase {
    constructor(startSymbol) {
        this.terminals = [];
        this.nonTerminals = [];
        this.productions = [];
        this.startSymbol = startSymbol;
    }
    findStartingProduction() {
        const results = [];
        this.productions.forEach((p) => {
            if (p.lhs === this.startSymbol) {
                const p2 = p.StripOutSemanticActions();
                if (p2.rhs.length > 0) {
                    const lastObject = p2.rhs[p2.rhs.length - 1];
                    if (lastObject === symbol_1.Symbol.terminalEOF) {
                        results.push(p2);
                    }
                }
            }
        });
        if (results.length !== 1) {
            throw new grammar_exception_1.GrammarException(`GrammarBase.FindStartingProduction() : Expected one starting production; found ${results.length}.`);
        }
        return results[0];
    }
    // public removeProductionsContainingSymbol(symbol: number): void {
    // 	this.productions = this.productions.filter((production: Production) => !production.ContainsSymbol(symbol));
    // }
    addProduction(lhs, rhs, n = 0) {
        this.productions.push(new production_1.Production(lhs, rhs, n));
    }
}
exports.GrammarBase = GrammarBase;
