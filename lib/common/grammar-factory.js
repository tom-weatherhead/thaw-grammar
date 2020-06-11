// tom-weatherhead/thaw-grammar/src/common/grammar-factory.ts
'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
exports.createGrammar = void 0;
const thaw_lexical_analyzer_1 = require("thaw-lexical-analyzer");
const apl_grammar_1 = require("../languages/apl/apl-grammar");
const chapter1_grammar_1 = require("../languages/chapter1/chapter1-grammar");
const clu_grammar_1 = require("../languages/clu/clu-grammar");
const inference_grammar_1 = require("../languages/inference/inference-grammar");
const json_grammar_1 = require("../languages/json/json-grammar");
const lisp_grammar_1 = require("../languages/lisp/lisp-grammar");
const micro_grammar_1 = require("../languages/micro/micro-grammar");
const minimal_language_grammar_1 = require("../languages/minimal/minimal-language-grammar");
const prolog_grammar_1 = require("../languages/prolog/prolog-grammar");
const sasl_grammar_1 = require("../languages/sasl/sasl-grammar");
const scheme_grammar_1 = require("../languages/scheme/scheme-grammar");
const smalltalk_grammar_1 = require("../languages/smalltalk/smalltalk-grammar");
const argument_exception_1 = require("./exceptions/argument-exception");
function createGrammar(ls) {
    switch (ls) {
        case thaw_lexical_analyzer_1.LanguageSelector.MinimalLanguage:
            return new minimal_language_grammar_1.MinimalLanguageGrammar();
        case thaw_lexical_analyzer_1.LanguageSelector.Micro:
            return new micro_grammar_1.MicroGrammar();
        case thaw_lexical_analyzer_1.LanguageSelector.Chapter1:
            return new chapter1_grammar_1.Chapter1Grammar();
        case thaw_lexical_analyzer_1.LanguageSelector.LISP:
            return new lisp_grammar_1.LISPGrammar();
        case thaw_lexical_analyzer_1.LanguageSelector.APL:
            return new apl_grammar_1.APLGrammar();
        case thaw_lexical_analyzer_1.LanguageSelector.Scheme:
            return new scheme_grammar_1.SchemeGrammar();
        case thaw_lexical_analyzer_1.LanguageSelector.SASL:
            return new sasl_grammar_1.SASLGrammar();
        case thaw_lexical_analyzer_1.LanguageSelector.CLU:
            return new clu_grammar_1.CluGrammar();
        case thaw_lexical_analyzer_1.LanguageSelector.Smalltalk:
            return new smalltalk_grammar_1.SmalltalkGrammar();
        case thaw_lexical_analyzer_1.LanguageSelector.Prolog:
            return new prolog_grammar_1.PrologGrammar();
        // LanguageSelector.Prolog2,    // "Real" Prolog.
        case thaw_lexical_analyzer_1.LanguageSelector.Inference:
            return new inference_grammar_1.InferenceGrammar();
        case thaw_lexical_analyzer_1.LanguageSelector.JSON:
            return new json_grammar_1.JSONGrammar();
        default:
            throw new argument_exception_1.ArgumentException(`createGrammar(): Unsupported language selector ${ls} ${thaw_lexical_analyzer_1.LanguageSelector[ls]}`, 'ls');
    }
}
exports.createGrammar = createGrammar;
