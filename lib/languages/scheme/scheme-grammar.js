// tom-weatherhead/thaw-grammar/src/languages/scheme/scheme-grammar.ts
// From the book:
// BracketedExpression -> Expression ExpressionList
// Value-Op -> primop?
// Value-Op -> closure?
// Value -> ( lambda ArgList Expression )
// Value -> Value-Op
// BracketedExpression -> LetKeyword ( VarExprList ) Expression
// LetKeyword -> let
// LetKeyword -> let*
// LetKeyword -> letrec
// VarExprList -> ( Variable Expression ) VarExprList
// VarExprList -> Lambda
// BracketedExpression -> call/cc Expression
'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
exports.SchemeGrammar = void 0;
const thaw_lexical_analyzer_1 = require("thaw-lexical-analyzer");
const expression_list_1 = require("../../common/domain-object-model/expression-list");
const name_1 = require("../../common/domain-object-model/name");
const variable_1 = require("../../common/domain-object-model/variable");
const variable_list_1 = require("../../common/domain-object-model/variable-list");
const begin_usage_1 = require("../../common/domain-object-model/begin-usage");
const cond_usage_1 = require("../../common/domain-object-model/cond-usage");
const if_usage_1 = require("../../common/domain-object-model/if-usage");
const let_star_usage_1 = require("../../common/domain-object-model/let-star-usage");
const let_usage_1 = require("../../common/domain-object-model/let-usage");
const set_usage_1 = require("../../common/domain-object-model/set-usage");
const while_usage_1 = require("../../common/domain-object-model/while-usage");
const argument_exception_1 = require("../../common/exceptions/argument-exception");
const grammar_exception_1 = require("../../common/exceptions/grammar-exception");
const grammar_base_1 = require("../../common/grammar-base");
const parser_selectors_1 = require("../../common/parser-selectors");
const production_1 = require("../../common/production");
const symbol_1 = require("../../common/symbol");
const integer_literal_1 = require("../lisp/domain-object-model/integer-literal");
const lisp_string_1 = require("../lisp/domain-object-model/lisp-string");
const lisp_symbol_1 = require("../lisp/domain-object-model/lisp-symbol");
const null_sexpression_1 = require("../lisp/domain-object-model/null-sexpression");
const quoted_constant_with_apostrophe_1 = require("../lisp/domain-object-model/quoted-constant-with-apostrophe");
const quoted_constant_with_quote_keyword_1 = require("../lisp/domain-object-model/quoted-constant-with-quote-keyword");
const sexpression_list_1 = require("../lisp/domain-object-model/sexpression-list");
const call_cc_usage_1 = require("./domain-object-model/call-cc-usage");
const evaluable_expression_1 = require("./domain-object-model/evaluable-expression");
const lambda_expression_1 = require("./domain-object-model/lambda-expression");
const let_rec_usage_1 = require("./domain-object-model/let-rec-usage");
const primitive_operator_1 = require("./domain-object-model/primitive-operator");
class SchemeGrammar extends grammar_base_1.GrammarBase {
    // The Scheme grammar from Kamin (the book 'Programming Languages: An Interpreter-Based Approach')
    constructor() {
        super(symbol_1.Symbol.nonterminalStart);
        // Start with the LISP grammar.
        // Then remove some stuff:
        // Terminals.Remove(Symbol.T_Define);
        // NonTerminals.Remove(Symbol.N_FunDef);
        // NonTerminals.Remove(Symbol.N_Optr);
        // RemoveProductionsContainingSymbol(Symbol.N_FunDef);
        // RemoveProductionsContainingSymbol(Symbol.N_Optr);
        // Then add the Scheme-specific stuff:
        // Terminals.UnionWith(new HashSet<Symbol>() {
        //     Symbol.T_PrimOpPred, Symbol.T_ClosurePred, Symbol.T_LambdaKeyword, Symbol.T_LetRec,
        //     Symbol.T_CallCC });
        // Productions.Add(new Production(Symbol.N_BracketedExpression, new List<object>() { Symbol.N_Expression, Symbol.N_ExpressionList, "#evaluableExpression" }, 63));
        // Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_PrimOpPred }, 64));
        // Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_ClosurePred }, 65));
        // Productions.Add(new Production(Symbol.N_Value, new List<object>() { Symbol.T_LeftBracket, Symbol.T_LambdaKeyword, Symbol.N_ArgList, Symbol.N_Expression, Symbol.T_RightBracket, "#lambdaExpression" }, 66));
        // Productions.Add(new Production(Symbol.N_Value, new List<object>() { Symbol.N_ValueOp, "#valueOp" }, 67));
        // Productions.Add(new Production(Symbol.N_LetKeyword, new List<object>() { Symbol.T_LetRec }, 68));
        // Productions.Add(new Production(Symbol.N_BracketedExpression, new List<object>() { Symbol.T_CallCC, Symbol.N_Expression, "#call/cc" }, 69));
        this.terminals.push(symbol_1.Symbol.terminalLeftBracket);
        this.terminals.push(symbol_1.Symbol.terminalRightBracket);
        // this.terminals.push(Symbol.terminalDefine); // Used for LISP; removed for Scheme.
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
        // this.terminals.push(Symbol.terminalFloatLiteral);
        this.terminals.push(symbol_1.Symbol.terminalCons);
        this.terminals.push(symbol_1.Symbol.terminalCar);
        this.terminals.push(symbol_1.Symbol.terminalCdr);
        this.terminals.push(symbol_1.Symbol.terminalNumberPred);
        this.terminals.push(symbol_1.Symbol.terminalSymbolPred);
        this.terminals.push(symbol_1.Symbol.terminalListPred);
        this.terminals.push(symbol_1.Symbol.terminalNullPred);
        this.terminals.push(symbol_1.Symbol.terminalApostrophe);
        this.terminals.push(symbol_1.Symbol.terminalDot);
        this.terminals.push(symbol_1.Symbol.terminalList);
        this.terminals.push(symbol_1.Symbol.terminalCond);
        this.terminals.push(symbol_1.Symbol.terminalStringPred);
        this.terminals.push(symbol_1.Symbol.terminalFloor);
        this.terminals.push(symbol_1.Symbol.terminalRandom);
        this.terminals.push(symbol_1.Symbol.terminalLet);
        this.terminals.push(symbol_1.Symbol.terminalLetStar);
        this.terminals.push(symbol_1.Symbol.terminalLetRec);
        this.terminals.push(symbol_1.Symbol.terminalQuoteKeyword);
        // this.terminals.push(Symbol.terminalRplaca);
        // this.terminals.push(Symbol.terminalRplacd);
        // this.terminals.push(Symbol.terminalDefineMacro);
        // this.terminals.push(Symbol.terminalStringLiteral);
        // this.terminals.push(Symbol.terminalToString);
        // this.terminals.push(Symbol.terminalListToString);
        // this.terminals.push(Symbol.terminalStringToList);
        // this.terminals.push(Symbol.terminalStringToSymbol);
        // this.terminals.push(Symbol.terminalStringLessThan);
        // this.terminals.push(Symbol.terminalPow);
        // this.terminals.push(Symbol.terminalExp);
        // this.terminals.push(Symbol.terminalLn);
        // this.terminals.push(Symbol.terminalSin);
        // this.terminals.push(Symbol.terminalCos);
        // this.terminals.push(Symbol.terminalTan);
        // this.terminals.push(Symbol.terminalAtan2);
        // this.terminals.push(Symbol.terminalThrow);
        this.terminals.push(symbol_1.Symbol.terminalPrimOpPred); // Added for Scheme.
        this.terminals.push(symbol_1.Symbol.terminalClosurePred); // Added for Scheme.
        this.terminals.push(symbol_1.Symbol.terminalLambdaKeyword); // Added for Scheme.
        this.terminals.push(symbol_1.Symbol.terminalLetRec); // Added for Scheme.
        this.terminals.push(symbol_1.Symbol.terminalCallCC); // Added for Scheme.
        this.terminals.push(symbol_1.Symbol.terminalEOF);
        this.nonTerminals.push(symbol_1.Symbol.nonterminalStart);
        this.nonTerminals.push(symbol_1.Symbol.nonterminalInput);
        this.nonTerminals.push(symbol_1.Symbol.nonterminalExpression);
        // this.nonTerminals.push(Symbol.nonterminalFunDef); // Used for LISP; removed for Scheme.
        this.nonTerminals.push(symbol_1.Symbol.nonterminalFunction);
        this.nonTerminals.push(symbol_1.Symbol.nonterminalArgList);
        this.nonTerminals.push(symbol_1.Symbol.nonterminalVariableList);
        this.nonTerminals.push(symbol_1.Symbol.nonterminalVariable);
        this.nonTerminals.push(symbol_1.Symbol.nonterminalValue);
        this.nonTerminals.push(symbol_1.Symbol.nonterminalBracketedExpression);
        this.nonTerminals.push(symbol_1.Symbol.nonterminalExpressionList);
        // this.nonTerminals.push(Symbol.nonterminalOptr); // Used for LISP; removed for Scheme.
        this.nonTerminals.push(symbol_1.Symbol.nonterminalValueOp);
        this.nonTerminals.push(symbol_1.Symbol.nonterminalQuotedConst);
        this.nonTerminals.push(symbol_1.Symbol.nonterminalSExpression);
        this.nonTerminals.push(symbol_1.Symbol.nonterminalSExpressionList);
        this.nonTerminals.push(symbol_1.Symbol.nonterminalSExpressionListTail);
        this.nonTerminals.push(symbol_1.Symbol.nonterminalSymbol);
        this.nonTerminals.push(symbol_1.Symbol.nonterminalExprPairList);
        // this.nonTerminals.push(Symbol.nonterminalMacroDef);
        this.nonTerminals.push(symbol_1.Symbol.nonterminalLetKeyword);
        this.nonTerminals.push(symbol_1.Symbol.nonterminalVarExprList);
        // this.nonTerminals.push(Symbol.nonterminalBracketedEntity);
        // this.nonTerminals.push(Symbol.nonterminalBracketedInput);
        // this.nonTerminals.push(Symbol.nonterminalUnbracketedInput);
        // This initial production needed to be added: Start -> Input EOF
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalStart, [symbol_1.Symbol.nonterminalInput, symbol_1.Symbol.terminalEOF], 1));
        /* Old: Uses nonterminalBracketedInput and nonterminalUnbracketedInput
        // Input -> ( BracketedInput )
        this.productions.push(new Production(Symbol.nonterminalInput,
            [
                Symbol.terminalLeftBracket,
                Symbol.nonterminalBracketedInput,
                Symbol.terminalRightBracket
            ], 2));

        // Input -> UnbracketedInput
        this.productions.push(new Production(Symbol.nonterminalInput,
            [
                Symbol.nonterminalUnbracketedInput
            ], 3));

        // BracketedInput -> BracketedExpression
        this.productions.push(new Production(Symbol.nonterminalBracketedInput,
            [
                Symbol.nonterminalBracketedExpression
            ], 4));

        // BracketedInput -> FunDef // Used for LISP; removed for Scheme.
        // this.productions.push(new Production(Symbol.nonterminalBracketedInput,
        // 	[
        // 		Symbol.nonterminalFunDef
        // 	], 5));

        // - UnbracketedInput -> Value
        this.productions.push(new Production(Symbol.nonterminalUnbracketedInput,
            [
                Symbol.nonterminalValue
            ], 6));

        // - UnbracketedInput -> Variable
        this.productions.push(new Production(Symbol.nonterminalUnbracketedInput,
            [
                Symbol.nonterminalVariable
            ], 7));
         */
        // BEGIN: New: Does not use nonterminalBracketedInput and nonterminalUnbracketedInput
        // Input -> Expression
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalInput, [symbol_1.Symbol.nonterminalExpression], 2));
        // Expression -> Value
        // this.productions.push(new Production(Symbol.nonterminalExpression,
        // 	[
        // 		Symbol.nonterminalValue
        // 	], 3));
        // Expression -> Variable
        // this.productions.push(new Production(Symbol.nonterminalExpression,
        // 	[
        // 		Symbol.nonterminalVariable
        // 	], 4));
        // Expression -> ( BracketedExpression )
        // this.productions.push(new Production(Symbol.nonterminalExpression,
        // 	[
        // 		Symbol.terminalLeftBracket,
        // 		Symbol.nonterminalBracketedExpression,
        // 		Symbol.terminalRightBracket
        // 	], 5));
        // END: New: Does not use nonterminalBracketedInput and nonterminalUnbracketedInput
        // FunDef -> define Function ArgList Expression // Used for LISP; removed for Scheme.
        // this.productions.push(new Production(Symbol.nonterminalFunDef,
        // 	[
        // 		Symbol.terminalDefine,
        // 		Symbol.nonterminalFunction,
        // 		Symbol.nonterminalArgList,
        // 		Symbol.nonterminalExpression,
        // 		'#functionDefinition'
        // 	], 8));
        // ArgList -> ( VariableList )
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalArgList, [
            symbol_1.Symbol.terminalLeftBracket,
            symbol_1.Symbol.nonterminalVariableList,
            symbol_1.Symbol.terminalRightBracket
        ], 9));
        // VariableList -> Variable VariableList
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalVariableList, [
            symbol_1.Symbol.nonterminalVariable,
            symbol_1.Symbol.nonterminalVariableList,
            '#variableList'
        ], 10));
        // VariableList -> Lambda
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalVariableList, [symbol_1.Symbol.Lambda, '#emptyVariableList'], 11));
        // Expression -> Value
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalExpression, [symbol_1.Symbol.nonterminalValue], 12));
        // Expression -> Variable
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalExpression, [symbol_1.Symbol.nonterminalVariable], 13));
        // Expression -> ( BracketedExpression )
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalExpression, [
            symbol_1.Symbol.terminalLeftBracket,
            symbol_1.Symbol.nonterminalBracketedExpression,
            symbol_1.Symbol.terminalRightBracket
        ], 14));
        // BracketedExpression -> if Expression Expression Expression
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalBracketedExpression, [
            symbol_1.Symbol.terminalIf,
            symbol_1.Symbol.nonterminalExpression,
            symbol_1.Symbol.nonterminalExpression,
            symbol_1.Symbol.nonterminalExpression,
            '#if'
        ], 15));
        // BracketedExpression -> while Expression Expression
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalBracketedExpression, [
            symbol_1.Symbol.terminalWhile,
            symbol_1.Symbol.nonterminalExpression,
            symbol_1.Symbol.nonterminalExpression,
            '#while'
        ], 16));
        // BracketedExpression -> set Variable Expression
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalBracketedExpression, [
            symbol_1.Symbol.terminalSet,
            symbol_1.Symbol.nonterminalVariable,
            symbol_1.Symbol.nonterminalExpression,
            '#set'
        ], 17));
        // BracketedExpression -> begin Expression ExpressionList
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalBracketedExpression, [
            symbol_1.Symbol.terminalBegin,
            symbol_1.Symbol.nonterminalExpression,
            symbol_1.Symbol.nonterminalExpressionList,
            '#begin'
        ], 18));
        // BracketedExpression -> Optr ExpressionList // Used for LISP; removed for Scheme.
        // this.productions.push(new Production(Symbol.nonterminalBracketedExpression,
        // 	[
        // 		Symbol.nonterminalOptr,
        // 		Symbol.nonterminalExpressionList,
        // 		'#operatorUsage'
        // 	], 19));
        // ExpressionList -> Expression ExpressionList
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalExpressionList, [
            symbol_1.Symbol.nonterminalExpression,
            symbol_1.Symbol.nonterminalExpressionList,
            '#expressionList'
        ], 20));
        // ExpressionList -> Lambda
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalExpressionList, [symbol_1.Symbol.Lambda, '#emptyExpressionList'], 21));
        // Optr -> Function // Used for LISP; removed for Scheme.
        // this.productions.push(new Production(Symbol.nonterminalOptr,
        // 	[
        // 		Symbol.nonterminalFunction
        // 	], 22));
        // Optr -> Value-Op // Used for LISP; removed for Scheme.
        // this.productions.push(new Production(Symbol.nonterminalOptr,
        // 	[
        // 		Symbol.nonterminalValueOp
        // 	], 23));
        // Value -> Integer
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalValue, [symbol_1.Symbol.terminalIntegerLiteral], 24));
        // Value-Op -> +
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalValueOp, [symbol_1.Symbol.terminalPlus], 25));
        // Value-Op -> -
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalValueOp, [symbol_1.Symbol.terminalMinus], 26));
        // Value-Op -> *
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalValueOp, [symbol_1.Symbol.terminalMultiply], 27));
        // Value-Op -> /
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalValueOp, [symbol_1.Symbol.terminalDivide], 28));
        // Value-Op -> =
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalValueOp, [symbol_1.Symbol.terminalEquals], 29));
        // Value-Op -> <
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalValueOp, [symbol_1.Symbol.terminalLessThan], 30));
        // Value-Op -> >
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalValueOp, [symbol_1.Symbol.terminalGreaterThan], 31));
        // Value-Op -> print
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalValueOp, [symbol_1.Symbol.terminalPrint], 32));
        // Function -> Name
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalFunction, [symbol_1.Symbol.terminalID], 33));
        // Variable -> Name
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalVariable, [symbol_1.Symbol.terminalID, '#variable'], 34));
        // Value -> Quoted-Const
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalValue, [symbol_1.Symbol.nonterminalQuotedConst], 35));
        // Value-Op -> cons
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalValueOp, [symbol_1.Symbol.terminalCons], 36));
        // Value-Op -> car
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalValueOp, [symbol_1.Symbol.terminalCar], 37));
        // Value-Op -> cdr
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalValueOp, [symbol_1.Symbol.terminalCdr], 38));
        // Value-Op -> number?
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalValueOp, [symbol_1.Symbol.terminalNumberPred], 39));
        // Value-Op -> symbol?
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalValueOp, [symbol_1.Symbol.terminalSymbolPred], 40));
        // Value-Op -> list?
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalValueOp, [symbol_1.Symbol.terminalListPred], 41));
        // Value-Op -> null?
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalValueOp, [symbol_1.Symbol.terminalNullPred], 42));
        // Quoted-Const -> ' S-Expression
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalQuotedConst, [
            symbol_1.Symbol.terminalApostrophe,
            symbol_1.Symbol.nonterminalSExpression,
            '#quotedConstantWithApostrophe'
        ], 43));
        // S-Expression -> Integer
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalSExpression, [symbol_1.Symbol.terminalIntegerLiteral], 44));
        // S-Expression -> Symbol
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalSExpression, [symbol_1.Symbol.nonterminalSymbol], 45));
        // S-Expression -> ( S-Expression-List )
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalSExpression, [
            symbol_1.Symbol.terminalLeftBracket,
            symbol_1.Symbol.nonterminalSExpressionList,
            symbol_1.Symbol.terminalRightBracket
        ], 46));
        // TODO: 2020-01-08: Replace:
        // S-Expression -> ( S-Expression-List )
        // ...with:
        // S-Expression -> ( Bracketed-S-Expression )
        // Bracketed-S-Expression -> S-Expression-List
        // Bracketed-S-Expression -> quote S-Expression #quotedConstantWithQuoteKeyword
        // Q: In what situations do we need to use the quote keyword?
        // **** BEGIN Old ****
        // S-Expression-List -> S-Expression S-Expression-List
        // this.productions.push(new Production(Symbol.nonterminalSExpressionList, [Symbol.nonterminalSExpression, Symbol.nonterminalSExpressionList, '#sExpressionList'], 51));
        // S-Expression-List -> S-Expression . S-Expression
        // this.productions.push(new Production(Symbol.nonterminalSExpressionList, [Symbol.nonterminalSExpression, Symbol.terminalDot, Symbol.nonterminalSExpression, '#sExpressionList'], 52));
        // **** END Old ****
        // **** BEGIN New ****
        // S-Expression-List -> S-Expression S-Expression-List-Tail
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalSExpressionList, [
            symbol_1.Symbol.nonterminalSExpression,
            symbol_1.Symbol.nonterminalSExpressionListTail,
            '#sExpressionList'
        ], 47));
        // S-Expression-List-Tail -> S-Expression-List
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalSExpressionListTail, [symbol_1.Symbol.nonterminalSExpressionList], 48));
        // S-Expression-List-Tail -> . S-Expression
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalSExpressionListTail, [symbol_1.Symbol.terminalDot, symbol_1.Symbol.nonterminalSExpression], 49));
        // **** END New ****
        // S-Expression-List -> Lambda
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalSExpressionList, [symbol_1.Symbol.Lambda, '#emptySExpressionList'], 50));
        // Symbol -> Name
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalSymbol, [symbol_1.Symbol.terminalID, '#symbol'], 51));
        // BracketedExpression -> cond ( Expression Expression ) ExprPairList
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalBracketedExpression, [
            symbol_1.Symbol.terminalCond,
            symbol_1.Symbol.terminalLeftBracket,
            symbol_1.Symbol.nonterminalExpression,
            symbol_1.Symbol.nonterminalExpression,
            symbol_1.Symbol.terminalRightBracket,
            symbol_1.Symbol.nonterminalExprPairList,
            '#condUsage'
        ], 52));
        // ExprPairList -> ( Expression Expression ) ExprPairList
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalExprPairList, [
            symbol_1.Symbol.terminalLeftBracket,
            symbol_1.Symbol.nonterminalExpression,
            symbol_1.Symbol.nonterminalExpression,
            symbol_1.Symbol.terminalRightBracket,
            symbol_1.Symbol.nonterminalExprPairList,
            '#exprPairList'
        ], 53));
        // ExprPairList -> Lambda
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalExprPairList, [symbol_1.Symbol.Lambda, '#emptyExprPairList'], 54));
        // Value-Op -> list
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalValueOp, [symbol_1.Symbol.terminalList], 55));
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalValueOp, [symbol_1.Symbol.terminalStringPred], 56));
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalValueOp, [symbol_1.Symbol.terminalFloor], 57));
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalValueOp, [symbol_1.Symbol.terminalRandom], 58));
        // this.productions.push(new Production(Symbol.nonterminalValue, [Symbol.terminalFloatLiteral], 60)); // Note: Number is out of order
        // Productions.Add(new Production(Symbol.N_BracketedExpression, new List<object>() {
        //     Symbol.N_LetKeyword,
        //     Symbol.T_LeftBracket,
        //     Symbol.N_VarExprList,
        //     Symbol.T_RightBracket,
        //     Symbol.N_Expression, "#letUsage" }, 34));
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalBracketedExpression, [
            symbol_1.Symbol.nonterminalLetKeyword,
            symbol_1.Symbol.terminalLeftBracket,
            symbol_1.Symbol.nonterminalVarExprList,
            symbol_1.Symbol.terminalRightBracket,
            symbol_1.Symbol.nonterminalExpression,
            '#letUsage'
        ], 59));
        // Productions.Add(new Production(Symbol.N_LetKeyword, new List<object>() { Symbol.T_Let }, 35));
        // Productions.Add(new Production(Symbol.N_LetKeyword, new List<object>() { Symbol.T_LetStar }, 36));
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalLetKeyword, [symbol_1.Symbol.terminalLet], 60));
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalLetKeyword, [symbol_1.Symbol.terminalLetStar], 61));
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalLetKeyword, [symbol_1.Symbol.terminalLetRec], 62));
        // Productions.Add(new Production(Symbol.N_VarExprList, new List<object>() {
        //     Symbol.T_LeftBracket,
        //     Symbol.N_Variable,
        //     Symbol.N_Expression,
        //     Symbol.T_RightBracket,
        //     Symbol.N_VarExprList, "#varExprList" }, 37));
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalVarExprList, [
            symbol_1.Symbol.terminalLeftBracket,
            symbol_1.Symbol.nonterminalVariable,
            symbol_1.Symbol.nonterminalExpression,
            symbol_1.Symbol.terminalRightBracket,
            symbol_1.Symbol.nonterminalVarExprList,
            '#varExprList'
        ], 63));
        // Productions.Add(new Production(Symbol.N_VarExprList, new List<object>() { Symbol.Lambda, "#emptyVarExprList" }, 38));
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalVarExprList, [symbol_1.Symbol.Lambda, '#emptyVarExprList'], 64));
        // this.productions.push(new Production(
        // 	Symbol.nonterminalSExpression, // TODO? : Create Symbol.nonterminalBracketedSExpression ?
        // 	[
        // 		Symbol.terminalLeftBracket,
        // 		Symbol.terminalQuoteKeyword,
        // 		Symbol.nonterminalSExpression,
        // 		Symbol.terminalRightBracket,
        // 		'#quotedConstantWithQuoteKeyword'
        // 	], n));
        // Quoted-Const -> ' S-Expression
        // this.productions.push(new Production(Symbol.nonterminalQuotedConst,
        // 	[
        // 		Symbol.terminalLeftBracket,
        // 		Symbol.terminalQuoteKeyword,
        // 		Symbol.nonterminalSExpression,
        // 		Symbol.terminalRightBracket,
        // 		'#quotedConstantWithQuoteKeyword'
        // 	], 65));
        // BracketedExpression -> begin Expression ExpressionList
        // this.productions.push(new Production(Symbol.nonterminalBracketedExpression,
        // this.productions.push(new Production(Symbol.nonterminalSExpressionList, // Real HACK.
        // 	[
        // 		Symbol.terminalQuoteKeyword,
        // 		Symbol.nonterminalSExpression,
        // 		'#quotedConstantWithQuoteKeyword'
        // 	], 65));
        // **** BEGIN : Ignore for now ****
        // Productions.Add(new Production(Symbol.N_Value, new List<object>() { Symbol.T_StringLiteral }, 75)); // Note: Number is out of order
        // Productions.Add(new Production(Symbol.N_Value, new List<object>() { Symbol.T_FloatLiteral }, 79)); // Note: Number is out of order
        // this.productions.push(new Production(Symbol.nonterminalValue, [Symbol.terminalStringLiteral], 75)); // Note: Number is out of order
        // this.productions.push(new Production(Symbol.nonterminalValueOp, [Symbol.terminalThrow], 34));
        // this.productions.push(new Production(Symbol.nonterminalValueOp, [Symbol.terminalRplaca], 56));
        // this.productions.push(new Production(Symbol.nonterminalValueOp, [Symbol.terminalRplacd], 57));
        // this.productions.push(new Production(Symbol.nonterminalValueOp, [Symbol.terminalToString], 76));
        // this.productions.push(new Production(Symbol.nonterminalValueOp, [Symbol.terminalListToString], 77));
        // this.productions.push(new Production(Symbol.nonterminalValueOp, [Symbol.terminalStringToList], 78));
        // this.productions.push(new Production(Symbol.nonterminalValueOp, [Symbol.terminalStringToSymbol], 81));
        // this.productions.push(new Production(Symbol.nonterminalValueOp, [Symbol.terminalPow], 88));
        // this.productions.push(new Production(Symbol.nonterminalValueOp, [Symbol.terminalExp], 82));
        // this.productions.push(new Production(Symbol.nonterminalValueOp, [Symbol.terminalLn], 83));
        // this.productions.push(new Production(Symbol.nonterminalValueOp, [Symbol.terminalSin], 84));
        // this.productions.push(new Production(Symbol.nonterminalValueOp, [Symbol.terminalCos], 85));
        // this.productions.push(new Production(Symbol.nonterminalValueOp, [Symbol.terminalTan], 86));
        // this.productions.push(new Production(Symbol.nonterminalValueOp, [Symbol.terminalAtan2], 89));
        // this.productions.push(new Production(Symbol.nonterminalValueOp, [Symbol.terminalStringLessThan], 91));
        // **** END : Ignore for now ****
        // Old
        // this.productions.push(new Production(Symbol.nonterminalInput, [Symbol.nonterminalMacroDef], 58));
        // this.productions.push(new Production(
        // 	Symbol.nonterminalMacroDef,
        // 	[
        // 		Symbol.terminalLeftBracket,
        // 		Symbol.terminalDefineMacro,
        // 		Symbol.nonterminalFunction,
        // 		Symbol.nonterminalArgList,
        // 		Symbol.nonterminalExpression,
        // 		Symbol.terminalRightBracket,
        // 		'#macroDefinition'
        // 	],
        // 	59));
        // New
        // BracketedInput -> MacroDef
        // this.productions.push(new Production(Symbol.nonterminalBracketedInput,
        // 	[
        // 		Symbol.nonterminalMacroDef
        // 	], n));
        // this.productions.push(new Production(
        // 	Symbol.nonterminalMacroDef,
        // 	[
        // 		Symbol.terminalDefineMacro,
        // 		Symbol.nonterminalFunction,
        // 		Symbol.nonterminalArgList,
        // 		Symbol.nonterminalExpression,
        // 		'#macroDefinition'
        // 	], n));
        // Next production number is 56.
        // Added for Scheme:
        // BracketedExpression -> Expression ExpressionList
        // Productions.Add(new Production(Symbol.N_BracketedExpression, new List<object>() { Symbol.N_Expression, Symbol.N_ExpressionList, "#evaluableExpression" }, 63));
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalBracketedExpression, [
            symbol_1.Symbol.nonterminalExpression,
            symbol_1.Symbol.nonterminalExpressionList,
            '#evaluableExpression'
        ], 101));
        // Value-Op -> primop?
        // Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_PrimOpPred }, 64));
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalValueOp, [symbol_1.Symbol.terminalPrimOpPred], 102));
        // Value-Op -> closure?
        // Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_ClosurePred }, 65));
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalValueOp, [symbol_1.Symbol.terminalClosurePred], 103));
        // Value -> ( lambda ArgList Expression )
        // Productions.Add(new Production(Symbol.N_Value, new List<object>() { Symbol.T_LeftBracket, Symbol.T_LambdaKeyword, Symbol.N_ArgList, Symbol.N_Expression, Symbol.T_RightBracket, "#lambdaExpression" }, 66));
        // this.productions.push(new Production(Symbol.nonterminalValue,
        // 	[
        // 		Symbol.terminalLeftBracket,
        // 		Symbol.terminalLambdaKeyword,
        // 		Symbol.nonterminalArgList,
        // 		Symbol.nonterminalExpression,
        // 		Symbol.terminalRightBracket,
        // 		'#lambdaExpression'
        // 	], 104));
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalBracketedExpression, [
            // Symbol.terminalLeftBracket,
            symbol_1.Symbol.terminalLambdaKeyword,
            symbol_1.Symbol.nonterminalArgList,
            symbol_1.Symbol.nonterminalExpression,
            // Symbol.terminalRightBracket,
            '#lambdaExpression'
        ], 104));
        // Value -> Value-Op
        // Productions.Add(new Production(Symbol.N_Value, new List<object>() { Symbol.N_ValueOp, "#valueOp" }, 67));
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalValue, [symbol_1.Symbol.nonterminalValueOp, '#valueOp'], 105));
        // BracketedExpression -> LetKeyword ( VarExprList ) Expression
        // LetKeyword -> let
        // LetKeyword -> let*
        // LetKeyword -> letrec
        // VarExprList -> ( Variable Expression ) VarExprList
        // VarExprList -> Lambda
        // Productions.Add(new Production(Symbol.N_LetKeyword, new List<object>() { Symbol.T_LetRec }, 68));
        // this.productions.push(new Production(Symbol.nonterminalLetKeyword,
        // 	[
        // 		Symbol.terminalLetRec
        // 	], 106));
        // BracketedExpression -> call/cc Expression
        // Productions.Add(new Production(Symbol.N_BracketedExpression, new List<object>() { Symbol.T_CallCC, Symbol.N_Expression, "#call/cc" }, 69));
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalBracketedExpression, [
            symbol_1.Symbol.terminalCallCC,
            symbol_1.Symbol.nonterminalExpression,
            '#call/cc'
        ], 107));
    }
    get languageName() {
        return 'Scheme';
    }
    get selectorsOfCompatibleParsers() {
        return [parser_selectors_1.ParserSelector.LL1];
    }
    executeSemanticAction(semanticStack, action) {
        // console.log(`SchemeGrammar.executeSemanticAction() : action is ${typeof action} ${action}`);
        let name;
        let variable;
        let variableList;
        let expression;
        let expression2;
        let expression3;
        let expressionList;
        let sexpression;
        let head;
        let tail;
        let varExprList;
        let exprPairList;
        switch (action) {
            // case '#functionDefinition':
            // 	expression = semanticStack.pop() as IExpression<ISExpression>; // The function's body
            // 	variableList = semanticStack.pop() as VariableList<ISExpression>; // The function's formal argument list
            // 	name = semanticStack.pop() as Name; // The function name
            // 	semanticStack.push(new FunctionDefinition<ISExpression>(name, variableList, expression)); // Add line and column?
            // 	break;
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
            // case '#operatorUsage':
            // 	expressionList = semanticStack.pop() as ExpressionList<ISExpression>;
            // 	name = semanticStack.pop() as Name;
            // 	semanticStack.push(new LISPOperatorUsage(name, expressionList));
            // 	break;
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
            case '#quotedConstantWithApostrophe':
                sexpression = semanticStack.pop();
                semanticStack.push(new quoted_constant_with_apostrophe_1.QuotedConstantWithApostrophe(sexpression));
                break;
            case '#quotedConstantWithQuoteKeyword':
                sexpression = semanticStack.pop();
                semanticStack.push(new quoted_constant_with_quote_keyword_1.QuotedConstantWithQuoteKeyword(sexpression));
                break;
            case '#sExpressionList':
                tail = semanticStack.pop();
                head = semanticStack.pop();
                semanticStack.push(new sexpression_list_1.SExpressionList(head, tail));
                break;
            case '#emptySExpressionList':
                semanticStack.push(new null_sexpression_1.NullSExpression());
                break;
            case '#symbol':
                name = semanticStack.pop();
                semanticStack.push(new lisp_symbol_1.LISPSymbol(name.value));
                break;
            case '#condUsage':
                exprPairList = semanticStack.pop();
                expression2 = semanticStack.pop();
                expression = semanticStack.pop();
                exprPairList.unshift([expression, expression2]);
                semanticStack.push(new cond_usage_1.CondUsage(exprPairList));
                break;
            case '#exprPairList':
                exprPairList = semanticStack.pop();
                expression2 = semanticStack.pop();
                expression = semanticStack.pop();
                exprPairList.unshift([expression, expression2]);
                semanticStack.push(exprPairList);
                break;
            case '#emptyExprPairList':
                semanticStack.push(new Array());
                break;
            case '#letUsage':
                expression = semanticStack.pop();
                varExprList = semanticStack.pop();
                name = semanticStack.pop();
                semanticStack.push(this.createLetUsage(name, varExprList, expression));
                break;
            case '#varExprList':
                varExprList = semanticStack.pop();
                expression = semanticStack.pop();
                variable = semanticStack.pop();
                varExprList.unshift([variable, expression]);
                semanticStack.push(varExprList);
                break;
            case '#emptyVarExprList':
                semanticStack.push(new Array());
                break;
            // case '#macroDefinition':
            // 	expression = semanticStack.pop() as IExpression<ISExpression>; // macroBody
            // 	variableList = semanticStack.pop() as VariableList<ISExpression>; // macroArgList
            // 	name = semanticStack.pop() as Name; // macroName
            // 	semanticStack.push(new MacroDefinition(name, variableList, expression));
            // 	break;
            case '#lambdaExpression':
                const body = semanticStack.pop();
                const argList = semanticStack.pop();
                semanticStack.push(new lambda_expression_1.LambdaExpression(argList, body));
                break;
            case '#evaluableExpression':
                expressionList = semanticStack.pop();
                expression = semanticStack.pop();
                semanticStack.push(new evaluable_expression_1.EvaluableExpression(expression, expressionList));
                break;
            case '#valueOp':
                name = semanticStack.pop();
                semanticStack.push(new primitive_operator_1.PrimOp(name));
                break;
            case '#call/cc':
                expression = semanticStack.pop();
                semanticStack.push(new call_cc_usage_1.CallCCUsage(expression));
                break;
            default:
                throw new grammar_exception_1.GrammarException(`Unrecognized semantic action: ${action}`);
        }
    }
    tokenToSymbol(token) {
        // Returns Symbol
        const tokenValueAsString = token.tokenValue;
        switch (token.tokenType) {
            case thaw_lexical_analyzer_1.LexicalState.tokenEOF:
                return symbol_1.Symbol.terminalEOF;
            case thaw_lexical_analyzer_1.LexicalState.tokenIntLit:
                return symbol_1.Symbol.terminalIntegerLiteral;
            // HACK version 2 : ThAW 2020-01-06 : The value in an IntegerLiteral can be int or float.
            // In the future, we will need to properly support FloatLiterals
            // and distinguish them from IntegerLiterals.
            // case LexicalState.tokenFltLit: return Symbol.terminalFloatLiteral;
            case thaw_lexical_analyzer_1.LexicalState.tokenFltLit:
                return symbol_1.Symbol.terminalIntegerLiteral;
            case thaw_lexical_analyzer_1.LexicalState.tokenStrLit:
                return symbol_1.Symbol.terminalStringLiteral;
            // case LexicalState.tokenIdent: return Symbol.terminalID;
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
            case thaw_lexical_analyzer_1.LexicalState.tokenApostrophe:
                return symbol_1.Symbol.terminalApostrophe;
            case thaw_lexical_analyzer_1.LexicalState.tokenQuoteKeyword:
                return symbol_1.Symbol.terminalQuoteKeyword;
            case thaw_lexical_analyzer_1.LexicalState.tokenIdent:
                switch (tokenValueAsString) {
                    // Quoting never changes tokens with these values into IDs.
                    case '.':
                        return symbol_1.Symbol.terminalDot; // We could modify the tokenizer to generate TokenType.T_Dot in this case, to obviate this line.
                    // case 'quote': return Symbol.terminalQuoteKeyword;
                    default:
                        break;
                }
                if (token.isQuoted) {
                    return symbol_1.Symbol.terminalID;
                }
                switch (tokenValueAsString) {
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
                    case 'cons':
                        return symbol_1.Symbol.terminalCons;
                    case 'car':
                        return symbol_1.Symbol.terminalCar;
                    case 'cdr':
                        return symbol_1.Symbol.terminalCdr;
                    case 'number?':
                        return symbol_1.Symbol.terminalNumberPred;
                    case 'symbol?':
                        return symbol_1.Symbol.terminalSymbolPred;
                    case 'list?':
                        return symbol_1.Symbol.terminalListPred;
                    case 'null?':
                        return symbol_1.Symbol.terminalNullPred;
                    case 'primop?':
                        return symbol_1.Symbol.terminalPrimOpPred; // Added for Scheme
                    case 'closure?':
                        return symbol_1.Symbol.terminalClosurePred; // Added for Scheme
                    case 'string?':
                        return symbol_1.Symbol.terminalStringPred;
                    case 'list':
                        return symbol_1.Symbol.terminalList;
                    case 'rplaca':
                        return symbol_1.Symbol.terminalRplaca;
                    case 'rplacd':
                        return symbol_1.Symbol.terminalRplacd;
                    case 'define-macro':
                        return symbol_1.Symbol.terminalDefineMacro;
                    case 'random':
                        return symbol_1.Symbol.terminalRandom;
                    case 'tostring':
                        return symbol_1.Symbol.terminalToString;
                    case 'listtostring':
                        return symbol_1.Symbol.terminalListToString;
                    case 'stringtolist':
                        return symbol_1.Symbol.terminalStringToList;
                    case 'stringtosymbol':
                        return symbol_1.Symbol.terminalStringToSymbol;
                    case 'pow':
                        return symbol_1.Symbol.terminalPow;
                    case 'exp':
                        return symbol_1.Symbol.terminalExp;
                    case 'ln':
                        return symbol_1.Symbol.terminalLn;
                    case 'sin':
                        return symbol_1.Symbol.terminalSin;
                    case 'cos':
                        return symbol_1.Symbol.terminalCos;
                    case 'tan':
                        return symbol_1.Symbol.terminalTan;
                    case 'atan2':
                        return symbol_1.Symbol.terminalAtan2;
                    case 'floor':
                        return symbol_1.Symbol.terminalFloor;
                    case 'throw':
                        return symbol_1.Symbol.terminalThrow;
                    case 'string<':
                        return symbol_1.Symbol.terminalStringLessThan;
                    case 'cond':
                        return symbol_1.Symbol.terminalCond;
                    case 'let':
                        return symbol_1.Symbol.terminalLet;
                    case 'let*':
                        return symbol_1.Symbol.terminalLetStar;
                    case 'letrec':
                        return symbol_1.Symbol.terminalLetRec;
                    case 'lambda':
                        return symbol_1.Symbol.terminalLambdaKeyword; // Added for Scheme
                    case 'call/cc':
                        return symbol_1.Symbol.terminalCallCC;
                    default:
                        return symbol_1.Symbol.terminalID;
                }
                break;
            default:
                break;
        }
        throw new grammar_exception_1.GrammarException(`No grammar symbol matches token ${token.tokenType} ${thaw_lexical_analyzer_1.LexicalState[token.tokenType]} (value '${token.tokenValue}')`, token.line, token.column);
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
            case symbol_1.Symbol.terminalCons:
            case symbol_1.Symbol.terminalCar:
            case symbol_1.Symbol.terminalCdr:
            case symbol_1.Symbol.terminalNumberPred:
            case symbol_1.Symbol.terminalSymbolPred:
            case symbol_1.Symbol.terminalListPred:
            case symbol_1.Symbol.terminalNullPred:
            case symbol_1.Symbol.terminalPrimOpPred: // Added for Scheme
            case symbol_1.Symbol.terminalClosurePred: // Added for Scheme
            case symbol_1.Symbol.terminalStringPred:
            case symbol_1.Symbol.terminalList:
            case symbol_1.Symbol.terminalRplaca:
            case symbol_1.Symbol.terminalRplacd:
            case symbol_1.Symbol.terminalRandom:
            case symbol_1.Symbol.terminalToString:
            case symbol_1.Symbol.terminalListToString:
            case symbol_1.Symbol.terminalStringToList:
            case symbol_1.Symbol.terminalStringToSymbol:
            case symbol_1.Symbol.terminalPow:
            case symbol_1.Symbol.terminalExp:
            case symbol_1.Symbol.terminalLn:
            case symbol_1.Symbol.terminalSin:
            case symbol_1.Symbol.terminalCos:
            case symbol_1.Symbol.terminalTan:
            case symbol_1.Symbol.terminalAtan2:
            case symbol_1.Symbol.terminalFloor:
            case symbol_1.Symbol.terminalThrow:
            case symbol_1.Symbol.terminalStringLessThan:
            case symbol_1.Symbol.terminalLet:
            case symbol_1.Symbol.terminalLetStar:
            case symbol_1.Symbol.terminalLetRec:
                semanticStack.push(new name_1.Name(value, token.line, token.column));
                break;
            case symbol_1.Symbol.terminalIntegerLiteral:
                semanticStack.push(new integer_literal_1.IntegerLiteral(value));
                break;
            // case Symbol.terminalFloatLiteral:
            // 	// semanticStack.push(new FloatLiteral(value));
            // 	// HACK ThAW 2020-01-06 : The value in an IntegerLiteral can be int or float.
            // 	// In the future, we will need to properly support FloatLiterals
            // 	// and distinguish them from IntegerLiterals.
            // 	semanticStack.push(new IntegerLiteral(value));
            // 	break;
            case symbol_1.Symbol.terminalStringLiteral:
                semanticStack.push(new lisp_string_1.LISPString(value));
                break;
            case symbol_1.Symbol.terminalLeftBracket:
            case symbol_1.Symbol.terminalRightBracket:
            case symbol_1.Symbol.terminalApostrophe:
            case symbol_1.Symbol.terminalQuoteKeyword:
            case symbol_1.Symbol.terminalDefine:
            case symbol_1.Symbol.terminalIf:
            case symbol_1.Symbol.terminalWhile:
            case symbol_1.Symbol.terminalSet:
            case symbol_1.Symbol.terminalBegin:
            case symbol_1.Symbol.terminalPrint:
            case symbol_1.Symbol.terminalCond:
            case symbol_1.Symbol.terminalLambdaKeyword: // Added for Scheme
            case symbol_1.Symbol.terminalCallCC: // Added for Scheme
            case symbol_1.Symbol.terminalEOF:
                break;
            default:
                throw new grammar_exception_1.GrammarException(`pushTokenOntoSemanticStack() : Unexpected tokenAsSymbol ${symbol_1.Symbol[tokenAsSymbol]} (${tokenAsSymbol})`, token.line, token.column);
        }
    }
    createLetUsage(letName, varExprList, expression) {
        switch (letName.value) {
            case 'let':
                return new let_usage_1.LetUsage(varExprList, expression);
            case 'let*':
                return new let_star_usage_1.LetStarUsage(varExprList, expression);
            case 'letrec':
                return new let_rec_usage_1.LetRecUsage(varExprList, expression);
            default:
                throw new argument_exception_1.ArgumentException(`SchemeGrammar.createLetUsage() : Unknown 'let' keyword '${letName.value}.`, 'letName', letName.line, letName.column);
        }
    }
}
exports.SchemeGrammar = SchemeGrammar;
