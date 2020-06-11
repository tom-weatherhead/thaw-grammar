// tom-weatherhead/thaw-grammar/src/languages/lisp/lisp-grammar.ts
// **** BEGIN : From the C# version in the Inference project ****
// Terminals.UnionWith(new HashSet<Symbol>() {
//     Symbol.T_LeftBracket, Symbol.T_RightBracket, Symbol.T_Define,
//     Symbol.T_If, Symbol.T_While, Symbol.T_Set, Symbol.T_Begin,
//     Symbol.T_Plus, Symbol.T_Minus, Symbol.T_Multiply, Symbol.T_Divide,
//     Symbol.T_Equals, Symbol.T_LessThan, //Symbol.T_GreaterThan,
//     Symbol.T_Print, Symbol.T_ID, Symbol.T_IntegerLiteral, Symbol.T_Cond,
//     Symbol.T_Let, Symbol.T_LetStar, Symbol.T_EOF });
// NonTerminals.UnionWith(new HashSet<Symbol>() { Symbol.N_Start,
//     Symbol.N_Input, Symbol.N_Expression, Symbol.N_FunDef, Symbol.N_Function,
//     Symbol.N_ArgList, Symbol.N_VariableList, Symbol.N_Variable, Symbol.N_Value,
//     Symbol.N_BracketedExpression, Symbol.N_ExpressionList, Symbol.N_Optr, Symbol.N_ValueOp,
//     Symbol.N_ExprPairList, Symbol.N_LetKeyword, Symbol.N_VarExprList });
// Productions.Add(new Production(Symbol.N_Start, new List<object>() { Symbol.N_Input, Symbol.T_EOF }, 1));
// Productions.Add(new Production(Symbol.N_Input, new List<object>() { Symbol.N_Expression }, 2));
// Productions.Add(new Production(Symbol.N_Input, new List<object>() { Symbol.N_FunDef }, 3));
// Productions.Add(new Production(Symbol.N_FunDef, new List<object>() { Symbol.T_LeftBracket, Symbol.T_Define, Symbol.N_Function, Symbol.N_ArgList, Symbol.N_Expression, Symbol.T_RightBracket, "#functionDefinition" }, 4));
// Productions.Add(new Production(Symbol.N_ArgList, new List<object>() { Symbol.T_LeftBracket, Symbol.N_VariableList, Symbol.T_RightBracket }, 5));
// Productions.Add(new Production(Symbol.N_VariableList, new List<object>() { Symbol.N_Variable, Symbol.N_VariableList, "#variableList" }, 6));
// Productions.Add(new Production(Symbol.N_VariableList, new List<object>() { Symbol.Lambda, "#emptyVariableList" }, 7));
// Productions.Add(new Production(Symbol.N_Expression, new List<object>() { Symbol.N_Value }, 8));
// Productions.Add(new Production(Symbol.N_Expression, new List<object>() { Symbol.N_Variable }, 9));
// Productions.Add(new Production(Symbol.N_Expression, new List<object>() { Symbol.T_LeftBracket, Symbol.N_BracketedExpression, Symbol.T_RightBracket }, 10));
// Productions.Add(new Production(Symbol.N_BracketedExpression, new List<object>() { Symbol.T_If, Symbol.N_Expression, Symbol.N_Expression, Symbol.N_Expression, "#if" }, 11));
// Productions.Add(new Production(Symbol.N_BracketedExpression, new List<object>() { Symbol.T_While, Symbol.N_Expression, Symbol.N_Expression, "#while" }, 12));
// Productions.Add(new Production(Symbol.N_BracketedExpression, new List<object>() { Symbol.T_Set, Symbol.N_Variable, Symbol.N_Expression, "#set" }, 13));
// Productions.Add(new Production(Symbol.N_BracketedExpression, new List<object>() { Symbol.T_Begin, Symbol.N_Expression, Symbol.N_ExpressionList, "#begin" }, 14));
// Productions.Add(new Production(Symbol.N_BracketedExpression, new List<object>() { Symbol.N_Optr, Symbol.N_ExpressionList, "#operatorUsage" }, 15));
// Productions.Add(new Production(Symbol.N_ExpressionList, new List<object>() { Symbol.N_Expression, Symbol.N_ExpressionList, "#expressionList" }, 16));
// Productions.Add(new Production(Symbol.N_ExpressionList, new List<object>() { Symbol.Lambda, "#emptyExpressionList" }, 17));
// Productions.Add(new Production(Symbol.N_Optr, new List<object>() { Symbol.N_Function }, 18));
// Productions.Add(new Production(Symbol.N_Optr, new List<object>() { Symbol.N_ValueOp }, 19));
// Productions.Add(new Production(Symbol.N_Value, new List<object>() { Symbol.T_IntegerLiteral }, 20));
// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Plus }, 21));
// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Minus }, 22));
// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Multiply }, 23));
// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Divide }, 24));
// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Equals }, 25));
// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_LessThan }, 26));
// //Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_GreaterThan }, 27));
// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Print }, 28));
// Productions.Add(new Production(Symbol.N_Function, new List<object>() { Symbol.T_ID }, 29));
// Productions.Add(new Production(Symbol.N_Variable, new List<object>() { Symbol.T_ID, "#variable" }, 30));
// Productions.Add(new Production(Symbol.N_BracketedExpression, new List<object>() {
//     Symbol.T_Cond,
//     Symbol.T_LeftBracket,
//     Symbol.N_Expression,
//     Symbol.N_Expression,
//     Symbol.T_RightBracket,
//     Symbol.N_ExprPairList, "#condUsage" }, 31));
// Productions.Add(new Production(Symbol.N_ExprPairList, new List<object>() {
//     Symbol.T_LeftBracket,
//     Symbol.N_Expression,
//     Symbol.N_Expression,
//     Symbol.T_RightBracket,
//     Symbol.N_ExprPairList, "#exprPairList" }, 32));
// Productions.Add(new Production(Symbol.N_ExprPairList, new List<object>() { Symbol.Lambda, "#emptyExprPairList" }, 33));
// Productions.Add(new Production(Symbol.N_BracketedExpression, new List<object>() {
//     Symbol.N_LetKeyword,
//     Symbol.T_LeftBracket,
//     Symbol.N_VarExprList,
//     Symbol.T_RightBracket,
//     Symbol.N_Expression, "#letUsage" }, 34));
// Productions.Add(new Production(Symbol.N_LetKeyword, new List<object>() { Symbol.T_Let }, 35));
// Productions.Add(new Production(Symbol.N_LetKeyword, new List<object>() { Symbol.T_LetStar }, 36));
// Productions.Add(new Production(Symbol.N_VarExprList, new List<object>() {
//     Symbol.T_LeftBracket,
//     Symbol.N_Variable,
//     Symbol.N_Expression,
//     Symbol.T_RightBracket,
//     Symbol.N_VarExprList, "#varExprList" }, 37));
// Productions.Add(new Production(Symbol.N_VarExprList, new List<object>() { Symbol.Lambda, "#emptyVarExprList" }, 38));
// From Chapter 2 of Kamin:
// Terminals.UnionWith(new HashSet<Symbol>() {
//     Symbol.T_Cons, Symbol.T_Car, Symbol.T_Cdr,
//     Symbol.T_NumberPred, Symbol.T_SymbolPred, Symbol.T_ListPred, Symbol.T_NullPred,
//     Symbol.T_Apostrophe, Symbol.T_Dot, Symbol.T_List,
//     Symbol.T_Rplaca, Symbol.T_Rplacd, Symbol.T_DefineMacro, Symbol.T_QuoteKeyword,
//     Symbol.T_Random, Symbol.T_StringLiteral, Symbol.T_StringPred,
//     Symbol.T_ToString, Symbol.T_ListToString, Symbol.T_StringToList, Symbol.T_StringToSymbol,
//     Symbol.T_FloatLiteral, Symbol.T_Pow, Symbol.T_Exp, Symbol.T_Ln, Symbol.T_Sin, Symbol.T_Cos, Symbol.T_Tan, Symbol.T_Atan2, Symbol.T_Floor,
//     Symbol.T_Throw, Symbol.T_StringLessThan });
// NonTerminals.UnionWith(new HashSet<Symbol>() {
//     Symbol.N_QuotedConst, Symbol.N_SExpression, Symbol.N_SExpressionList, Symbol.N_Symbol,
//     Symbol.N_MacroDef });
// Productions.Add(new Production(Symbol.N_Value, new List<object>() { Symbol.N_QuotedConst }, 39));
// Productions.Add(new Production(Symbol.N_Value, new List<object>() { Symbol.T_StringLiteral }, 75)); // Note: Number is out of order
// Productions.Add(new Production(Symbol.N_Value, new List<object>() { Symbol.T_FloatLiteral }, 79)); // Note: Number is out of order
// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Cons }, 40));
// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Car }, 41));
// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Cdr }, 42));
// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_NumberPred }, 43));
// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_SymbolPred }, 44));
// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_ListPred }, 45));
// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_NullPred }, 46));
// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_StringPred }, 73)); // Note: Number is out of order
// Productions.Add(new Production(Symbol.N_QuotedConst, new List<object>() { Symbol.T_Apostrophe, Symbol.N_SExpression, "#quotedConstantWithApostrophe" }, 47));
// Productions.Add(new Production(Symbol.N_SExpression, new List<object>() { Symbol.T_IntegerLiteral }, 48));
// Productions.Add(new Production(Symbol.N_SExpression, new List<object>() { Symbol.N_Symbol }, 49));
// Productions.Add(new Production(Symbol.N_SExpression, new List<object>() { Symbol.T_StringLiteral }, 74)); // Note: Number is out of order
// Productions.Add(new Production(Symbol.N_SExpression, new List<object>() { Symbol.T_FloatLiteral }, 80)); // Note: Number is out of order
// Productions.Add(new Production(Symbol.N_SExpression, new List<object>() { Symbol.T_LeftBracket, Symbol.N_SExpressionList, Symbol.T_RightBracket }, 50));
// Productions.Add(new Production(Symbol.N_SExpressionList, new List<object>() { Symbol.N_SExpression, Symbol.N_SExpressionList, "#sExpressionList" }, 51));
// Productions.Add(new Production(Symbol.N_SExpressionList, new List<object>() { Symbol.N_SExpression, Symbol.T_Dot, Symbol.N_SExpression, "#sExpressionList" }, 52));
// Productions.Add(new Production(Symbol.N_SExpressionList, new List<object>() { Symbol.Lambda, "#emptySExpressionList" }, 53));
// Productions.Add(new Production(Symbol.N_Symbol, new List<object>() { Symbol.T_ID, "#symbol" }, 54));
// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_List }, 55));
// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Rplaca }, 56));
// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Rplacd }, 57));
// Productions.Add(new Production(Symbol.N_Input, new List<object>() { Symbol.N_MacroDef }, 58));
// Productions.Add(new Production(Symbol.N_MacroDef, new List<object>() { Symbol.T_LeftBracket, Symbol.T_DefineMacro, Symbol.N_Function, Symbol.N_ArgList, Symbol.N_Expression, Symbol.T_RightBracket, "#macroDefinition" }, 59));
// Productions.Add(new Production(Symbol.N_SExpression, new List<object>() { Symbol.T_LeftBracket, Symbol.T_QuoteKeyword, Symbol.N_SExpression, Symbol.T_RightBracket, "#quotedConstantWithQuoteKeyword" }, 60));
// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Random }, 61));
// //Productions.Add(new Production(Symbol.N_QuotedConst, new List<object>() { Symbol.T_LeftBracket, Symbol.T_QuoteKeyword, Symbol.N_SExpression, Symbol.T_RightBracket, "#quotedConstantWithQuoteKeyword" }, 62)); // Needed for macros.
// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_ToString }, 76)); // Note: Number is out of order
// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_ListToString }, 77)); // Note: Number is out of order
// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_StringToList }, 78)); // Note: Number is out of order
// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_StringToSymbol }, 81)); // Note: Number is out of order
// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Pow }, 88)); // Note: Number is out of order
// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Exp }, 82)); // Note: Number is out of order
// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Ln }, 83)); // Note: Number is out of order
// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Sin }, 84)); // Note: Number is out of order
// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Cos }, 85)); // Note: Number is out of order
// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Tan }, 86)); // Note: Number is out of order
// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Floor }, 87)); // Note: Number is out of order
// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Atan2 }, 89)); // Note: Number is out of order
// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_Throw }, 90)); // Note: Number is out of order
// Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_StringLessThan }, 91)); // Note: Number is out of order
// **** END : From the C# version in the Inference project ****
'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
exports.LISPGrammar = void 0;
const thaw_lexical_analyzer_1 = require("thaw-lexical-analyzer");
const expression_list_1 = require("../../common/domain-object-model/expression-list");
const name_1 = require("../../common/domain-object-model/name");
const variable_1 = require("../../common/domain-object-model/variable");
const variable_list_1 = require("../../common/domain-object-model/variable-list");
const begin_usage_1 = require("../../common/domain-object-model/begin-usage");
const cond_usage_1 = require("../../common/domain-object-model/cond-usage");
const function_definition_1 = require("../../common/domain-object-model/function-definition");
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
const float_literal_1 = require("./domain-object-model/float-literal");
const integer_literal_1 = require("./domain-object-model/integer-literal");
const lisp_operator_usage_1 = require("./domain-object-model/lisp-operator-usage");
const lisp_string_1 = require("./domain-object-model/lisp-string");
const lisp_symbol_1 = require("./domain-object-model/lisp-symbol");
const null_sexpression_1 = require("./domain-object-model/null-sexpression");
const quoted_constant_with_apostrophe_1 = require("./domain-object-model/quoted-constant-with-apostrophe");
const quoted_constant_with_quote_keyword_1 = require("./domain-object-model/quoted-constant-with-quote-keyword");
const sexpression_list_1 = require("./domain-object-model/sexpression-list");
class LISPGrammar extends grammar_base_1.GrammarBase {
    // The LISP grammar from Kamin (the book 'Programming Languages: An Interpreter-Based Approach')
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
        // this.terminals.push(Symbol.terminalRplaca);
        // this.terminals.push(Symbol.terminalRplacd);
        // this.terminals.push(Symbol.terminalDefineMacro);
        // this.terminals.push(Symbol.terminalQuoteKeyword);
        // this.terminals.push(Symbol.terminalStringLiteral);
        // this.terminals.push(Symbol.terminalStringPred);
        // this.terminals.push(Symbol.terminalToString);
        // this.terminals.push(Symbol.terminalListToString);
        // this.terminals.push(Symbol.terminalStringToList);
        // this.terminals.push(Symbol.terminalStringToSymbol);
        // this.terminals.push(Symbol.terminalFloatLiteral);
        // this.terminals.push(Symbol.terminalPow);
        // this.terminals.push(Symbol.terminalExp);
        // this.terminals.push(Symbol.terminalLn);
        // this.terminals.push(Symbol.terminalSin);
        // this.terminals.push(Symbol.terminalCos);
        // this.terminals.push(Symbol.terminalTan);
        // this.terminals.push(Symbol.terminalAtan2);
        // this.terminals.push(Symbol.terminalFloor);
        // this.terminals.push(Symbol.terminalStringLessThan);
        // this.terminals.push(Symbol.terminalRandom);
        // this.terminals.push(Symbol.terminalThrow);
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
        this.nonTerminals.push(symbol_1.Symbol.nonterminalQuotedConst);
        this.nonTerminals.push(symbol_1.Symbol.nonterminalSExpression);
        this.nonTerminals.push(symbol_1.Symbol.nonterminalSExpressionList);
        this.nonTerminals.push(symbol_1.Symbol.nonterminalSExpressionListTail);
        this.nonTerminals.push(symbol_1.Symbol.nonterminalSymbol);
        this.nonTerminals.push(symbol_1.Symbol.nonterminalExprPairList);
        // this.nonTerminals.push(Symbol.nonterminalMacroDef);
        // this.nonTerminals.push(Symbol.nonterminalBracketedEntity);
        this.nonTerminals.push(symbol_1.Symbol.nonterminalBracketedInput);
        this.nonTerminals.push(symbol_1.Symbol.nonterminalUnbracketedInput);
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
        // This initial production needed to be added: Start -> Input EOF
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalStart, [symbol_1.Symbol.nonterminalInput, symbol_1.Symbol.terminalEOF], 1));
        // Input -> ( BracketedInput )
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalInput, [
            symbol_1.Symbol.terminalLeftBracket,
            symbol_1.Symbol.nonterminalBracketedInput,
            symbol_1.Symbol.terminalRightBracket
        ], 2));
        // Input -> UnbracketedInput
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalInput, [symbol_1.Symbol.nonterminalUnbracketedInput], 3));
        // BracketedInput -> BracketedExpression
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalBracketedInput, [symbol_1.Symbol.nonterminalBracketedExpression], 4));
        // BracketedInput -> FunDef
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalBracketedInput, [symbol_1.Symbol.nonterminalFunDef], 5));
        // - UnbracketedInput -> Value
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalUnbracketedInput, [symbol_1.Symbol.nonterminalValue], 6));
        // - UnbracketedInput -> Variable
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalUnbracketedInput, [symbol_1.Symbol.nonterminalVariable], 7));
        // FunDef -> define Function ArgList Expression
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalFunDef, [
            symbol_1.Symbol.terminalDefine,
            symbol_1.Symbol.nonterminalFunction,
            symbol_1.Symbol.nonterminalArgList,
            symbol_1.Symbol.nonterminalExpression,
            '#functionDefinition'
        ], 8));
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
        // BracketedExpression -> Optr ExpressionList
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalBracketedExpression, [
            symbol_1.Symbol.nonterminalOptr,
            symbol_1.Symbol.nonterminalExpressionList,
            '#operatorUsage'
        ], 19));
        // ExpressionList -> Expression ExpressionList
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalExpressionList, [
            symbol_1.Symbol.nonterminalExpression,
            symbol_1.Symbol.nonterminalExpressionList,
            '#expressionList'
        ], 20));
        // ExpressionList -> Lambda
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalExpressionList, [symbol_1.Symbol.Lambda, '#emptyExpressionList'], 21));
        // Optr -> Function
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalOptr, [symbol_1.Symbol.nonterminalFunction], 22));
        // Optr -> Value-Op
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalOptr, [symbol_1.Symbol.nonterminalValueOp], 23));
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
        // Integer -> ...
        // Name -> ...
        // From Chapter 2 of Kamin:
        // Value -> Quoted-Const
        // Value-Op -> cons
        // Value-Op -> car
        // Value-Op -> cdr
        // Value-Op -> number?
        // Value-Op -> symbol?
        // Value-Op -> list?
        // Value-Op -> null?
        // Quoted-Const -> ' S-Expression
        // S-Expression -> Integer
        // S-Expression -> Symbol
        // S-Expression -> ( S-Expression-List )
        // S-Expression-List -> S-Expression S-Expression-List
        // S-Expression-List -> S-Expression . S-Expression
        // S-Expression-List -> Lambda
        // Symbol -> Name
        // BracketedExpression -> cond ( Expression Expression ) ExprPairList
        // ExprPairList -> ( Expression Expression ) ExprPairList
        // ExprPairList -> Lambda
        // Value-Op -> list
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
            symbol_1.Symbol.nonterminalExprPairList
        ], 53));
        // ExprPairList -> Lambda
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalExprPairList, [symbol_1.Symbol.Lambda, '#emptyExprPairList'], 54));
        // Value-Op -> list
        this.productions.push(new production_1.Production(symbol_1.Symbol.nonterminalValueOp, [symbol_1.Symbol.terminalList], 55));
        // **** BEGIN : Ignore for now ****
        // Productions.Add(new Production(Symbol.N_ValueOp, new List<object>() { Symbol.T_StringPred }, 73)); // Note: Number is out of order
        // Productions.Add(new Production(Symbol.N_Value, new List<object>() { Symbol.T_StringLiteral }, 75)); // Note: Number is out of order
        // Productions.Add(new Production(Symbol.N_Value, new List<object>() { Symbol.T_FloatLiteral }, 79)); // Note: Number is out of order
        // Productions.Add(new Production(Symbol.N_BracketedExpression, new List<object>() {
        //     Symbol.N_LetKeyword,
        //     Symbol.T_LeftBracket,
        //     Symbol.N_VarExprList,
        //     Symbol.T_RightBracket,
        //     Symbol.N_Expression, "#letUsage" }, 34));
        // Productions.Add(new Production(Symbol.N_LetKeyword, new List<object>() { Symbol.T_Let }, 35));
        // Productions.Add(new Production(Symbol.N_LetKeyword, new List<object>() { Symbol.T_LetStar }, 36));
        // Productions.Add(new Production(Symbol.N_VarExprList, new List<object>() {
        //     Symbol.T_LeftBracket,
        //     Symbol.N_Variable,
        //     Symbol.N_Expression,
        //     Symbol.T_RightBracket,
        //     Symbol.N_VarExprList, "#varExprList" }, 37));
        // Productions.Add(new Production(Symbol.N_VarExprList, new List<object>() { Symbol.Lambda, "#emptyVarExprList" }, 38));
        // this.productions.push(new Production(Symbol.nonterminalValue, [Symbol.terminalStringLiteral], 75)); // Note: Number is out of order
        // this.productions.push(new Production(Symbol.nonterminalValue, [Symbol.terminalFloatLiteral], 79)); // Note: Number is out of order
        // this.productions.push(new Production(Symbol.nonterminalValueOp, [Symbol.terminalStringPred], 73)); // Note: Number is out of order
        // this.productions.push(new Production(Symbol.nonterminalValueOp, [Symbol.terminalRandom], 33));
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
        // this.productions.push(new Production(Symbol.nonterminalValueOp, [Symbol.terminalFloor], 87));
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
        // this.productions.push(new Production(
        // 	Symbol.nonterminalSExpression, // TODO? : Create Symbol.nonterminalBracketedSExpression ?
        // 	[
        // 		Symbol.terminalLeftBracket,
        // 		Symbol.terminalQuoteKeyword,
        // 		Symbol.nonterminalSExpression,
        // 		Symbol.terminalRightBracket,
        // 		'#quotedConstantWithQuoteKeyword'
        // 	], n));
        // Next production number is 56.
    }
    get languageName() {
        return 'LISP';
    }
    get selectorsOfCompatibleParsers() {
        return [parser_selectors_1.ParserSelector.LL1];
    }
    executeSemanticAction(semanticStack, action) {
        // console.log(`LISPGrammar.executeSemanticAction() : action is ${typeof action} ${action}`);
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
                semanticStack.push(new lisp_operator_usage_1.LISPOperatorUsage(name, expressionList));
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
            case thaw_lexical_analyzer_1.LexicalState.tokenFltLit:
                return symbol_1.Symbol.terminalFloatLiteral;
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
                    case 'quote':
                        return symbol_1.Symbol.terminalQuoteKeyword;
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
                semanticStack.push(new name_1.Name(value, token.line, token.column));
                break;
            case symbol_1.Symbol.terminalIntegerLiteral:
                semanticStack.push(new integer_literal_1.IntegerLiteral(value));
                break;
            case symbol_1.Symbol.terminalFloatLiteral:
                semanticStack.push(new float_literal_1.FloatLiteral(value));
                break;
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
            default:
                throw new argument_exception_1.ArgumentException(`LISPGrammar.createLetUsage() : Unknown 'let' keyword '${letName.value}.`, 'letName', letName.line, letName.column);
        }
    }
}
exports.LISPGrammar = LISPGrammar;
