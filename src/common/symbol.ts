// tom-weatherhead/thaw-grammar/src/common/symbols.ts

// TODO: Rename Symbol as something like GrammarSymbol,
// since Symbol is already defined by JavaScript or Node.js
// (e.g. Symbol.iterator)

// export enum GrammarSymbol {
// export enum Symbol {
// 	UndefinedSymbol = 0,
// 	Lambda,
// 	Dot,
//
// 	terminalBegin,
// 	terminalEnd,
// 	terminalAssign,
// 	terminalSemicolon,
// 	terminalRead,
// 	terminalWrite,
// 	terminalLeftBracket,
// 	terminalRightBracket,
// 	terminalLeftSquareBracket,
// 	terminalRightSquareBracket,
// 	terminalOrBar,
// 	terminalComma,
// 	terminalID,
// 	terminalVariable,
// 	terminalIntegerLiteral,
// 	terminalFloatLiteral,
// 	terminalStringLiteral,
// 	terminalPlus,
// 	terminalMinus,
// 	terminalCond,
// 	terminalLet,
// 	terminalLetStar,
//
// 	// LISP terminals
// 	terminalDefine,
// 	terminalIf,
// 	terminalWhile,
// 	terminalSet,
// 	terminalMultiply,
// 	terminalDivide,
// 	terminalEquals,
// 	terminalLessThan,
// 	terminalGreaterThan,
// 	terminalPrint,
// 	terminalCons,
// 	terminalCar,
// 	terminalCdr,
// 	terminalNumberPred,
// 	terminalSymbolPred,
// 	terminalListPred,
// 	terminalNullPred,
// 	terminalStringPred,
// 	terminalApostrophe,
// 	terminalDot,
// 	terminalList,
// 	terminalRplaca,
// 	terminalRplacd,
// 	terminalDefineMacro,
// 	terminalQuoteKeyword,
// 	terminalRandom,
// 	terminalToString,
// 	terminalListToString,
// 	terminalStringToList,
// 	terminalStringToSymbol,
// 	terminalSin,
// 	terminalCos,
// 	terminalTan,
// 	terminalPow,
// 	terminalExp,
// 	terminalLn,
// 	terminalFloor,
// 	terminalAtan2,
// 	terminalThrow,
// 	terminalStringLessThan,
//
// 	// Scheme terminals
// 	terminalPrimOpPred,
// 	terminalClosurePred,
// 	terminalLambdaKeyword,
// 	terminalLetRec,
// 	terminalCallCC,
//
// 	// Prolog terminals
// 	terminalNameBeginningWithCapital,
// 	terminalNameNotBeginningWithCapital,
// 	terminalFrom,
// 	terminalInferPred,
// 	terminalNotSymbol,
// 	terminalIs,
//
// 	// EcstaSKI terminals
// 	terminalFn,
// 	terminalThickArrow,
//
// 	// terminal,
//
// 	terminalEOF,
//
// 	// Non-terminal symbols
// 	nonterminalStart, // The start symbol (i.e. the system goal)
// 	nonterminalProgram,
// 	nonterminalStatementList,
// 	nonterminalStatementTail,
// 	nonterminalStatement,
// 	nonterminalIDList,
// 	nonterminalIDTail,
// 	nonterminalExprList,
// 	nonterminalExprTail,
// 	nonterminalExpression,
// 	nonterminalTerm,
// 	nonterminalPrimaryTail,
// 	nonterminalPrimary,
// 	nonterminalAddOp,
//
// 	// Chapter 1 non-terminals
// 	nonterminalBracketedEntity, // TODO: Migrate away from this; use nonterminalBracketedInput and nonterminalUnbracketedInput instead.
//
// 	// LISP non-terminals
// 	nonterminalInput,
// 	nonterminalFunDef, // Function definition
// 	nonterminalFunction,
// 	nonterminalArgList,
// 	nonterminalVariableList,
// 	nonterminalVariable,
// 	nonterminalValue,
// 	nonterminalBracketedExpression,
// 	nonterminalExpressionList,
// 	nonterminalOptr,
// 	nonterminalValueOp,
// 	nonterminalQuotedConst,
// 	nonterminalSExpression,
// 	nonterminalSExpressionList,
// 	nonterminalSymbol,
// 	nonterminalMacroDef,
// 	nonterminalExprPairList,
// 	nonterminalLetKeyword,
// 	nonterminalVarExprList,
//
// 	nonterminalBracketedInput,
// 	nonterminalUnbracketedInput,
// 	nonterminalSExpressionListTail,
//
// 	// Prolog non-terminals
// 	nonterminalClause,
// 	nonterminalQuery,
// 	nonterminalGoal,
// 	// nonterminalLHSGoal,
// 	nonterminalClauseTail,
// 	// nonterminalLHSGoalTail,
// 	// nonterminalFunctor,
// 	// nonterminalFunctorParameters,
// 	// nonterminalGoalWithPossibleDisjunctiveTail,
// 	nonterminalGoalList,
// 	nonterminalGoalListTail,
// 	// nonterminalPossibleDisjunctiveTail,
// 	nonterminalList,
// 	nonterminalListContents,
// 	nonterminalListContentsTail,
// 	nonterminalFunctorExpression,
// 	nonterminalTailOfGoalOrFunctorExpression,
// 	nonterminalExpressionListTail,
//
// 	// EcstaSKI non-terminals
// 	nonterminalLambdaExpression,
// 	nonterminalLetStatement,
//
// 	nonterminalFunctionCall
// }
