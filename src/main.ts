// tom-weatherhead/thaw-grammar/src/main.ts

// Much of the code in this library was based on or inspired by the book
// 'Programming Languages: An Interpreter-Based Approach', by Samuel N. Kamin, 1990,
// Addison Wesley, ISBN 0-201-06824-9.

// Exceptions
export { KeyNotFoundException } from './common/exceptions/key-not-found-exception';
// export { NotImplementedException } from './common/exceptions/not-implemented-exception';

// Common DOM (Domain Object Model)
export { EnvironmentFrame } from './common/domain-object-model/environment-frame';
export { IExpression } from './common/domain-object-model/iexpression';
export { GlobalInfoBase } from './common/domain-object-model/global-info-base';
export { IGlobalInfo } from './common/domain-object-model/iglobal-info';
// export { IGlobalInfoOps } from './common/domain-object-model/iglobal-info-ops';
export { Variable } from './common/domain-object-model/variable';

// Other common stuff
export { createGrammar } from './common/grammar-factory';

// My (ThAW's) minimal language
export { MinimalLanguageGlobalInfo } from './languages/minimal/domain-object-model/global-info';

// The Chapter 1 language from Kamin (Kamin 1/8)
export { Chapter1GlobalInfo } from './languages/chapter1/domain-object-model/global-info';

// LISP (Kamin 2/8)
export { ISExpression } from './languages/lisp/domain-object-model/isexpression';
export { LISPGlobalInfo } from './languages/lisp/domain-object-model/lisp-global-info';

export { LISPGrammarForLRParser } from './languages/lisp/lisp-grammar-for-lr-parser';

// APL (Kamin 3/8)
export { IAPLExpression, IAPLValue } from './languages/apl/domain-object-model/interfaces/ivalue';
export { createAPLNullValue } from './languages/apl/domain-object-model/data-types/value';
export { APLGlobalInfo } from './languages/apl/domain-object-model/global-info';

// Scheme (Kamin 4/8)
export { PrimOp } from './languages/scheme/domain-object-model/primitive-operator';
export { SchemeGlobalInfo } from './languages/scheme/domain-object-model/scheme-global-info';

// SASL (Kamin 5/8)
export { SASLGlobalInfo } from './languages/sasl/domain-object-model/global-info';

// CLU (Kamin 6/8)
export { ICLUExpression, ICLUValue } from './languages/clu/domain-object-model/interfaces/ivalue';
export { CLUGlobalInfo } from './languages/clu/domain-object-model/global-info';

// Smalltalk (Kamin 7/8)
export * from './languages/smalltalk/domain-object-model/interfaces/iexpression';
export { SmalltalkGlobalInfo } from './languages/smalltalk/domain-object-model/global-info';
export { SmalltalkEnvironmentFrame } from './languages/smalltalk/domain-object-model/environment-frame';

// Prolog (Kamin 8/8)
export { PrologClause } from './languages/prolog/domain-object-model/prolog-clause';
export { PrologFloatLiteral } from './languages/prolog/domain-object-model/prolog-float-literal';
export {
	isPrologFunctorExpression,
	PrologFunctorExpression
} from './languages/prolog/domain-object-model/prolog-functor-expression';
export { PrologGlobalInfo } from './languages/prolog/domain-object-model/prolog-global-info';
export { isPrologGoal, PrologGoal } from './languages/prolog/domain-object-model/prolog-goal';
export { PrologIntegerLiteral } from './languages/prolog/domain-object-model/prolog-integer-literal';
export { createVariable } from './languages/prolog/domain-object-model/prolog-variable';

export { IPrologExpression } from './languages/prolog/domain-object-model/interfaces/iprolog-expression';
export {
	isIVariable,
	IVariable
} from './languages/prolog/domain-object-model/interfaces/ivariable';

// The Lambda Calculus
export * from './languages/lambda-calculus/church-numerals';
export * from './languages/lambda-calculus/operators';
export * from './languages/lambda-calculus/type-guards';
export * from './languages/lambda-calculus/utilities';
export * from './languages/lambda-calculus/utilities-with-dom';

export * from './languages/lambda-calculus/domain-object-model/interfaces/expression';

export * from './languages/lambda-calculus/domain-object-model/call';
export * from './languages/lambda-calculus/domain-object-model/lambda-expression';

// The Lambda Calculus with Integer Extensions
export * from './languages/lambda-calculus-integer-extension/domain-object-model/integer-literal';

export * from './languages/lambda-calculus-integer-extension/domain-object-model/primitive-operator';
