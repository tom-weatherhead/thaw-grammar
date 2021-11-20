// tom-weatherhead/thaw-grammar/src/common/domain-object-model/iexpression.ts

import { IStringifiable } from 'thaw-common-utilities.ts';

import { IEnvironmentFrame } from './environment-frame';
import { IGlobalInfo } from './iglobal-info';

export interface IExpression<T> extends IStringifiable {
	// By default, if localEnvironment is undefined, then use globalInfo.globalEnvironment
	// options will be used by Clu and Smalltalk.
	evaluate(
		globalInfo: IGlobalInfo<T>,
		localEnvironment?: IEnvironmentFrame<T>,
		options?: unknown
	): T;
}

// TODO 2021-11-09: Use IExpression<T, U>, where:
// T is the language's common value interface (e.g. ISExpression, IAPLValue);
// U is the type of the options parameter to the evaluate() function;
// e.g. in CLU, U can include a cluster parameter.
// e.g. in Smalltalk, U can include a class parameter and a receiver parameter.
// This should eliminate duplication of classes such as BeginUsage, IfUsage, etc.

// Also, use interfaces more. E.g. refer to IEnvironmentFrame<T>, not to EnvironmentFrame<T>

// I.e.:

// export interface IExpression<T, U> {
// 	evaluate(localEnvironment: IEnvironmentFrame<T>, globalInfo: IGlobalInfo<T>, options?: U): T;
// }
