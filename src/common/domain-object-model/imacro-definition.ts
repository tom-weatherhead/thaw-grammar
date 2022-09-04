// tom-weatherhead/thaw-grammar/src/common/domain-object-model/imacro-definition.ts

import { IEnvironmentFrame } from './environment-frame';
import { IExpression } from './iexpression';
import { IGlobalInfo } from './iglobal-info';

export interface IMacroDefinition<T> {
	argumentCount: number; // This is a 'get' accessor.
	invokeMacro(
		unevaluatedArguments: IExpression<T>[],
		localEnvironment: IEnvironmentFrame<T> | undefined,
		globalInfo: IGlobalInfo<T>
	): T;
}
