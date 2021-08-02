// tom-weatherhead/thaw-grammar/src/common/domain-object-model/imacro-definition.ts

import { EnvironmentFrame } from './environment-frame';
import { IExpression } from './iexpression';
import { IGlobalInfo } from './iglobal-info';

export interface IMacroDefinition<T> {
	argumentCount: number; // This is a 'get' accessor.
	invokeMacro(
		unevaluatedArguments: IExpression<T>[],
		localEnvironment: EnvironmentFrame<T>,
		globalInfo: IGlobalInfo<T>
	): T;
}
