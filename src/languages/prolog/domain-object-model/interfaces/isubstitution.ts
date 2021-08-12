// tom-weatherhead/thaw-grammar/src/languages/prolog/domain-object-model/interfaces/isubstitution.ts

import { IPrologExpression } from './iprolog-expression';

export interface ISubstitution {
	readonly SubstitutionList: Map<string, IPrologExpression>;

	length: number;
	// IsOneToOne: boolean;

	toString(): string;
	Compose(otherSub: ISubstitution): ISubstitution;
	// ContainsOnlyVariables(): boolean;
	// FindBindingVariables(): Set<PrologVariable>;
}
