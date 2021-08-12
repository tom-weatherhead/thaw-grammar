// tom-weatherhead/thaw-grammar/src/languages/prolog/domain-object-model/interfaces/iprolog-number.ts

import { IPrologExpression } from './iprolog-expression';

export interface IPrologNumber extends IPrologExpression {
	ToInteger(): number;
	ToDouble(): number;
}
