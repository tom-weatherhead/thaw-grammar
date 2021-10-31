// tom-weatherhead/thaw-grammar/src/languages/smalltalk/domain-object-model/interfaces/iexpression.ts

import { IExpression } from '../../../../common/domain-object-model/iexpression';

import { ISmalltalkValue } from './ivalue';

export type ISmalltalkExpression = IExpression<ISmalltalkValue>;
