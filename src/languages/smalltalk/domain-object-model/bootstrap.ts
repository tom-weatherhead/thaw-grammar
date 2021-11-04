// tom-weatherhead/thaw-grammar/src/languages/smalltalk/domain-object-model/bootstrap.ts

// TODO: Rename this file to object-class.ts

// **** BEGIN Bootstrapping Code Part 1: Set up objectClass ****

import { SmalltalkClass } from './class';

import { SmalltalkFunctionDefinition } from './function-definition';

import { SmalltalkVariable } from './variable';

export const objectClassName = 'Object';

const selfVariableName = 'self';
const falseVariableName = 'false';
const trueVariableName = 'true';

export const selfVar = new SmalltalkVariable(selfVariableName);
export const falseVar = new SmalltalkVariable(falseVariableName);
export const trueVar = new SmalltalkVariable(trueVariableName);

export const objectClass = new SmalltalkClass(
	objectClassName,
	undefined,
	[],
	[selfVar],
	[
		new SmalltalkFunctionDefinition('isNil', [], falseVar),
		new SmalltalkFunctionDefinition('notNil', [], trueVar)
	]
);

// **** END Bootstrapping Code Part 1: Set up objectClass ****
