// tom-weatherhead/thaw-grammar/src/languages/smalltalk/domain-object-model/bootstrap.ts

// TODO: Rename this file to object-class.ts

// **** BEGIN Bootstrapping Code Part 1: Set up objectClass ****

import { SmalltalkClass } from './class';

import { SmalltalkFunctionDefinition } from './function-definition';

import { SmalltalkVariable } from './variable';

const objectClassName = 'Object';

const nilClassName = 'Nil';
// const nilValueAsString = 'nil';

// const falseValueClassName = 'FalseValue';
// const falseValueAsString = 'false';

// const trueValueClassName = 'TrueValue';
// const trueValueAsString = 'true';

const selfVariableName = 'self';
const nilVariableName = 'nil';
const falseVariableName = 'false';
const trueVariableName = 'true';

export const selfVar = new SmalltalkVariable(selfVariableName);
export const nilVar = new SmalltalkVariable(nilVariableName);
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

export const nilClass = new SmalltalkClass(
	nilClassName,
	objectClassName,
	[],
	[],
	[
		new SmalltalkFunctionDefinition('isNil', [], trueVar),
		new SmalltalkFunctionDefinition('notNil', [], falseVar)
	]
);

// **** END Bootstrapping Code Part 1: Set up objectClass ****
