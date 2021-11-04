// tom-weatherhead/thaw-grammar/src/languages/smalltalk/domain-object-model/bootstrap.ts

// TODO: Rename this file to object-class.ts

// **** BEGIN Bootstrapping Code Part 1: Set up objectClass ****

import { SmalltalkClass } from './class';

import { SmalltalkFunctionDefinition } from './function-definition';

import { SmalltalkVariable } from './variable';

// const nilVariableName = 'nil';
// const nilValueAsString = 'nil';

// const falseValueClassName = 'FalseValue';
// const falseValueAsString = 'false';

// const trueValueClassName = 'TrueValue';
// const trueValueAsString = 'true';

const selfVariableName = 'self';

// const nilVariableName = 'nil';

const falseVariableName = 'false';
const trueVariableName = 'true';

export const selfVar = new SmalltalkVariable(selfVariableName);

export const falseVar = new SmalltalkVariable(falseVariableName);
export const trueVar = new SmalltalkVariable(trueVariableName);

export const objectClass = new SmalltalkClass(
	'Object',
	undefined,
	[],
	[selfVar],
	[
		new SmalltalkFunctionDefinition('isNil', [], falseVar),
		new SmalltalkFunctionDefinition('notNil', [], trueVar)
	]
);

// const objectInstanceEnvFrame = new SmalltalkEnvironmentFrame();
// export const zeroValue = new SmalltalkIntegerValue(0);
//
// objectInstanceEnvFrame.add(selfVar, zeroValue);
//
// export const objectInstance = new SmalltalkUserValue(objectClass, objectInstanceEnvFrame);
//
// // Tie the self-referential knot:
// objectInstance.value.dict.set(selfVar.name, objectInstance);

// **** END Bootstrapping Code Part 1: Set up objectClass ****
