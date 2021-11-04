// tom-weatherhead/thaw-grammar/src/languages/smalltalk/domain-object-model/object-instance.ts

// **** BEGIN Bootstrapping Code Part 2: Set up objectInstance ****

import { objectClass, nilClass, selfVar } from './bootstrap';

import { SmalltalkEnvironmentFrame } from './environment-frame';

import { SmalltalkIntegerValue } from './integer';

import { SmalltalkUserValue } from './user-value';

// Create objectInstance

const objectInstanceEnvFrame = new SmalltalkEnvironmentFrame();
export const defaultValue = new SmalltalkIntegerValue(0);

objectInstanceEnvFrame.add(selfVar, defaultValue);

export const objectInstance = new SmalltalkUserValue(objectClass, objectInstanceEnvFrame);

// Tie the self-referential knot:
objectInstance.value.dict.set(selfVar.name, objectInstance);

// Create nilInstance

const nilInstanceEnvFrame = new SmalltalkEnvironmentFrame();

nilInstanceEnvFrame.add(selfVar, defaultValue);

export const nilInstance = new SmalltalkUserValue(nilClass, nilInstanceEnvFrame);

// Tie the self-referential knot:
nilInstance.value.dict.set(selfVar.name, nilInstance);

// true and false: Version 1:

export const falseValue = new SmalltalkIntegerValue(0);
export const trueValue = new SmalltalkIntegerValue(1);

// true and false: Version 2:

// We want true and false to be functions, as in the Lambda calculus:

// (true x y) -> x
// (false x y) -> y

// ... but evaluate x and y lazily; e.g. in the case of 'true', do not evaluate y.

// const x = new SmalltalkVariable('x');
// const y = new SmalltalkVariable('y');
//
// // Note: This will not work; a function definition is not a value. We need closures.
// export const falseValue = new SmalltalkFunctionDefinition(falseVariableName, [x, y], y);
// export const trueValue = new SmalltalkFunctionDefinition(trueVariableName, [x, y], x);

// **** END Bootstrapping Code Part 2: Set up objectInstance ****
