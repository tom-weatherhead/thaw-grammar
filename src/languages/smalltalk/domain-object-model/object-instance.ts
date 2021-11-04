// object-instance.ts

// **** BEGIN Bootstrapping Code Part 2: Set up objectInstance ****

import { objectClass, selfVar } from './bootstrap';

import { SmalltalkEnvironmentFrame } from './environment-frame';

import { SmalltalkIntegerValue } from './integer';

import { SmalltalkUserValue } from './user-value';

const objectInstanceEnvFrame = new SmalltalkEnvironmentFrame();
export const defaultValue = new SmalltalkIntegerValue(0);

objectInstanceEnvFrame.add(selfVar, defaultValue);

export const objectInstance = new SmalltalkUserValue(objectClass, objectInstanceEnvFrame);

// Tie the self-referential knot:
objectInstance.value.dict.set(selfVar.name, objectInstance);

// **** END Bootstrapping Code Part 2: Set up objectInstance ****
