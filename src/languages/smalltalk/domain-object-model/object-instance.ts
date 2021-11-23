// tom-weatherhead/thaw-grammar/src/languages/smalltalk/domain-object-model/object-instance.ts

// **** BEGIN Bootstrapping Code Part 2 ****

import { Name } from 'thaw-interpreter-core';

import { BeginUsage } from '../../../common/domain-object-model/begin-usage';

import { FunctionDefinition } from '../../../common/domain-object-model/function-definition';

import { ISmalltalkValue } from './interfaces/iexpression';

import { SmalltalkInteger } from './data-types/integer';

import { SmalltalkString } from './data-types/string';

import { SmalltalkUserValue } from './data-types/user-value';

// import { SmalltalkBeginUsage } from './begin-usage';

import { falseVar, objectClass, objectClassName, selfVar, trueVar } from './bootstrap';

import { SmalltalkClass } from './class';

import { SmalltalkEnvironmentFrame } from './environment-frame';

// import { SmalltalkFunctionDefinition } from './function-definition';

import { SmalltalkSetUsage } from './set-usage';

import { SmalltalkVariable } from './variable';

// Create objectInstance

const objectInstanceEnvFrame = new SmalltalkEnvironmentFrame();
export const defaultValue = new SmalltalkInteger(0);

objectInstanceEnvFrame.add(selfVar, defaultValue);

export const objectInstance = new SmalltalkUserValue(objectClass, objectInstanceEnvFrame);

// Tie the self-referential knot:
objectInstance.value.dict.set(selfVar.name, objectInstance);

// true and false: Version 1:

export const falseValue = new SmalltalkInteger(0);
export const trueValue = new SmalltalkInteger(1);

// true and false: Version 2:

// We want true and false to be functions, as in the Lambda calculus:

// (true x y) -> x
// (false x y) -> y

// ... but evaluate x and y lazily; e.g. in the case of 'true', do not evaluate y.

// const x = new SmalltalkVariable('x');
// const y = new SmalltalkVariable('y');
//
// // Note: This will not work; a function definition is not a value. We need closures...
// // Or use Blocks, and unblock only one of the two parameters.
// // E.g. in the case of 'true', unblock x but not y.
// export const falseValue = new SmalltalkFunctionDefinition(falseVariableName, [x, y], y);
// export const trueValue = new SmalltalkFunctionDefinition(trueVariableName, [x, y], x);

// From the C# version:

// Evaluate(string.Format(@"
// (class {0} Object ()
// (stringValue) ; stringValue is used as the value of the object of this class when it is converted to a string.
// (define init () (begin (set stringValue '{1}') self))
// (define if (trueBlock falseBlock) falseBlock)
// (define and (x) {2})
// (define or (x) x)
// (define xor (x) x)
// (define not () {3})
// )", FalseValueClassName, FalseValueAsString, FalseVariableName, TrueVariableName));
// Evaluate(string.Format(@"
// (class {0} Object ()
// (stringValue) ; stringValue is used as the value of the object of this class when it is converted to a string.
// (define init () (begin (set stringValue '{1}') self))
// (define if (trueBlock falseBlock) trueBlock)
// (define and (x) x)
// (define or (x) {2})
// (define xor (x) (not x))
// (define not () {3})
// )", TrueValueClassName, TrueValueAsString, TrueVariableName, FalseVariableName));
// Evaluate(string.Format("(set {0} (init (new {1})))", FalseVariableName, FalseValueClassName));
// Evaluate(string.Format("(set {0} (init (new {1})))", TrueVariableName, TrueValueClassName));
// FalseVal = GlobalEnvironment.Dict[new SmalltalkVariable(FalseVariableName)];
// TrueVal = GlobalEnvironment.Dict[new SmalltalkVariable(TrueVariableName)];

const nilClassName = 'Nil';
const nilVariableName = 'nil';
const nilValueAsString = 'nil';

const falseClassName = 'FalseValue';
const falseValueAsString = 'false';

const trueClassName = 'TrueValue';
const trueValueAsString = 'true';

const stringValueVariableName = 'stringValue';

export const nilVar = new SmalltalkVariable(nilVariableName);
const stringValueVar = new SmalltalkVariable(stringValueVariableName);

const x = new SmalltalkVariable('x');
const y = new SmalltalkVariable('y');

export const nilClass = new SmalltalkClass(
	nilClassName,
	objectClassName,
	[],
	[stringValueVar],
	[
		new FunctionDefinition<ISmalltalkValue>(
			new Name('init'),
			[],
			new BeginUsage<ISmalltalkValue>(
				new SmalltalkSetUsage(stringValueVar, new SmalltalkString(nilValueAsString)),
				[selfVar]
			)
		),
		new FunctionDefinition<ISmalltalkValue>(new Name('isNil'), [], trueVar),
		new FunctionDefinition<ISmalltalkValue>(new Name('notNil'), [], falseVar)
	]
);

export const falseClass = new SmalltalkClass(
	falseClassName,
	objectClassName,
	[],
	[stringValueVar],
	[
		// (define init () (begin (set stringValue '{1}') self)) :
		new FunctionDefinition<ISmalltalkValue>(
			new Name('init'),
			[],
			new BeginUsage<ISmalltalkValue>(
				new SmalltalkSetUsage(stringValueVar, new SmalltalkString(falseValueAsString)),
				[selfVar]
			)
		),
		new FunctionDefinition<ISmalltalkValue>(new Name('if'), [x, y], y)
		// ... and, or, xor, not
	]
);

export const trueClass = new SmalltalkClass(
	trueClassName,
	objectClassName,
	[],
	[stringValueVar],
	[
		// (define init () (begin (set stringValue '{1}') self)) :
		new FunctionDefinition<ISmalltalkValue>(
			new Name('init'),
			[],
			new BeginUsage<ISmalltalkValue>(
				new SmalltalkSetUsage(stringValueVar, new SmalltalkString(trueValueAsString)),
				[selfVar]
			)
		),
		new FunctionDefinition<ISmalltalkValue>(new Name('if'), [x, y], x)
		// ... and, or, xor, not
	]
);

// Create nilInstance

const nilInstanceEnvFrame = new SmalltalkEnvironmentFrame();

nilInstanceEnvFrame.add(selfVar, defaultValue);

export const nilInstance = new SmalltalkUserValue(nilClass, nilInstanceEnvFrame);

// Tie the self-referential knot:
nilInstance.value.dict.set(selfVar.name, nilInstance);

// Create falseInstance ...

const falseInstanceEnvFrame = new SmalltalkEnvironmentFrame();

falseInstanceEnvFrame.add(selfVar, defaultValue);

export const falseInstance = new SmalltalkUserValue(falseClass, falseInstanceEnvFrame);

// Tie the self-referential knot:
falseInstance.value.dict.set(selfVar.name, falseInstance);

// Create trueInstance ...

const trueInstanceEnvFrame = new SmalltalkEnvironmentFrame();

trueInstanceEnvFrame.add(selfVar, defaultValue);

export const trueInstance = new SmalltalkUserValue(trueClass, trueInstanceEnvFrame);

// Tie the self-referential knot:
trueInstance.value.dict.set(selfVar.name, trueInstance);

// **** END Bootstrapping Code Part 2 ****
