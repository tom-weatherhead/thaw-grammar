// tom-weatherhead/thaw-grammar/src/languages/smalltalk/domain-object-model/variable.ts

import { ArgumentException } from 'thaw-interpreter-core';

// import { ArgumentNullException } from '../../../common/exceptions/argument-null-exception';
// import { EvaluationException } from '../../../common/exceptions/evaluation-exception';
// import { KeyNotFoundException } from '../exceptions/key-not-found-exception';

// import { EnvironmentFrame } from '../../../common/domain-object-model/environment-frame';
// import { IExpression } from '../../../common/domain-object-model/iexpression';
// import { IGlobalInfo } from '../../../common/domain-object-model/iglobal-info';

import {
	// ISmalltalkClass,
	ISmalltalkEnvironmentFrame,
	ISmalltalkEvaluateOptions,
	ISmalltalkExpression,
	ISmalltalkGlobalInfo,
	ISmalltalkValue,
	ISmalltalkVariable
} from './interfaces/iexpression';

const typenameSmalltalkVariable = 'SmalltalkVariable';

export function isSmalltalkVariable(obj: unknown): obj is ISmalltalkVariable {
	const v = obj as ISmalltalkVariable;

	return (
		typeof v !== 'undefined' &&
		typeof v.typename !== 'undefined' &&
		v.typename === typenameSmalltalkVariable
	);
}

export class SmalltalkVariable implements ISmalltalkExpression, ISmalltalkVariable {
	public readonly typename: string = typenameSmalltalkVariable;
	public readonly name: string;
	// public readonly line: number;
	// public readonly column: number;

	constructor(name: string, public readonly line = 0, public readonly column = 0) {
		if (!name) {
			throw new ArgumentException('A Variable cannot have an empty name', 'name');
		}

		// Console.WriteLine("Creating a Variable named '{0}'.", name);

		// if (name.StartsWith("Inference")) {
		// 	throw new Exception(string.Format("Error: Creating variable named '{0}'.", name));
		// }

		this.name = name;
		// this.line = line;
		// this.column = column;
	}

	public toString(): string {
		return this.name;
	}

	// equals()

	// public evaluate(
	// 	localEnvironment: EnvironmentFrame<ISmalltalkValue>,
	// 	// eslint-disable-next-line @typescript-eslint/no-unused-vars
	// 	globalInfo: IGlobalInfo<ISmalltalkValue>
	// ): ISmalltalkValue {
	// 	try {
	// 		return localEnvironment.lookup(this);
	// 	} catch (KeyNotFoundException) {
	// 		throw new EvaluationException(
	// 			`Variable.Evaluate() : No value found for variable ${this.name}`,
	// 			this.line,
	// 			this.column
	// 		);
	// 	}
	// }

	/* eslint-disable no-unused-vars */
	public evaluate(
		globalInfo: ISmalltalkGlobalInfo, // I.e. IGlobalInfo<ISmalltalkValue>
		localEnvironment: ISmalltalkEnvironmentFrame | undefined, // I.e. IEnvironmentFrame<ISmalltalkValue> | undefined
		options?: unknown
		/* eslint-enable @typescript-eslint/no-unused-vars */
	): ISmalltalkValue {
		// See Kamin page 295.

		// if (typeof options === 'undefined') {
		// 	throw new Error('SmalltalkVariable.evaluate() : options is undefined');
		// }

		const optionsX = options as ISmalltalkEvaluateOptions;
		const receiver = typeof optionsX !== 'undefined' ? optionsX.receiver : undefined;
		const userVal = typeof receiver !== 'undefined' ? receiver.toUserValue() : undefined;

		if (typeof localEnvironment !== 'undefined' && localEnvironment.isDefined(this)) {
			// TODO: Stop this Lookup (and IsDefined above) from searching the GlobalEnvironment.
			// Probably the best way to do this is to avoid linking any environment frames with the GlobalEnvironment.
			// 2013/12/05 : I believe this is a non-issue, since the top-level Evaluate passes in a null localEnvironment.
			return localEnvironment.lookup(this);
		} else if (typeof userVal !== 'undefined' && userVal.value.dict.has(this.name)) {
			return userVal.value.dict.get(this.name) as ISmalltalkValue;
		} else {
			// 2013/12/05 : Question: In what situations does c differ from userVal.Owner?
			// - At the top level Evaluate(), where c is null, and userVal.Owner is ObjectClass (because userVal is ObjectInstance)
			// ? Perhaps in class hierarchies, e.g. where class D inherits from C which inherits from B; one can imagine an instance where
			//   userVal.Owner is class D, and c is class C (so that "super" refers to class B).
			//   - I.e. if userVal is an object of type D, but we are in a function in class C.

			const c = typeof optionsX !== 'undefined' ? optionsX.c : undefined;

			if (typeof c !== 'undefined') {
				// TODO: Should we use userVal.Owner instead of c?  2013/12/05 : I think not; the current implementation matches the code in SmalltalkSetUsage.Evaluate().
				const value = c.findClassVariableValue(this);

				if (typeof value !== 'undefined') {
					return value;
				}
			}

			return globalInfo.globalEnvironment.lookup(this);
		}
	}
}
