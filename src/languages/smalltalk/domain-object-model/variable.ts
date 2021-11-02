// tom-weatherhead/thaw-grammar/src/languages/smalltalk/domain-object-model/variable.ts

import { ArgumentNullException } from '../../../common/exceptions/argument-null-exception';
// import { EvaluationException } from '../../../common/exceptions/evaluation-exception';
// import { KeyNotFoundException } from '../exceptions/key-not-found-exception';

// import { EnvironmentFrame } from '../../../common/domain-object-model/environment-frame';
// import { IExpression } from '../../../common/domain-object-model/iexpression';
// import { IGlobalInfo } from '../../../common/domain-object-model/iglobal-info';

import {
	ISmalltalkClass,
	ISmalltalkEnvironmentFrame,
	ISmalltalkExpression,
	ISmalltalkGlobalInfo,
	ISmalltalkValue,
	ISmalltalkVariable
} from './interfaces/iexpression';
// import { ISmalltalkValue } from './interfaces/ivalue';

export class SmalltalkVariable implements ISmalltalkExpression, ISmalltalkVariable {
	public readonly name: string;
	// public readonly line: number;
	// public readonly column: number;

	constructor(name: string, public readonly line: number, public readonly column: number) {
		if (!name) {
			throw new ArgumentNullException('A Variable cannot have a null or empty name', 'name');
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

	public evaluate(
		localEnvironment: ISmalltalkEnvironmentFrame | undefined,
		receiver: ISmalltalkValue,
		c: ISmalltalkClass | undefined,
		globalInfo: ISmalltalkGlobalInfo
	): ISmalltalkValue {
		// See Kamin page 295.
		const userVal = receiver.toUserValue();

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
