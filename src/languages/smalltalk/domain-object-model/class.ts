// tom-weatherhead/thaw-grammar/src/languages/smalltalk/domain-object-model/class.ts

import { IParser, ITokenizer } from 'thaw-interpreter-types';

import { EnvironmentFrame } from '../../../common/domain-object-model/environment-frame';

import {
	ISmalltalkClass,
	ISmalltalkEnvironmentFrame,
	// ISmalltalkExpression,
	ISmalltalkFunctionDefinition,
	ISmalltalkGlobalInfo,
	ISmalltalkValue,
	ISmalltalkVariable
} from './interfaces/iexpression';
// import { ISmalltalkValue } from './interfaces/ivalue';

// import { SmalltalkEnvironmentFrame } from './environment-frame';

export class SmalltalkClass implements ISmalltalkClass {
	public superClass: ISmalltalkClass | undefined = undefined;
	public readonly exportedDict = new Map<string, ISmalltalkFunctionDefinition>();
	public readonly classVariableEnvFrame = new EnvironmentFrame<ISmalltalkValue>();
	// private static readonly reservedTypeNames = new HashSet<string>() { "int", "float", "symbol", "char", "string", "array" };

	constructor(
		public readonly className: string,
		public readonly superClassName: string | undefined,
		public readonly classVariableList: ISmalltalkVariable[],
		public readonly clRep: ISmalltalkVariable[],
		private exportedList: ISmalltalkFunctionDefinition[],
		public readonly line = 0,
		public readonly column = 0
	) {}

	public findMethod(methodName: string): {
		method: ISmalltalkFunctionDefinition | undefined;
		classInWhichMethodWasFound: ISmalltalkClass | undefined;
	} {
		const method = this.exportedDict.get(methodName);

		if (typeof method !== 'undefined') {
			return { method, classInWhichMethodWasFound: this };
		} else if (typeof this.superClass !== 'undefined') {
			return this.superClass.findMethod(methodName);
		} else {
			return { method: undefined, classInWhichMethodWasFound: undefined };
		}
	}

	public findClassVariableValue(variable: ISmalltalkVariable): ISmalltalkValue | undefined {
		const value = this.classVariableEnvFrame.lookup(variable);

		if (typeof value !== 'undefined') {
			return value;
		} else if (typeof this.superClass !== 'undefined') {
			return this.superClass.findClassVariableValue(variable);
		} else {
			return undefined;
		}
	}

	public trySetClassVariableValue(variable: ISmalltalkVariable, value: ISmalltalkValue): boolean {
		if (this.classVariableEnvFrame.isDefined(variable)) {
			this.classVariableEnvFrame.dict.set(variable.name, value);
			// Or: this.classVariableEnvFrame.add(variable, value);

			return true;
		} else if (typeof this.superClass !== 'undefined') {
			return this.superClass.trySetClassVariableValue(variable, value);
		} else {
			return false;
		}
	}

	public addFunction(tokenizer: ITokenizer, parser: IParser, functionAsString: string): void {
		const funcDef = parser.parse(
			tokenizer.tokenize(functionAsString)
		) as ISmalltalkFunctionDefinition;

		this.exportedDict.set(funcDef.functionName.value, funcDef);
	}

	/* eslint-disable no-unused-vars */
	// public evaluate(
	// 	localEnvironment: ISmalltalkEnvironmentFrame | undefined,
	// 	receiver: ISmalltalkValue, // | undefined,
	// 	c: ISmalltalkClass | undefined,
	// 	globalInfo: ISmalltalkGlobalInfo
	// ): ISmalltalkValue {
	/* eslint-disable @typescript-eslint/no-unused-vars */
	public evaluate(
		globalInfo: ISmalltalkGlobalInfo, // I.e. IGlobalInfo<ISmalltalkValue>
		localEnvironment: ISmalltalkEnvironmentFrame | undefined, // I.e. IEnvironmentFrame<ISmalltalkValue> | undefined
		options?: unknown
		/* eslint-enable @typescript-eslint/no-unused-vars */
	): ISmalltalkValue {
		// if (reservedTypeNames.indexOf(this.className) >= 0) {
		//     throw new EvaluationException(
		//         string.Format("SmalltalkClass.Evaluate() : Cannot create a class named '{0}'; reserved typename", ClassName),
		//         LineNumber, ColumnNumber);
		// }

		globalInfo.classDict.set(this.className, this);

		// if (string.IsNullOrEmpty(SuperClassName)) {
		//     throw new EvaluationException(
		//         string.Format("SmalltalkClass.Evaluate() : Class {0} : SuperClassName is null or empty", ClassName),
		//         LineNumber, ColumnNumber);
		// }
		// TODO 2014/12/09 : Throw an exception if ClassName == SuperClassName
		// else

		if (typeof this.superClassName !== 'undefined') {
			const superClass = globalInfo.classDict.get(this.superClassName);

			if (typeof superClass === 'undefined') {
				// throw new EvaluationException(
				//     string.Format("SmalltalkClass.Evaluate() : Class {0} : Unknown SuperClass {1}", ClassName, SuperClassName),
				//     LineNumber, ColumnNumber);
				throw new Error(
					`SmalltalkClass.Evaluate() : Class ${this.className} : Unknown SuperClass ${this.superClassName}`
				);
			}

			this.superClass = superClass;
			// this.clRep.AddRange(this.superClass.clRep);

			for (const v of this.superClass.clRep) {
				this.clRep.push(v);
			}
		}

		for (const exportedFuncDef of this.exportedList) {
			this.exportedDict.set(exportedFuncDef.functionName.value, exportedFuncDef);
		}

		for (const classVariable of this.classVariableList) {
			this.classVariableEnvFrame.add(classVariable, globalInfo.falseValue);
		}

		return globalInfo.falseValue;
	}
	/* eslint-enable no-unused-vars */
}
