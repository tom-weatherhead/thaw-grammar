// clu/domain-object-model/operator-usage.ts

// import { ArgumentException } from 'thaw-interpreter-core';

import { IEnvironmentFrame } from '../../../common/domain-object-model/environment-frame';

import { IGlobalInfo } from '../../../common/domain-object-model/iglobal-info';

import {
	// ICLUEnvironmentFrame,
	ICLUExpression,
	ICLUFunctionName,
	// ICLUGlobalInfo,
	ICluster,
	ICLUValue,
	ICLUVariable
} from './interfaces/ivalue';

// import { isCLUPrimitiveValue } from './data-types/primitive-value';

import { CLUUserValue, isCLUUserValue } from './data-types/user-value';

import { isCluEvaluateOptions } from './cluster';

import { isCLUConstructorDefinition } from './constructor-definition';

import { CLUEnvironmentFrame } from './environment-frame';

import { CLUFunctionDefinitionBase } from './function-definition-base';

import { isCLUGlobalInfo } from './global-info';

import {
	CLUNormalFunctionDefinition,
	isCLUNormalFunctionDefinition
} from './normal-function-definition';

import { isOnePartFunctionName } from './one-part-function-name';

import { isCLUSelectorDefinition } from './selector-definition';

import { isCLUSettorDefinition } from './settor-definition';

import { isTwoPartFunctionName } from './two-part-function-name';

export class FunctionNotExportedException extends Error {
	constructor(clusterName: string, functionName: string) {
		super(`The cluster '${clusterName}' does not export a function named '${functionName}'.`);
	}
}

const builtInOperatorNames = ['+', '-', '*', '/', '=', '<', '>', 'print'];

const twoArgumentIntegerOperators = new Map<string, (x: number, y: number) => number>();
const twoArgumentIntegerPredicates = new Map<string, (x: number, y: number) => boolean>();

twoArgumentIntegerOperators.set('+', (x: number, y: number) => x + y);
twoArgumentIntegerOperators.set('-', (x: number, y: number) => x - y);
twoArgumentIntegerOperators.set('*', (x: number, y: number) => x * y);
twoArgumentIntegerOperators.set('/', (x: number, y: number) => Math.floor(x / y));

twoArgumentIntegerPredicates.set('=', (x: number, y: number) => x === y);
twoArgumentIntegerPredicates.set('<', (x: number, y: number) => x < y);
twoArgumentIntegerPredicates.set('>', (x: number, y: number) => x > y);

export class CLUOperatorUsage implements ICLUExpression {
	private readonly clusterName: string;
	private readonly functionName: string;

	constructor(operatorName: ICLUFunctionName, public readonly expressionList: ICLUExpression[]) {
		if (isTwoPartFunctionName(operatorName)) {
			this.clusterName = operatorName.clusterPart;
			this.functionName = operatorName.functionPart;
		} else if (isOnePartFunctionName(operatorName)) {
			this.clusterName = '';
			this.functionName = operatorName.functionPart;
		} else {
			throw new Error('CLUOperatorUsage constructor');
		}
	}

	/*
	public override string ToString()
	{
		return string.Format("({0} {1})", OperatorName, ExpressionList);
	}
	 */

	protected tryGetExpectedNumArgs(
		funDef: CLUFunctionDefinitionBase | undefined,
		cluster: ICluster | undefined,
		// eslint-disable-next-line @typescript-eslint/no-unused-vars
		globalInfo: IGlobalInfo<ICLUValue>
	): number | undefined {
		if (typeof funDef === 'undefined') {
			switch (this.functionName) {
				case 'print':
					return 1;

				case '+':
				case '-':
				case '*':
				case '/':
				case '=':
				case '<':
				case '>':
					return 2;

				default:
					throw new Error(
						`TryGetExpectedNumArgs() : Unknown built-in operator '${this.functionName}'.`
					);
			}
		}

		if (isCLUNormalFunctionDefinition(funDef)) {
			return funDef.argList.length;
		} else if (isCLUConstructorDefinition(funDef)) {
			if (typeof cluster === 'undefined') {
				throw new Error('tryGetExpectedNumArgs() : cluster is undefined');
			}

			return cluster.clRep.length;
		} else if (isCLUSelectorDefinition(funDef)) {
			return 1;
		} else if (isCLUSettorDefinition(funDef)) {
			return 2;
		} else {
			throw new Error('tryGetExpectedNumArgs() : Unknown operator type.');
		}
	}

	protected checkArgTypes(
		funDef: CLUFunctionDefinitionBase | undefined,
		cluster: ICluster | undefined,
		evaluatedArguments: ICLUValue[]
	): void {
		if (typeof funDef !== 'undefined') {
			let associatedVariable: ICLUVariable | undefined;

			if (isCLUSelectorDefinition(funDef)) {
				associatedVariable = funDef.associatedVariable;
			} else if (isCLUSettorDefinition(funDef)) {
				associatedVariable = funDef.associatedVariable;
			}

			if (typeof associatedVariable !== 'undefined') {
				const userValue = evaluatedArguments[0]; // as ICLUUserValue;

				if (!isCLUUserValue(userValue)) {
					throw new Error(
						'Selector/settor arg type check: First arg is not a CLUUserValue.'
					);
				} else if (!userValue.value.has(associatedVariable)) {
					throw new Error(
						`Selector/settor arg type check: First arg does not contain the member var ${associatedVariable}.`
					);
				}
			}

			return;
		}

		// if (IntegerOperatorKeeper.TwoArgumentOperators.ContainsKey(this.functionName) ||
		// 	IntegerOperatorKeeper.TwoArgumentPredicates.ContainsKey(this.functionName)) {
		//
		// 	if (!isCLUPrimitiveValue(evaluatedArguments[0])) {
		// 		throw new ArgumentException(`Operator ${this.functionName} : First argument is not a number.`, 'evaluatedArguments[0]');
		// 	}
		//
		// 	if (!isCLUPrimitiveValue(evaluatedArguments[1])) {
		// 		throw new ArgumentException(`Operator ${this.functionName} : Second argument is not a number.`, 'evaluatedArguments[1]');
		// 	}
		// }
	}

	protected evaluateNormal(
		funDef: CLUNormalFunctionDefinition | undefined,
		evaluatedArguments: ICLUValue[],
		cluster: ICluster | undefined,
		globalInfo: IGlobalInfo<ICLUValue>
	): ICLUValue {
		const firstArgAsInt =
			evaluatedArguments.length > 0 && globalInfo.valueIsInteger(evaluatedArguments[0])
				? globalInfo.valueAsInteger(evaluatedArguments[0])
				: 0;
		const secondArgAsInt =
			evaluatedArguments.length > 1 && globalInfo.valueIsInteger(evaluatedArguments[1])
				? globalInfo.valueAsInteger(evaluatedArguments[1])
				: 0;

		if (typeof cluster === 'undefined') {
			const operator = twoArgumentIntegerOperators.get(this.functionName);
			const predicate = twoArgumentIntegerPredicates.get(this.functionName);

			if (typeof operator !== 'undefined') {
				return globalInfo.integerAsValue(operator(firstArgAsInt, secondArgAsInt));
			} else if (typeof predicate !== 'undefined') {
				return predicate(firstArgAsInt, secondArgAsInt)
					? globalInfo.trueValue
					: globalInfo.falseValue;
			}

			switch (this.functionName) {
				case '=':
					return evaluatedArguments[0].equals(evaluatedArguments[1])
						? globalInfo.trueValue
						: globalInfo.falseValue;

				case 'print':
					console.log(evaluatedArguments[0]);

					return evaluatedArguments[0];

				default:
					break;
			}
		}

		if (typeof funDef === 'undefined') {
			throw new Error('evaluateNormal() : funDef is undefined');
		}

		// Evaluate a user-defined function.
		const newEnvironment = new CLUEnvironmentFrame(globalInfo.globalEnvironment);

		newEnvironment.compose(funDef.argList, evaluatedArguments);

		return funDef.body.evaluate(globalInfo, newEnvironment, { cluster });
	}

	// public evaluate(
	// 	localEnvironment: ICLUEnvironmentFrame,
	// 	cluster: ICluster | undefined,
	// 	globalInfo: ICLUGlobalInfo
	// ): ICLUValue {
	public evaluate(
		globalInfo: IGlobalInfo<ICLUValue>,
		localEnvironment?: IEnvironmentFrame<ICLUValue>,
		options?: unknown
	): ICLUValue {
		if (!isCluEvaluateOptions(options)) {
			throw new Error('CLUOperatorUsage.evaluate() : options is not CluEvaluateOptions');
		}

		let { cluster } = options;
		const originalCluster = cluster;
		let funDef: CLUFunctionDefinitionBase | undefined;

		if (this.clusterName !== '') {
			if (!isCLUGlobalInfo(globalInfo)) {
				throw new Error('Cluster.evaluate() : globalInfo is not isCLUGlobalInfo.');
			}

			cluster = globalInfo.clusterDict.get(this.clusterName);

			if (typeof cluster === 'undefined') {
				throw new Error(
					`CLUOperatorUsage.evaluate() : Unknown cluster '${this.clusterName}'.`
				);
			}

			funDef = cluster.exportedDict.get(this.functionName);

			if (typeof funDef === 'undefined') {
				//throw new Exception(string.Format("CLUOperatorUsage.Evaluate() : Cluster '{0}' does not contain an exported function named '{1}'.", ClusterName, FunctionName));
				throw new FunctionNotExportedException(this.clusterName, this.functionName);
			}
		} else if (typeof cluster === 'undefined') {
			if (builtInOperatorNames.indexOf(this.functionName) >= 0) {
				funDef = undefined;
			} else {
				funDef = globalInfo.functionDefinitions.get(this.functionName);

				if (typeof funDef === 'undefined') {
					throw new Error(
						`CLUOperatorUsage.evaluate() : Unknown global function '${this.functionName}'.`
					);
				}
			}
		} else if (cluster.exportedDict.has(this.functionName)) {
			funDef = cluster.exportedDict.get(this.functionName);
		} else if (cluster.nonExportedDict.has(this.functionName)) {
			funDef = cluster.nonExportedDict.get(this.functionName);
		} else {
			cluster = undefined;

			if (builtInOperatorNames.indexOf(this.functionName) >= 0) {
				funDef = undefined;
			} else {
				if (!globalInfo.functionDefinitions.has(this.functionName)) {
					throw new Error(
						`CLUOperatorUsage.evaluate() : Unknown global function '${this.functionName}'.`
					);
				}

				funDef = globalInfo.functionDefinitions.get(this.functionName);
			}
		}

		// At this point, funDef == null means that it's a built-in operator.

		const actualNumArgs = this.expressionList.length;
		const expectedNumArgs = this.tryGetExpectedNumArgs(funDef, cluster, globalInfo);

		if (typeof expectedNumArgs === 'undefined') {
			throw new Error(
				`CLUOperatorUsage.evaluate() : Unknown operator name '${this.functionName}'.`
			);
		} else if (actualNumArgs != expectedNumArgs) {
			throw new Error(
				`CLUOperatorUsage : Expected ${expectedNumArgs} arguments for operator '${this.functionName}', instead of the actual ${actualNumArgs} arguments.`
			);
		}

		// Evaluate using originalCluster, not cluster:
		const evaluatedArguments = this.expressionList.map((expr) =>
			expr.evaluate(globalInfo, localEnvironment, { cluster: originalCluster })
		);

		this.checkArgTypes(funDef, cluster, evaluatedArguments);

		if (typeof funDef === 'undefined' || isCLUNormalFunctionDefinition(funDef)) {
			return this.evaluateNormal(funDef, evaluatedArguments, cluster, globalInfo);
		} else if (isCLUConstructorDefinition(funDef)) {
			if (typeof cluster === 'undefined') {
				throw new Error('CLUOperatorUsage.evaluate() : cluster is undefined');
			}

			const newEnvironment = new CLUEnvironmentFrame(globalInfo.globalEnvironment);

			newEnvironment.compose(cluster.clRep, evaluatedArguments);
			return new CLUUserValue(cluster, newEnvironment);
		} else if (isCLUSelectorDefinition(funDef) && isCLUUserValue(evaluatedArguments[0])) {
			const instance = evaluatedArguments[0];

			return funDef.evaluate(globalInfo, instance.value, { cluster });
		} else if (isCLUSettorDefinition(funDef) && isCLUUserValue(evaluatedArguments[0])) {
			const settor = funDef;
			const instance = evaluatedArguments[0];

			settor.setValue = evaluatedArguments[1];
			return funDef.evaluate(globalInfo, instance.value, { cluster });
		} else {
			throw new Error('CLUOperatorUsage : Failed to evaluate; unrecognized type of funDef.');
		}
	}
}
