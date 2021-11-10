// clu/domain-object-model/operator-usage.ts

import { ArgumentException } from 'thaw-interpreter-core';

import {
	ICLUEnvironmentFrame,
	ICLUExpression,
	ICLUFunctionName,
	ICLUGlobalInfo,
	ICluster,
	ICLUValue,
	ICLUVariable
} from './interfaces/ivalue';

import { isCLUPrimitiveValue } from './data-types/primitive-value';

import { CLUUserValue, isCLUUserValue } from './data-types/user-value';

import { isCLUConstructorDefinition } from './constructor-definition';

import { CLUEnvironmentFrame } from './environment-frame';

import { CLUFunctionDefinitionBase } from './function-definition-base';

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
	// public readonly expressionList: ICLUExpression[];
	private readonly clusterName: string;
	private readonly functionName: string;
	// private readonly builtInOperatorNames: string[];

	constructor(operatorName: ICLUFunctionName, public readonly expressionList: ICLUExpression[]) {
		// ExpressionList = expressionList;

		if (isTwoPartFunctionName(operatorName)) {
			// var o = (TwoPartFunctionName)operatorName;

			this.clusterName = operatorName.clusterPart;
			this.functionName = operatorName.functionPart;
		} else if (isOnePartFunctionName(operatorName)) {
			// var o = (OnePartFunctionName)operatorName;

			this.clusterName = '';
			this.functionName = operatorName.functionPart;
		} else {
			throw new Error('CLUOperatorUsage constructor');
		}

		// BuiltInOperatorNames = new HashSet<string>() { "+", "-", "*", "/", "=", "<", /* ">", */ "print" };
	}

	/*
	public override string ToString()
	{
		return string.Format("({0} {1})", OperatorName, ExpressionList);
	}
	 */

	protected tryGetExpectedNumArgs(
		funDef: CLUFunctionDefinitionBase | undefined,
		cluster: ICluster,
		globalInfo: ICLUGlobalInfo
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
			// var normalFunDef = (CLUNormalFunctionDefinition)funDef;

			return funDef.argList.length;
		} else if (isCLUConstructorDefinition(funDef)) {
			return cluster.clRep.length;
		} else if (isCLUSelectorDefinition(funDef)) {
			return 1;
		} else if (isCLUSettorDefinition(funDef)) {
			return 2;
		} else {
			throw new Error('TryGetExpectedNumArgs() : Unknown operator type.');
		}
	}

	protected checkArgTypes(
		funDef: CLUFunctionDefinitionBase,
		cluster: ICluster,
		evaluatedArguments: ICLUValue[]
	): void {
		if (typeof funDef !== 'undefined') {
			let associatedVariable: ICLUVariable | undefined;

			if (isCLUSelectorDefinition(funDef)) {
				// var selector = (CLUSelectorDefinition)funDef;

				associatedVariable = funDef.associatedVariable;
			} else if (isCLUSettorDefinition(funDef)) {
				// var settor = (CLUSettorDefinition)funDef;

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
		funDef: CLUNormalFunctionDefinition,
		evaluatedArguments: ICLUValue[],
		cluster: ICluster,
		globalInfo: ICLUGlobalInfo
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
				// case "=":
				// 	return evaluatedArguments[0].equals(evaluatedArguments[1]) ? globalInfo.trueValue : globalInfo.falseValue;

				case 'print':
					console.log(evaluatedArguments[0]);

					return evaluatedArguments[0];

				default:
					break;
			}
		}

		// Evaluate a user-defined function.
		const newEnvironment = new CLUEnvironmentFrame(globalInfo.globalEnvironment);

		newEnvironment.compose(funDef.argList, evaluatedArguments);

		return funDef.body.evaluate(newEnvironment, cluster, globalInfo);
	}

	public evaluate(
		localEnvironment: ICLUEnvironmentFrame,
		cluster: ICluster,
		globalInfo: ICLUGlobalInfo
	): ICLUValue {
		const originalCluster = cluster;
		let funDef: CLUFunctionDefinitionBase | undefined;

		if (this.clusterName !== '') {
			if (!globalInfo.clusterDict.has(this.clusterName)) {
				throw new Error(
					`CLUOperatorUsage.evaluate() : Unknown cluster '${this.clusterName}'.`
				);
			}

			cluster = globalInfo.clusterDict[this.clusterName];

			if (!cluster.exportedDict.has(this.functionName)) {
				//throw new Exception(string.Format("CLUOperatorUsage.Evaluate() : Cluster '{0}' does not contain an exported function named '{1}'.", ClusterName, FunctionName));
				throw new FunctionNotExportedException(this.clusterName, this.functionName);
			}

			funDef = cluster.exportedDict[this.functionName];
		} else if (cluster == null) {
			if (builtInOperatorNames.indexOf(this.functionName) >= 0) {
				funDef = null;
			} else {
				if (!globalInfo.functionDefinitions.has(this.functionName)) {
					throw new Error(
						`CLUOperatorUsage.evaluate() : Unknown global function '${this.functionName}'.`
					);
				}

				funDef = globalInfo.functionDefinitions[this.functionName];
			}
		} else if (cluster.exportedDict.has(this.functionName)) {
			funDef = cluster.exportedDict[this.functionName];
		} else if (cluster.nonExportedDict.has(this.functionName)) {
			funDef = cluster.nonExportedDict[this.functionName];
		} else {
			cluster = null;

			if (builtInOperatorNames.indexOf(this.functionName) >= 0) {
				funDef = null;
			} else {
				if (!globalInfo.functionDefinitions.has(this.functionName)) {
					throw new Error(
						`CLUOperatorUsage.evaluate() : Unknown global function '${this.functionName}'.`
					);
				}

				funDef = globalInfo.functionDefinitions[this.functionName];
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

		// originalCluster
		//List<ICLUValue> evaluatedArguments = ExpressionList.Select(expr => expr.Evaluate(localEnvironment, cluster, globalInfo)).ToList();

		const evaluatedArguments = this.expressionList.map((expr) =>
			expr.evaluate(localEnvironment, originalCluster, globalInfo)
		);

		this.checkArgTypes(funDef, cluster, evaluatedArguments);

		if (/* funDef == null || */ isCLUNormalFunctionDefinition(funDef)) {
			return this.evaluateNormal(funDef, evaluatedArguments, cluster, globalInfo);
		} else if (isCLUConstructorDefinition(funDef)) {
			const newEnvironment = new CLUEnvironmentFrame(globalInfo.globalEnvironment);

			newEnvironment.compose(cluster.clRep, evaluatedArguments);
			return new CLUUserValue(cluster, newEnvironment);
		} else if (isCLUSelectorDefinition(funDef) && isCLUUserValue(evaluatedArguments[0])) {
			const instance = evaluatedArguments[0];

			return funDef.evaluate(instance.value, cluster, globalInfo);
		} else if (isCLUSettorDefinition(funDef) && isCLUUserValue(evaluatedArguments[0])) {
			const settor = funDef;
			const instance = evaluatedArguments[0];

			settor.setValue = evaluatedArguments[1];
			return funDef.evaluate(instance.value, cluster, globalInfo);
		} else {
			throw new Error('CLUOperatorUsage : Failed to evaluate; unrecognized type of funDef.');
		}
	}
}
