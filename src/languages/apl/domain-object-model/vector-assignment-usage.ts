// thaw-grammar/src/languages/apl/domain-object-model/vector-assignment-usage.ts

import { EnvironmentFrame } from '../../../common/domain-object-model/environment-frame';

import { IExpression } from '../../../common/domain-object-model/iexpression';

import { IGlobalInfo } from '../../../common/domain-object-model/iglobal-info';

import { Variable } from '../../../common/domain-object-model/variable';

import { IAPLValue } from './interfaces/ivalue';

import { APLValue } from './data-types/value';

export class VectorAssignmentUsage implements IExpression<IAPLValue> {
	// public readonly Variable<IAPLValue> VariableName;
	// public readonly IExpression<IAPLValue> IndicesExpression;
	// public readonly IExpression<IAPLValue> ValuesExpression;

	constructor(
		public readonly variableName: Variable<IAPLValue>,
		public readonly indicesExpression: IExpression<IAPLValue>,
		public readonly valuesExpression: IExpression<IAPLValue>
	) {
		// VariableName = variableName;
		// IndicesExpression = indicesExpression;
		// ValuesExpression = valuesExpression;
	}

	public toString(): string {
		// return string.Format("(:= {0} {1} {2})", VariableName, IndicesExpression, ValuesExpression);

		return '<VectorAssignmentUsage>';
	}

	private assignVectorElement(v: number[], i: number, x: number): void {
		if (i < 1 || i > v.length) {
			throw new Error(`:= : Index ${i} is not in the range from 1 to ${v.length}.`);
		}

		v[i - 1] = x;
	}

	public evaluateHelper(
		vectorValue: IAPLValue,
		indicesValue: IAPLValue,
		valuesValue: IAPLValue
	): IAPLValue {
		const newIntList = (vectorValue as APLValue).scalars.slice(0);

		if (indicesValue.isIntegerScalar && valuesValue.isScalar) {
			const indexScalar = indicesValue as APLValue;
			const valueScalar = valuesValue as APLValue;

			this.assignVectorElement(
				newIntList,
				indexScalar.getFirstScalar(),
				valueScalar.getFirstScalar()
			);
		} else if (indicesValue.isIntegerVector && valuesValue.isVector) {
			const indicesVector = (indicesValue as APLValue).scalars;
			const valuesVector = (valuesValue as APLValue).scalars;

			if (indicesVector.length !== valuesVector.length) {
				throw new Error(
					`:= : Indices vector has length ${indicesVector.length}; values vector has length ${valuesVector.length}.`
				);
			}

			for (let i = 0; i < indicesVector.length; ++i) {
				this.assignVectorElement(newIntList, indicesVector[i], valuesVector[i]);
			}
		} else {
			// throw new Error(`:= : Vector has type {0}; indices have type {1}; values have type {2}.`,
			// 	vectorValue.GetType().FullName, indicesValue.GetType().FullName, valuesValue.GetType().FullName));
			throw new Error(':= : Bad argument types.');
		}

		return APLValue.createVector1(newIntList);
	}

	public evaluate(
		localEnvironment: EnvironmentFrame<IAPLValue>,
		globalInfo: IGlobalInfo<IAPLValue>
	): IAPLValue {
		let vectorValue = this.variableName.evaluate(localEnvironment, globalInfo);
		const indicesValue = this.indicesExpression.evaluate(localEnvironment, globalInfo);
		const valuesValue = this.valuesExpression.evaluate(localEnvironment, globalInfo);

		// if (vectorValue is APLValue<int>)
		// {
		// 	vectorValue = EvaluateHelper((APLValue<int>)vectorValue, indicesValue, valuesValue);
		// }
		// else if (vectorValue is APLValue<double>)
		// {
		// 	vectorValue = EvaluateHelper((APLValue<double>)vectorValue, indicesValue, valuesValue);
		// }
		// else
		// {
		// 	throw new Exception(":= : v is not a vector");
		// }
		vectorValue = this.evaluateHelper(vectorValue, indicesValue, valuesValue);

		// If the variable is not already defined in the local env, we may have to assign it to the global env (assuming that there are only two envs).
		localEnvironment.addBubbleDown(this.variableName, vectorValue);

		return vectorValue;
	}
}
