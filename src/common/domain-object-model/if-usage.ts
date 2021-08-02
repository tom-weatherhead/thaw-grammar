// tom-weatherhead/thaw-grammar/src/common/domain-object-model/if-usage.ts

import { EnvironmentFrame } from './environment-frame';
import { IExpression } from './iexpression';
import { IGlobalInfo } from './iglobal-info';

export class IfUsage<T> implements IExpression<T> {
	public readonly condition: IExpression<T>;
	public readonly ifBody: IExpression<T>;
	public readonly elseBody: IExpression<T>;

	constructor(condition: IExpression<T>, ifBody: IExpression<T>, elseBody: IExpression<T>) {
		// console.log(`IfUsage 1: condition is ${condition}`);
		// console.log(`IfUsage 2: ifBody is ${ifBody}`);
		// console.log(`IfUsage 3: elseBody is ${elseBody}`);
		this.condition = condition;
		this.ifBody = ifBody;
		this.elseBody = elseBody;
	}

	public toString(): string {
		return `(if ${this.condition} ${this.ifBody} ${this.elseBody})`;
	}

	public evaluate(localEnvironment: EnvironmentFrame<T>, globalInfo: IGlobalInfo<T>): T {
		const conditionValue = this.condition.evaluate(localEnvironment, globalInfo);

		// console.log(`IfUsage.evaluate() 1: conditionValue is ${typeof conditionValue} ${conditionValue}`);
		// console.log(`IfUsage.evaluate() 2: globalInfo.falseValue is ${typeof globalInfo.falseValue} ${globalInfo.falseValue}`);
		// console.log(`IfUsage.evaluate() 3: conditionValue is false? ${globalInfo.valueIsFalse(conditionValue)}`);

		if (!globalInfo.valueIsFalse(conditionValue)) {
			return this.ifBody.evaluate(localEnvironment, globalInfo);
		} else {
			return this.elseBody.evaluate(localEnvironment, globalInfo);
		}
	}
}
