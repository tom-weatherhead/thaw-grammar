// clu/domain-object-model/data-types/user-value.ts

import { IEnvironmentFrame } from '../../../../common/domain-object-model/environment-frame';

import { IGlobalInfo } from '../../../../common/domain-object-model/iglobal-info';

import {
	ICLUEnvironmentFrame,
	/* ICLUGlobalInfo, */ ICluster,
	ICLUValue
} from '../interfaces/ivalue';

const typenameCLUUserValue = 'CLUUserValue';

export function isCLUUserValue(obj: unknown): obj is CLUUserValue {
	const v = obj as CLUUserValue;

	return (
		typeof v !== 'undefined' &&
		typeof v.typename !== 'undefined' &&
		v.typename === typenameCLUUserValue
	);
}

export class CLUUserValue implements ICLUValue {
	public readonly typename: string = typenameCLUUserValue;

	constructor(public readonly owner: ICluster, public readonly value: ICLUEnvironmentFrame) {}

	public toString(): string {
		// Output the values of the cluster instance variables, in the order in which they are declared in the cluster.

		return this.owner.clRep.map((v) => `${v} = ${this.value.lookup(v)}`).join('; ');
	}

	public equals(other: unknown): boolean {
		return (
			isCLUUserValue(other) &&
			other.owner.clusterName === this.owner.clusterName &&
			other.toString() === this.toString()
		);
	}

	// public evaluate(
	// 	// eslint-disable-next-line @typescript-eslint/no-unused-vars
	// 	localEnvironment: ICLUEnvironmentFrame,
	// 	// eslint-disable-next-line @typescript-eslint/no-unused-vars
	// 	cluster: ICluster | undefined,
	// 	// eslint-disable-next-line @typescript-eslint/no-unused-vars
	// 	globalInfo: ICLUGlobalInfo
	// ): ICLUValue {
	// 	return this;
	// }

	/* eslint-disable @typescript-eslint/no-unused-vars */
	public evaluate(
		globalInfo: IGlobalInfo<ICLUValue>,
		localEnvironment?: IEnvironmentFrame<ICLUValue>,
		options?: unknown
	): ICLUValue {
		return this;
	}
	/* eslint-enable @typescript-eslint/no-unused-vars */
}
