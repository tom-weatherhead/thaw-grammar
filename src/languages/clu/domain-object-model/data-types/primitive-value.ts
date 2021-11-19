// clu/domain-object-model/data-types/primitive-value.ts

import { IEnvironmentFrame } from '../../../../common/domain-object-model/environment-frame';

import { IGlobalInfo } from '../../../../common/domain-object-model/iglobal-info';

import {
	/* ICLUEnvironmentFrame, ICLUGlobalInfo, ICluster, */ ICLUValue
} from '../interfaces/ivalue';

const typenameCLUPrimitiveValue = 'CLUPrimitiveValue';

export function isCLUPrimitiveValue(obj: unknown): obj is CLUPrimitiveValue {
	const v = obj as CLUPrimitiveValue;

	return (
		typeof v !== 'undefined' &&
		typeof v.typename !== 'undefined' &&
		v.typename === typenameCLUPrimitiveValue
	);
}

export class CLUPrimitiveValue implements ICLUValue {
	public readonly typename: string = typenameCLUPrimitiveValue;

	constructor(public readonly value: number) {}

	public toString(): string {
		return this.value.toString();
	}

	public equals(other: unknown): boolean {
		return isCLUPrimitiveValue(other) && other.value === this.value;
	}

	// public evaluate(
	// 	// eslint-disable-next-line @typescript-eslint/no-unused-vars
	// 	localEnvironment: ICLUEnvironmentFrame,
	// 	// eslint-disable-next-line @typescript-eslint/no-unused-vars
	// 	cluster: ICluster | undefined,
	// 	// eslint-disable-next-line @typescript-eslint/no-unused-vars
	// 	globalInfo: ICLUGlobalInfo
	// ): ICLUValue {
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
