// clu/domain-object-model/data-types/user-value.ts

import { ICLUEnvironmentFrame, ICLUGlobalInfo, ICluster, ICLUValue } from '../interfaces/ivalue';

export class CLUUserValue implements ICLUValue {
	// public readonly owner: ICluster;
	// public readonly value: ICLUEnvironmentFrame;

	constructor(public readonly owner: ICluster, public readonly value: ICLUEnvironmentFrame) {
		// Owner = owner;
		// Value = environmentFrame;
	}

	public toString(): string {
		// TODO: Output the values of the cluster instance variables, in the order in which they are declared in the cluster.
		//return "CLUUserValue: " + Value.ToString();

		// return string.Join("\r\n", Owner.ClRep.Select(v => Value.Lookup(v)));

		return '<CLUUserValue>';
	}

	public evaluate(localEnvironment: ICLUEnvironmentFrame, cluster: ICluster, globalInfo: ICLUGlobalInfo): ICLUValue {
		return this;
	}
}
