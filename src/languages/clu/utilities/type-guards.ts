// tom-weatherhead/thaw-grammar/src/languages/clu/utilities/type-guards.ts

import { ICluEvaluateOptions, ICluster } from '../domain-object-model/interfaces/ivalue';

export const typenameCluster = 'Cluster';

export function isCluster(obj: unknown): obj is ICluster {
	const other = obj as ICluster;

	return typeof other !== 'undefined' && other.typename === typenameCluster;
}

export function isCluEvaluateOptions(obj: unknown): obj is ICluEvaluateOptions {
	const other = obj as ICluEvaluateOptions;

	return (
		typeof other !== 'undefined' &&
		(typeof other.cluster === 'undefined' || isCluster(other.cluster))
	);
}
