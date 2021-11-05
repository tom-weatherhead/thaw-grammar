// tom-weatherhead/thaw-grammar/src/languages/smalltalk/domain-object-model/block.ts

// A SmalltalkBlock is a lot like an SASL Thunk (a suspended computation).

// SmalltalkBlock objects are immutable.

import {
	ISmalltalkClass,
	ISmalltalkEnvironmentFrame,
	ISmalltalkExpression,
	ISmalltalkGlobalInfo,
	ISmalltalkValue
} from '../interfaces/iexpression';

import { objectClass } from '../bootstrap';

import { SmalltalkValueBase } from './base';

const typenameSmalltalkBlock = 'SmalltalkBlock';

export function isSmalltalkBlock(obj: unknown): obj is SmalltalkBlock {
	const v = obj as SmalltalkBlock;

	return (
		typeof v !== 'undefined' &&
		typeof v.typename !== 'undefined' &&
		v.typename === typenameSmalltalkBlock
	);
}

export function unblockValue(value: ISmalltalkValue): ISmalltalkValue {
	if (isSmalltalkBlock(value)) {
		// I.e. value is a SmalltalkBlock (a suspended computation)
		return value.unblock();
	} else {
		return value;
	}
}

export class SmalltalkBlock extends SmalltalkValueBase {
	public readonly typename: string = typenameSmalltalkBlock;

	constructor(
		public readonly expression: ISmalltalkExpression,
		public readonly localEnvironment: ISmalltalkEnvironmentFrame | undefined,
		public readonly receiver: ISmalltalkValue,
		public readonly classX: ISmalltalkClass | undefined,
		public readonly globalInfo: ISmalltalkGlobalInfo
	) {
		super(objectClass);
	}

	public override toString(): string {
		return '<block>';
	}

	// eslint-disable-next-line @typescript-eslint/no-unused-vars
	public equals(other: unknown): boolean {
		return false;
	}

	public override getTypename(): string {
		return 'block';
	}

	public unblock(): ISmalltalkValue {
		// eslint-disable-next-line @typescript-eslint/no-this-alias
		let result: ISmalltalkValue = this;

		for (;;) {
			const block = result as SmalltalkBlock;

			if (!isSmalltalkBlock(block)) {
				break;
			}

			result = block.expression.evaluate(
				block.localEnvironment,
				block.receiver,
				block.classX,
				block.globalInfo
			);
		}

		return result;
	}
}
