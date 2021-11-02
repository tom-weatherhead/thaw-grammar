// block.ts

// A SmalltalkBlock is a lot like an SASL Thunk (a suspended computation).

// SmalltalkBlock objects are immutable.

import {
	ISmalltalkClass,
	ISmalltalkEnvironmentFrame,
	ISmalltalkExpression,
	// ISmalltalkFunctionDefinition,
	ISmalltalkGlobalInfo,
	ISmalltalkValue
} from './interfaces/iexpression';

// import { SmalltalkEnvironmentFrame } from './environment-frame';

import { objectClass } from './global-info';

import { SmalltalkValueBase } from './value-base';

const typenameSmalltalkBlock = 'SmalltalkBlock';

export function isSmalltalkBlock(obj: unknown): obj is SmalltalkBlock {
	const v = obj as SmalltalkBlock;

	return (
		typeof v !== 'undefined' &&
		typeof v.typename !== 'undefined' &&
		v.typename === typenameSmalltalkBlock
	);
}

export class SmalltalkBlock extends SmalltalkValueBase {
	public readonly typename: string = typenameSmalltalkBlock;
	// public readonly ISmalltalkExpression expression;
	// public readonly ISmalltalkEnvironmentFrame localEnvironment;
	// public readonly ISmalltalkValue receiver;
	// public readonly ISmalltalkClass classX;
	// public readonly ISmalltalkGlobalInfo globalInfo;

	constructor(
		public readonly expression: ISmalltalkExpression,
		public readonly localEnvironment: ISmalltalkEnvironmentFrame | undefined,
		public readonly receiver: ISmalltalkValue,
		public readonly classX: ISmalltalkClass | undefined,
		public readonly globalInfo: ISmalltalkGlobalInfo
	) {
		super(objectClass);

		// Expression = expression;
		// LocalEnvironment = localEnvironment;
		// Receiver = receiver;
		// Class = c;
		// GlobalInfo = globalInfo;
	}

	public override toString(): string {
		return '<block>';
	}

	// public override bool Equals(object obj)
	// {
	//     return object.ReferenceEquals(this, obj);
	// }

	// public override int GetHashCode()
	// {
	//     return 0;
	// }

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
