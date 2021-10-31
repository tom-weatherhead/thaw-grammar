// tom-weatherhead/thaw-grammar/src/languages/smalltalk/domain-object-model/global-info.ts

import { GlobalInfoBase } from '../../../common/domain-object-model/global-info-base';

import { ArgumentException } from '../../../common/exceptions/argument-exception';

import { ISmalltalkValue } from './interfaces/ivalue';

import { SmalltalkIntegerValue } from './integer';

export class SmalltalkGlobalInfo extends GlobalInfoBase<ISmalltalkValue> {
	private readonly falseValueForAccessor = new SmalltalkIntegerValue(0);
	private readonly trueValueForAccessor = new SmalltalkIntegerValue(1);

	public get falseValue(): ISmalltalkValue {
		return this.falseValueForAccessor;
	}

	public get trueValue(): ISmalltalkValue {
		return this.trueValueForAccessor;
	}

	public override valueIsFalse(value: ISmalltalkValue): boolean {
		return value.toInteger() === 0;
	}

	public valueIsInteger(value: ISmalltalkValue): boolean {
		return value.isInteger;
	}

	public valueAsInteger(value: ISmalltalkValue): number {
		const valueAsNumber = value.toInteger();

		if (valueAsNumber === undefined) {
			throw new ArgumentException(
				'valueAsInteger() : The value is not an IntegerLiteral.',
				'valueAsNumber'
			);
		}

		return valueAsNumber;
	}

	public integerAsValue(value: number): ISmalltalkValue {
		return new SmalltalkIntegerValue(value);
	}
}
