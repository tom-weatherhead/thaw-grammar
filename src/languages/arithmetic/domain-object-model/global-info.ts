// tom-weatherhead/thaw-grammar/src/languages/arithmetic/domain-object-model/global-info.ts

import { IParser, ITokenizer } from 'thaw-interpreter-types';

import { GlobalInfoBase } from '../../../common/domain-object-model/global-info-base';

export class ArithmeticGlobalInfo extends GlobalInfoBase<number> {
	private readonly trueValueForAccessor = 1;
	private readonly falseValueForAccessor = 0;

	constructor(
		options: {
			parser?: IParser;
			tokenizer?: ITokenizer;
		} = {}
	) {
		super(options);
	}

	public get falseValue(): number {
		return this.falseValueForAccessor;
	}

	public get trueValue(): number {
		return this.trueValueForAccessor;
	}

	public override valueIsFalse(value: number): boolean {
		return value === this.falseValue;
	}

	public valueIsInteger(value: number): boolean {
		return Number.isInteger(value);
	}

	public valueAsInteger(value: number): number {
		return Math.round(value);
	}

	public integerAsValue(value: number): number {
		return value;
	}
}
