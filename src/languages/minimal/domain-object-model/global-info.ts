// tom-weatherhead/thaw-grammar/src/languages/minimal/domain-object-model/global-info.ts

import { GlobalInfoBase } from '../../../common/domain-object-model/global-info-base';

export class MinimalLanguageGlobalInfo extends GlobalInfoBase<number> {
	private readonly trueValueForAccessor = 1;
	private readonly falseValueForAccessor = 0;

	constructor() {
		super();
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

	// eslint-disable-next-line @typescript-eslint/no-unused-vars
	public valueIsInteger(value: number): boolean {
		return true;
	}

	public valueAsInteger(value: number): number {
		return value;
	}

	public integerAsValue(value: number): number {
		return value;
	}
}
