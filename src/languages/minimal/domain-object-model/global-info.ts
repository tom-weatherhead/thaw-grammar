// import { ITokenizer } from 'thaw-lexical-analyzer';

import { GlobalInfoBase } from '../../../common/domain-object-model/global-info-base';
// import { ArgumentException } from '../../../common/exceptions/argument-exception';
// import { IParser } from '../../common/parser/iparser';
// import { IntegerLiteral } from './integer-literal';

export class MinimalLanguageGlobalInfo extends GlobalInfoBase<number> {
	private readonly trueValueForAccessor = 1;
	private readonly falseValueForAccessor = 0;

	// constructor(tokenizer: ITokenizer, parser: IParser) {
	// 	super(tokenizer, parser);
	// }

	constructor() {
		super();
	}

	public get falseValue(): number {
		return this.falseValueForAccessor;
	}

	public get trueValue(): number {
		return this.trueValueForAccessor;
	}

	public valueIsFalse(value: number): boolean {
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
