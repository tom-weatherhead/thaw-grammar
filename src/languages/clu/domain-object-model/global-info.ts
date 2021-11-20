// clu/domain-object-model/global-info.ts

import { IParser, ITokenizer } from 'thaw-interpreter-types';

import { ArgumentException } from 'thaw-interpreter-core';

import { GlobalInfoBase } from '../../../common/domain-object-model/global-info-base';

import { ICLUGlobalInfo, ICluster, ICLUValue } from './interfaces/ivalue';

import { CLUPrimitiveValue, isCLUPrimitiveValue } from './data-types/primitive-value';

const typenameCLUGlobalInfo = 'CLUGlobalInfo';

export function isCLUGlobalInfo(obj: unknown): obj is ICLUGlobalInfo {
	const otherCLUGlobalInfo = obj as CLUGlobalInfo;

	return (
		typeof otherCLUGlobalInfo !== 'undefined' &&
		otherCLUGlobalInfo.typename === typenameCLUGlobalInfo
	);
}

export class CLUGlobalInfo
	extends GlobalInfoBase<ICLUValue>
	implements /* IGlobalInfoOps */ ICLUGlobalInfo
{
	public readonly typename: string = typenameCLUGlobalInfo;
	private readonly falseVal = new CLUPrimitiveValue(0);
	private readonly trueVal = new CLUPrimitiveValue(1);
	public readonly clusterDict = new Map<string, ICluster>();

	constructor(
		options: {
			parser?: IParser;
			tokenizer?: ITokenizer;
		} = {}
	) {
		super(options);
	}

	public override initialize(): void {
		super.initialize();

		this.clusterDict.clear();
	}

	// public override loadPresets(): void {
	// 	this.evaluate('(define > (x y) (< y x))');
	// }

	public get falseValue(): ICLUValue {
		return this.falseVal;
	}

	public get trueValue(): ICLUValue {
		return this.trueVal;
	}

	public override valueIsFalse(value: ICLUValue): boolean {
		// return value.equals(this.falseValue);

		return isCLUPrimitiveValue(value) && value.value === 0;
	}

	public valueIsInteger(value: ICLUValue): boolean {
		return isCLUPrimitiveValue(value);
	}

	public valueAsInteger(value: ICLUValue): number {
		if (!isCLUPrimitiveValue(value)) {
			throw new ArgumentException('ValueAsInteger() : value is not an integer', 'value');
		}

		return value.value;
	}

	public integerAsValue(value: number): ICLUValue {
		return new CLUPrimitiveValue(value);
	}
}
