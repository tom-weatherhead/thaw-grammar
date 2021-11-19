// clu/domain-object-model/global-info.ts

import { IParser, ITokenizer } from 'thaw-interpreter-types';

import { ArgumentException } from 'thaw-interpreter-core';

import { GlobalInfoBase } from '../../../common/domain-object-model/global-info-base';

import {
	// ICLUEnvironmentFrame,
	// ICLUExpression,
	ICLUGlobalInfo,
	ICluster,
	ICLUValue
} from './interfaces/ivalue';

// import { CLUEnvironmentFrame } from './environment-frame';

// import { CLUNormalFunctionDefinition } from './normal-function-definition';

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
	// private readonly ITokenizer Tokenizer;
	// private readonly IParser Parser;
	private readonly falseVal = new CLUPrimitiveValue(0);
	private readonly trueVal = new CLUPrimitiveValue(1);
	// public readonly globalEnvironment = new CLUEnvironmentFrame();
	// public readonly functionDefinitions = new Map<string, CLUNormalFunctionDefinition>();
	public readonly clusterDict = new Map<string, ICluster>();

	constructor(
		options: {
			parser?: IParser;
			tokenizer?: ITokenizer;
		} = {}
	) {
		super(options);
	}

	// public void Clear()
	// {
	// 	GlobalEnvironment.Dict.Clear();
	// 	FunctionDefinitions.Clear();
	// 	ClusterDict.Clear();
	// }

	// public string LoadPreset(string presetName)
	// {
	// 	throw new Exception(string.Format("LoadPreset() : Unknown preset name '{0}'.", presetName));
	// }

	// private void Evaluate(string input)
	// {
	// 	var expr = Parser.Parse(Tokenizer.Tokenize(input)) as ICLUExpression;
	//
	// 	if (expr == null)
	// 	{
	// 		throw new Exception(string.Format("CLUGlobalInfo.Evaluate() : Parse failed; input is: {0}", input));
	// 	}
	//
	// 	expr.Evaluate(GlobalEnvironment, null, this);
	// }

	// public void LoadPresets()
	// {
	// 	Evaluate("(define > (x y) (< y x))");
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

		// var prim = (CLUPrimitiveValue)value;

		// return prim.Value;

		return value.value;
	}

	public integerAsValue(value: number): ICLUValue {
		return new CLUPrimitiveValue(value);
	}

	// public setScoping(dynamicScoping: boolean): boolean {
	// 	return false;
	// }
	//
	// public setDebug(debug: boolean): boolean {
	// 	return false;
	// }

	// public override evaluate(expr: ICLUExpression): ICLUValue {
	// 	return expr.evaluate(this, this.globalEnvironment);
	// }
	// public override evaluate(str: string): ICLUValue {
	// 	if (typeof this.tokenizer === 'undefined') {
	// 		throw new Error('GlobalInfoBase.evaluate() : this.tokenizer is undefined.');
	// 	} else if (typeof this.parser === 'undefined') {
	// 		throw new Error('GlobalInfoBase.evaluate() : this.parser is undefined.');
	// 	}
	//
	// 	const parseResult = this.parser.parse(this.tokenizer.tokenize(str));
	// 	const expr = parseResult as IExpression<T>;
	//
	// 	return expr.evaluate(this, this.globalEnvironment);
	// }
}
