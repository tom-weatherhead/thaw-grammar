// clu/domain-object-model/global-info.ts

import { ICLUEnvironmentFrame, ICLUExpression, ICLUGlobalInfo, ICluster, ICLUValue } from './interfaces/ivalue';

export class CLUGlobalInfo implements /* IGlobalInfoOps */ ICLUGlobalInfo {
	private readonly ITokenizer Tokenizer;
	private readonly IParser Parser;
	private readonly ICLUValue FalseVal = new CLUPrimitiveValue(0);
	private readonly ICLUValue TrueVal = new CLUPrimitiveValue(1);
	public readonly CLUEnvironmentFrame GlobalEnvironment = new CLUEnvironmentFrame(null);
	public readonly Dictionary<string, CLUNormalFunctionDefinition> FunctionDefinitions = new Dictionary<string, CLUNormalFunctionDefinition>();
	public readonly Dictionary<string, Cluster> ClusterDict = new Dictionary<string, Cluster>();

	constructor(ITokenizer t, IParser p) {
		Tokenizer = t;
		Parser = p;
	}

	public void Clear()
	{
		GlobalEnvironment.Dict.Clear();
		FunctionDefinitions.Clear();
		ClusterDict.Clear();
	}

	public string LoadPreset(string presetName)
	{
		throw new Exception(string.Format("LoadPreset() : Unknown preset name '{0}'.", presetName));
	}

	private void Evaluate(string input)
	{
		var expr = Parser.Parse(Tokenizer.Tokenize(input)) as ICLUExpression;

		if (expr == null)
		{
			throw new Exception(string.Format("CLUGlobalInfo.Evaluate() : Parse failed; input is: {0}", input));
		}

		expr.Evaluate(GlobalEnvironment, null, this);
	}

	public void LoadPresets()
	{
		Evaluate("(define > (x y) (< y x))");
	}

	public ICLUValue FalseValue
	{
		get
		{
			return FalseVal;
		}
	}

	public ICLUValue TrueValue
	{
		get
		{
			return TrueVal;
		}
	}

	public bool ValueIsFalse(ICLUValue value)
	{
		return value.Equals(FalseValue);
	}

	public bool ValueIsInteger(ICLUValue value)
	{
		return value is CLUPrimitiveValue;
	}

	public int ValueAsInteger(ICLUValue value)
	{

		if (!ValueIsInteger(value))
		{
			throw new ArgumentException("ValueAsInteger() : value is not an integer");
		}

		var prim = (CLUPrimitiveValue)value;

		return prim.Value;
	}

	public ICLUValue IntegerAsValue(int value)
	{
		return new CLUPrimitiveValue(value);
	}

	public bool SetScoping(bool dynamicScoping)
	{
		return false;
	}

	public bool SetDebug(bool debug)
	{
		return false;
	}
}
