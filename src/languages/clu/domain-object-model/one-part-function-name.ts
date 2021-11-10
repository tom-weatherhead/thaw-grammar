// one-part-function-name.ts

import { ICLUEnvironmentFrame, ICLUExpression, ICLUGlobalInfo, ICluster, ICLUValue } from './interfaces/ivalue';

public class OnePartFunctionName : ICLUFunctionName
{
	public readonly string FunctionPart;

	public OnePartFunctionName(string f)
	{
		FunctionPart = f;
	}

	public override string ToString()
	{
		return FunctionPart;
	}

	public override bool Equals(object obj)
	{

		if (object.ReferenceEquals(this, obj))
		{
			return true;
		}

		if (obj == null || !GetType().Equals(obj.GetType()))
		{
			return false;
		}

		var otherOnePartFunName = (OnePartFunctionName)obj;

		return FunctionPart == otherOnePartFunName.FunctionPart;
	}

	public override int GetHashCode()
	{
		return FunctionPart.GetHashCode();
	}
}
