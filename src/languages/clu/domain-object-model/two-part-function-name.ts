// two-part-function-name.ts

export class TwoPartFunctionName extends OnePartFunctionName {
	public readonly string ClusterPart;

	public TwoPartFunctionName(string c, string f)
		: base(f)
	{
		ClusterPart = c;
	}

	public override string ToString()
	{
		return string.Format("{0}${1}", ClusterPart, FunctionPart);
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

		var otherTwoPartFunName = (TwoPartFunctionName)obj;

		return ClusterPart == otherTwoPartFunName.ClusterPart && FunctionPart == otherTwoPartFunName.FunctionPart;
	}

	public override int GetHashCode()
	{
		return ClusterPart.GetHashCode() * 101 + FunctionPart.GetHashCode();
	}
}
