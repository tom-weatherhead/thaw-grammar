// clu/domain-object-model/operator-usage.ts

import { ICLUEnvironmentFrame, ICLUExpression, ICLUGlobalInfo, ICluster, ICLUValue } from './interfaces/ivalue';

export class FunctionNotExportedException extends Error {
	constructor(clusterName: string, functionName: string) {
		super(`The cluster '${clusterName}' does not export a function named '${functionName}'.`);
	}
}

export class CLUOperatorUsage implements ICLUExpression {
	public readonly List<ICLUExpression> ExpressionList;
	private readonly string ClusterName;
	private readonly string FunctionName;
	private readonly HashSet<string> BuiltInOperatorNames;

	public CLUOperatorUsage(ICLUFunctionName operatorName, List<ICLUExpression> expressionList)
	{
		ExpressionList = expressionList;

		if (operatorName is TwoPartFunctionName)
		{
			var o = (TwoPartFunctionName)operatorName;

			ClusterName = o.ClusterPart;
			FunctionName = o.FunctionPart;
		}
		else
		{
			var o = (OnePartFunctionName)operatorName;

			ClusterName = string.Empty;
			FunctionName = o.FunctionPart;
		}

		BuiltInOperatorNames = new HashSet<string>() { "+", "-", "*", "/", "=", "<", /* ">", */ "print" };
	}

	/*
	public override string ToString()
	{
		return string.Format("({0} {1})", OperatorName, ExpressionList);
	}
	 */

	protected bool TryGetExpectedNumArgs(CLUFunctionDefinitionBase funDef, Cluster cluster, CLUGlobalInfo globalInfo, out int result)
	{

		if (funDef == null)
		{

			switch (FunctionName)
			{
				case "print":
					result = 1;
					return true;

				case "+":
				case "-":
				case "*":
				case "/":
				case "=":
				case "<":
				//case ">":
					result = 2;
					return true;

				default:
					throw new Exception(string.Format("TryGetExpectedNumArgs() : Unknown built-in operator '{0}'.", FunctionName));
			}
		}

		if (funDef is CLUNormalFunctionDefinition)
		{
			var normalFunDef = (CLUNormalFunctionDefinition)funDef;

			result = normalFunDef.ArgList.Count;
			return true;
		}
		else if (funDef is CLUConstructorDefinition)
		{
			result = cluster.ClRep.Count;
			return true;
		}
		else if (funDef is CLUSelectorDefinition)
		{
			result = 1;
			return true;
		}
		else if (funDef is CLUSettorDefinition)
		{
			result = 2;
			return true;
		}
		else
		{
			throw new Exception(string.Format("TryGetExpectedNumArgs() : Unknown operator type '{0}'.", funDef.GetType().FullName));
		}
	}

	protected void CheckArgTypes(CLUFunctionDefinitionBase funDef, Cluster cluster, List<ICLUValue> evaluatedArguments)
	{

		if (funDef != null)
		{
			CLUVariable associatedVariable = null;

			if (funDef is CLUSelectorDefinition)
			{
				var selector = (CLUSelectorDefinition)funDef;

				associatedVariable = selector.AssociatedVariable;
			}
			else if (funDef is CLUSettorDefinition)
			{
				var settor = (CLUSettorDefinition)funDef;

				associatedVariable = settor.AssociatedVariable;
			}

			if (associatedVariable != null)
			{
				var userValue = evaluatedArguments[0] as CLUUserValue;

				if (userValue == null)
				{
					throw new Exception("Selector/settor arg type check: First arg is not a CLUUserValue.");
				}
				else if (!userValue.Value.Dict.ContainsKey(associatedVariable))
				{
					throw new Exception(string.Format("Selector/settor arg type check: First arg does not contain the member var {0}.", associatedVariable));
				}
			}

			return;
		}

		if (IntegerOperatorKeeper.TwoArgumentOperators.ContainsKey(FunctionName) ||
			IntegerOperatorKeeper.TwoArgumentPredicates.ContainsKey(FunctionName))
		{

			if (!(evaluatedArguments[0] is CLUPrimitiveValue))
			{
				throw new ArgumentException(string.Format("Operator {0} : First argument is not a number.", FunctionName));
			}

			if (!(evaluatedArguments[1] is CLUPrimitiveValue))
			{
				throw new ArgumentException(string.Format("Operator {0} : Second argument is not a number.", FunctionName));
			}
		}
	}

	protected ICLUValue EvaluateNormal(CLUNormalFunctionDefinition funDef, List<ICLUValue> evaluatedArguments, Cluster cluster, CLUGlobalInfo globalInfo)
	{
		int firstArgAsInt = (evaluatedArguments.Count > 0 && globalInfo.ValueIsInteger(evaluatedArguments[0]))
			? globalInfo.ValueAsInteger(evaluatedArguments[0]) : 0;
		int secondArgAsInt = (evaluatedArguments.Count > 1 && globalInfo.ValueIsInteger(evaluatedArguments[1]))
			? globalInfo.ValueAsInteger(evaluatedArguments[1]) : 0;

		if (cluster == null)
		{

			if (IntegerOperatorKeeper.TwoArgumentOperators.ContainsKey(FunctionName))
			{
				return globalInfo.IntegerAsValue(IntegerOperatorKeeper.TwoArgumentOperators[FunctionName](firstArgAsInt, secondArgAsInt));
			}
			else if (IntegerOperatorKeeper.TwoArgumentPredicates.ContainsKey(FunctionName))
			{
				return IntegerOperatorKeeper.TwoArgumentPredicates[FunctionName](firstArgAsInt, secondArgAsInt) ? globalInfo.TrueValue : globalInfo.FalseValue;
			}

			switch (FunctionName)
			{
				case "=":
					return evaluatedArguments[0].Equals(evaluatedArguments[1]) ? globalInfo.TrueValue : globalInfo.FalseValue;

				case "print":
					Console.WriteLine(evaluatedArguments[0]);
					return evaluatedArguments[0];

				default:
					break;
			}
		}

		// Evaluate a user-defined function.
		var newEnvironment = new CLUEnvironmentFrame(globalInfo.GlobalEnvironment);

		newEnvironment.Compose(funDef.ArgList, evaluatedArguments);
		return funDef.Body.Evaluate(newEnvironment, cluster, globalInfo);
	}

	public evaluate(localEnvironment: ICLUEnvironmentFrame, cluster: ICluster, globalInfo: ICLUGlobalInfo): ICLUValue {
		Cluster originalCluster = cluster;
		CLUFunctionDefinitionBase funDef = null;

		if (!string.IsNullOrEmpty(ClusterName))
		{

			if (!globalInfo.ClusterDict.ContainsKey(ClusterName))
			{
				throw new Exception(string.Format("CLUOperatorUsage.Evaluate() : Unknown cluster '{0}'.", ClusterName));
			}

			cluster = globalInfo.ClusterDict[ClusterName];

			if (!cluster.ExportedDict.ContainsKey(FunctionName))
			{
				//throw new Exception(string.Format("CLUOperatorUsage.Evaluate() : Cluster '{0}' does not contain an exported function named '{1}'.", ClusterName, FunctionName));
				throw new FunctionNotExportedException(ClusterName, FunctionName);
			}

			funDef = cluster.ExportedDict[FunctionName];
		}
		else if (cluster == null)
		{

			if (BuiltInOperatorNames.Contains(FunctionName))
			{
				funDef = null;
			}
			else
			{

				if (!globalInfo.FunctionDefinitions.ContainsKey(FunctionName))
				{
					throw new Exception(string.Format("CLUOperatorUsage.Evaluate() : Unknown global function '{0}'.", FunctionName));
				}

				funDef = globalInfo.FunctionDefinitions[FunctionName];
			}
		}
		else if (cluster.ExportedDict.ContainsKey(FunctionName))
		{
			funDef = cluster.ExportedDict[FunctionName];
		}
		else if (cluster.NonExportedDict.ContainsKey(FunctionName))
		{
			funDef = cluster.NonExportedDict[FunctionName];
		}
		else
		{
			cluster = null;

			if (BuiltInOperatorNames.Contains(FunctionName))
			{
				funDef = null;
			}
			else
			{

				if (!globalInfo.FunctionDefinitions.ContainsKey(FunctionName))
				{
					throw new Exception(string.Format("CLUOperatorUsage.Evaluate() : Unknown global function '{0}'.", FunctionName));
				}

				funDef = globalInfo.FunctionDefinitions[FunctionName];
			}
		}

		// At this point, funDef == null means that it's a built-in operator.

		var actualNumArgs = ExpressionList.Count;
		int expectedNumArgs;

		if (!TryGetExpectedNumArgs(funDef, cluster, globalInfo, out expectedNumArgs))
		{
			throw new Exception(string.Format("CLUOperatorUsage : Unknown operator name '{0}'.", FunctionName));
		}
		else if (actualNumArgs != expectedNumArgs)
		{
			throw new Exception(string.Format("CLUOperatorUsage : Expected {0} arguments for operator '{1}', instead of the actual {2} arguments.",
				expectedNumArgs, FunctionName, actualNumArgs));
		}

		// originalCluster
		//List<ICLUValue> evaluatedArguments = ExpressionList.Select(expr => expr.Evaluate(localEnvironment, cluster, globalInfo)).ToList();
		List<ICLUValue> evaluatedArguments = ExpressionList.Select(expr => expr.Evaluate(localEnvironment, originalCluster, globalInfo)).ToList();

		CheckArgTypes(funDef, cluster, evaluatedArguments);

		if (funDef == null || funDef is CLUNormalFunctionDefinition)
		{
			return EvaluateNormal((CLUNormalFunctionDefinition)funDef, evaluatedArguments, cluster, globalInfo);
		}
		else if (funDef is CLUConstructorDefinition)
		{
			var newEnvironment = new CLUEnvironmentFrame(globalInfo.GlobalEnvironment);

			newEnvironment.Compose(cluster.ClRep, evaluatedArguments);
			return new CLUUserValue(cluster, newEnvironment);
		}
		else if (funDef is CLUSelectorDefinition)
		{
			var instance = (CLUUserValue)evaluatedArguments[0];

			return funDef.Evaluate(instance.Value, cluster, globalInfo);
		}
		else if (funDef is CLUSettorDefinition)
		{
			var settor = (CLUSettorDefinition)funDef;
			var instance = (CLUUserValue)evaluatedArguments[0];

			settor.SetValue = evaluatedArguments[1];
			return funDef.Evaluate(instance.Value, cluster, globalInfo);
		}
		else
		{
			throw new Exception("CLUOperatorUsage : Failed to evaluate; unrecognized type of funDef.");
		}
	}
}
