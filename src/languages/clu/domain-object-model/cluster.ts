// clu/domain-object-model/cluster.ts

import { ICLUEnvironmentFrame, ICLUExpression, ICLUGlobalInfo, ICluster, ICLUValue } from './interfaces/ivalue';

export class Cluster implements ICLUExpression {
	public readonly string ClusterName;
	public readonly HashSet<string> ExportSet;
	public readonly List<CLUVariable> ClRep;
	private List<CLUFunctionDefinitionBase> FunDefList;
	public readonly Dictionary<string, CLUFunctionDefinitionBase> ExportedDict = new Dictionary<string, CLUFunctionDefinitionBase>();
	public readonly Dictionary<string, CLUFunctionDefinitionBase> NonExportedDict = new Dictionary<string, CLUFunctionDefinitionBase>();

	constructor(string name, HashSet<string> exportSet, List<CLUVariable> clRep, List<CLUFunctionDefinitionBase> funDefList)
	{
		ClusterName = name;
		ExportSet = exportSet;
		ClRep = clRep;
		FunDefList = funDefList;
	}

	public override bool Equals(object obj)
	{

		if (object.ReferenceEquals(this, obj))
		{
			return true;
		}

		var otherCluster = obj as Cluster;

		return otherCluster != null && ClusterName == otherCluster.ClusterName;
	}

	public override int GetHashCode()
	{
		return ClusterName.GetHashCode();
	}

	public evaluate(localEnvironment: ICLUEnvironmentFrame, cluster: ICluster, globalInfo: ICLUGlobalInfo): ICLUValue {
		globalInfo.ClusterDict[ClusterName] = this;

		// Make the constructor:
		NonExportedDict[ClusterName] = new CLUConstructorDefinition(ClusterName);

		// Make the selectors and settors:

		foreach (var memberVariable in ClRep)
		{
			NonExportedDict[memberVariable.Name] = new CLUSelectorDefinition(memberVariable.Name, memberVariable);
			NonExportedDict["set-" + memberVariable.Name] =
				new CLUSettorDefinition("set-" + memberVariable.Name, memberVariable);
		}

		foreach (var funDef in FunDefList)
		{

			if (ExportSet.Contains(funDef.FunctionName))
			{
				ExportedDict[funDef.FunctionName] = funDef;
			}
			else
			{
				NonExportedDict[funDef.FunctionName] = funDef;
			}
		}

		return globalInfo.TrueValue;
	}
}
