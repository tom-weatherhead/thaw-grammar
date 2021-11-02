// set-usage.ts

// public class SmalltalkSetUsage : ISmalltalkExpression
// {
//     public readonly SmalltalkVariable VariableName;
//     public readonly ISmalltalkExpression Expression;
//
//     public SmalltalkSetUsage(SmalltalkVariable variableName, ISmalltalkExpression expression)
//     {
//         VariableName = variableName;
//         Expression = expression;
//     }
//
//     /*
//     public override string ToString()
//     {
//         return string.Format("(set {0} {1})", VariableName, Expression);
//     }
//      */
//
//     public ISmalltalkValue Evaluate(SmalltalkEnvironmentFrame localEnvironment, ISmalltalkValue receiver, SmalltalkClass c, SmalltalkGlobalInfo globalInfo)
//     {
//         var expressionValue = SmalltalkGlobalInfo.UnblockValue(Expression.Evaluate(localEnvironment, receiver, c, globalInfo));
//         var userVal = receiver as SmalltalkUserValue;
//
//         if (localEnvironment != null && localEnvironment.IsDefined(VariableName))
//         {
//             localEnvironment.AddBubbleDown(VariableName, expressionValue);
//         }
//         else if (userVal != null && userVal.Value.Dict.ContainsKey(VariableName))
//         {
//             userVal.Value.Dict[VariableName] = expressionValue;
//         }
//         else if (c == null || !c.TrySetClassVariableValue(VariableName, expressionValue))
//         {
//             globalInfo.GlobalEnvironment.Dict[VariableName] = expressionValue;
//         }
//
//         return expressionValue;
//     }
// }
