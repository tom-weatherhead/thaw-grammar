// cond-usage.ts

// public class SmalltalkCondUsage : ISmalltalkExpression
// {
//     public readonly List<KeyValuePair<ISmalltalkExpression, ISmalltalkExpression>> ExprPairList;
//
//     public SmalltalkCondUsage(List<KeyValuePair<ISmalltalkExpression, ISmalltalkExpression>> exprPairList)
//     {
//         ExprPairList = exprPairList;
//     }
//
//     public ISmalltalkValue Evaluate(SmalltalkEnvironmentFrame localEnvironment, ISmalltalkValue receiver, SmalltalkClass c, SmalltalkGlobalInfo globalInfo)
//     {
//         //var falseValue = globalInfo.FalseValue;
//
//         foreach (var exprPair in ExprPairList)
//         {
//
//             //if (!exprPair.Key.Evaluate(localEnvironment, receiver, c, globalInfo).Equals(falseValue))
//             if (!globalInfo.ValueIsFalse(SmalltalkGlobalInfo.UnblockValue(exprPair.Key.Evaluate(localEnvironment, receiver, c, globalInfo))))
//             {
//                 return exprPair.Value.Evaluate(localEnvironment, receiver, c, globalInfo);
//             }
//         }
//
//         //return falseValue;
//         return globalInfo.ZeroValue;
//     }
// }
