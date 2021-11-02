// begin-usage.ts

// public class SmalltalkBeginUsage : ISmalltalkExpression
// {
//     public readonly ISmalltalkExpression FirstExpression;
//     public readonly List<ISmalltalkExpression> ExpressionList;
//
//     public SmalltalkBeginUsage(ISmalltalkExpression firstExpression, List<ISmalltalkExpression> expressionList)
//     {
//         FirstExpression = firstExpression;
//         ExpressionList = expressionList;
//     }
//
//     /*
//     public override string ToString()
//     {
//         return string.Format("(begin {0} {1})", FirstExpression, ExpressionList);
//     }
//      */
//
//     public ISmalltalkValue Evaluate(SmalltalkEnvironmentFrame localEnvironment, ISmalltalkValue receiver, SmalltalkClass c, SmalltalkGlobalInfo globalInfo)
//     {
//         ISmalltalkValue result = FirstExpression.Evaluate(localEnvironment, receiver, c, globalInfo);
//
//         foreach (var expression in ExpressionList)
//         {
//             result = expression.Evaluate(localEnvironment, receiver, c, globalInfo);
//         }
//
//         return result;
//     }
// }
