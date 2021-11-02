// if-usage.ts

// public class SmalltalkIfUsage : ISmalltalkExpression
// {
//     public readonly ISmalltalkExpression Condition;
//     public readonly ISmalltalkExpression IfBody;
//     public readonly ISmalltalkExpression ElseBody;
//
//     public SmalltalkIfUsage(ISmalltalkExpression condition, ISmalltalkExpression ifBody, ISmalltalkExpression elseBody)
//     {
//         Condition = condition;
//         IfBody = ifBody;
//         ElseBody = elseBody;
//     }
//
//     /*
//     public override string ToString()
//     {
//         return string.Format("(if {0} {1} {2})", Condition, IfBody, ElseBody);
//     }
//      */
//
//     public ISmalltalkValue Evaluate(SmalltalkEnvironmentFrame localEnvironment, ISmalltalkValue receiver, SmalltalkClass c, SmalltalkGlobalInfo globalInfo)
//     {
//         ISmalltalkValue conditionValue = SmalltalkGlobalInfo.UnblockValue(Condition.Evaluate(localEnvironment, receiver, c, globalInfo));
//
//         //if (!conditionValue.Equals(globalInfo.FalseValue))
//         if (!globalInfo.ValueIsFalse(conditionValue))
//         {
//             return IfBody.Evaluate(localEnvironment, receiver, c, globalInfo);
//         }
//         else
//         {
//             return ElseBody.Evaluate(localEnvironment, receiver, c, globalInfo);
//         }
//     }
// }
