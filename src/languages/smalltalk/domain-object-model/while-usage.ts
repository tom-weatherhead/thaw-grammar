// while-usage.ts

// public class SmalltalkWhileUsage : ISmalltalkExpression
// {
//     public readonly ISmalltalkExpression Condition;
//     public readonly ISmalltalkExpression Body;
//
//     public SmalltalkWhileUsage(ISmalltalkExpression condition, ISmalltalkExpression body)
//     {
//         Condition = condition;
//         Body = body;
//     }
//
//     /*
//     public override string ToString()
//     {
//         return string.Format("(while {0} {1})", Condition, Body);
//     }
//      */
//
//     public ISmalltalkValue Evaluate(SmalltalkEnvironmentFrame localEnvironment, ISmalltalkValue receiver, SmalltalkClass c, SmalltalkGlobalInfo globalInfo)
//     {
// #if DEAD_CODE
//         ISmalltalkValue conditionValue;
//         ISmalltalkValue falseValue = globalInfo.FalseValue;
//
//         for (; ; )
//         {
//             conditionValue = Condition.Evaluate(localEnvironment, receiver, c, globalInfo);
//
//             if (conditionValue.Equals(falseValue))
//             {
//                 break;
//             }
//
//             Body.Evaluate(localEnvironment, receiver, c, globalInfo);
//         }
//
//         return conditionValue;
// #else
//
//         while (!globalInfo.ValueIsFalse(SmalltalkGlobalInfo.UnblockValue(Condition.Evaluate(localEnvironment, receiver, c, globalInfo))))
//         {
//             Body.Evaluate(localEnvironment, receiver, c, globalInfo);
//         }
//
//         return globalInfo.ZeroValue;
// #endif
//     }
// }
