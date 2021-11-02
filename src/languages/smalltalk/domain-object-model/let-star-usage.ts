// let-star-usage.ts

// public class SmalltalkLetStarUsage : ISmalltalkExpression
// {
//     public readonly List<KeyValuePair<SmalltalkVariable, ISmalltalkExpression>> Bindings;
//     public readonly ISmalltalkExpression Expression;
//
//     public SmalltalkLetStarUsage(List<KeyValuePair<SmalltalkVariable, ISmalltalkExpression>> bindings, ISmalltalkExpression expression)
//     {
//         Bindings = bindings;
//         Expression = expression;
//     }
//
//     public ISmalltalkValue Evaluate(SmalltalkEnvironmentFrame localEnvironment, ISmalltalkValue receiver, SmalltalkClass c, SmalltalkGlobalInfo globalInfo)
//     {
// #if DEAD_CODE
//         var newEnvFrame = new SmalltalkEnvironmentFrame(localEnvironment);
//
//         foreach (var binding in Bindings)
//         {
//             newEnvFrame.Add(binding.Key, binding.Value.Evaluate(newEnvFrame, receiver, c, globalInfo));
//         }
//
//         return Expression.Evaluate(newEnvFrame, receiver, c, globalInfo);
// #else
//         // 2014/02/17 : This implementation does not support recursive definitions.
//         var lastEnv = localEnvironment;
//
//         foreach (var binding in Bindings)
//         {
//             var newEnvFrame = new SmalltalkEnvironmentFrame(lastEnv);
//
//             newEnvFrame.Add(binding.Key, binding.Value.Evaluate(lastEnv, receiver, c, globalInfo));
//             lastEnv = newEnvFrame;
//         }
//
//         return Expression.Evaluate(lastEnv, receiver, c, globalInfo);
// #endif
//     }
// }
