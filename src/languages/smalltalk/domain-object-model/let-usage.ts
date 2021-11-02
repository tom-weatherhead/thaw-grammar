// let-usage.ts

// public class SmalltalkLetUsage : ISmalltalkExpression
// {
//     public readonly List<KeyValuePair<SmalltalkVariable, ISmalltalkExpression>> Bindings;
//     public readonly ISmalltalkExpression Expression;
//
//     public SmalltalkLetUsage(List<KeyValuePair<SmalltalkVariable, ISmalltalkExpression>> bindings, ISmalltalkExpression expression)
//     {
//         Bindings = bindings;
//         Expression = expression;
//     }
//
//     public ISmalltalkValue Evaluate(SmalltalkEnvironmentFrame localEnvironment, ISmalltalkValue receiver, SmalltalkClass c, SmalltalkGlobalInfo globalInfo)
//     {
//         var newEnvFrame = new SmalltalkEnvironmentFrame(localEnvironment);
//
//         foreach (var binding in Bindings)
//         {
//             newEnvFrame.Add(binding.Key, binding.Value.Evaluate(localEnvironment, receiver, c, globalInfo));
//         }
//
//         return Expression.Evaluate(newEnvFrame, receiver, c, globalInfo);
//     }
// }
