// block.ts

// A SmalltalkBlock is a lot like an SASL Thunk (a suspended computation).

// SmalltalkBlock objects are immutable.

// public class SmalltalkBlock : SmalltalkValueBase
// {
//     public readonly ISmalltalkExpression Expression;
//     public readonly SmalltalkEnvironmentFrame LocalEnvironment;
//     public readonly ISmalltalkValue Receiver;
//     public readonly SmalltalkClass Class;
//     public readonly SmalltalkGlobalInfo GlobalInfo;
//
//     public SmalltalkBlock(ISmalltalkExpression expression, SmalltalkEnvironmentFrame localEnvironment,
//         ISmalltalkValue receiver, SmalltalkClass c, SmalltalkGlobalInfo globalInfo)
//         : base(SmalltalkObjectClassKeeper.ObjectClass)
//     {
//         Expression = expression;
//         LocalEnvironment = localEnvironment;
//         Receiver = receiver;
//         Class = c;
//         GlobalInfo = globalInfo;
//     }
//
//     public override string ToString()
//     {
//         return "<block>";
//     }
//
//     public override bool Equals(object obj)
//     {
//         return object.ReferenceEquals(this, obj);
//     }
//
//     public override int GetHashCode()
//     {
//         return 0;
//     }
//
//     public override string GetTypename()
//     {
//         return "block";
//     }
//
//     public ISmalltalkValue Unblock()
//     {
//         ISmalltalkValue result = this;
//
//         for (; ;)
//         {
//             var block = result as SmalltalkBlock;
//
//             if (block == null)
//             {
//                 break;
//             }
//
//             result = block.Expression.Evaluate(block.LocalEnvironment, block.Receiver, block.Class, block.GlobalInfo);
//         }
//
//         return result;
//     }
// }
