// symbol.ts

// SmalltalkSymbolValue objects are immutable.

// public class SmalltalkSymbolValue : SmalltalkValueBase
// {
//     public readonly string Value;
//
//     public SmalltalkSymbolValue(string value)
//         : base(SmalltalkObjectClassKeeper.ObjectClass)
//     {
//
//         if (string.IsNullOrEmpty(value))
//         {
//             throw new ArgumentException("SmalltalkSymbolValue constructor: value is null or empty.", "value");
//         }
//
//         Value = value;
//     }
//
//     public override string ToString()
//     {
//         //return "#" + Value;
//         return Value;
//     }
//
//     public override bool Equals(object obj)
//     {
//
//         if (object.ReferenceEquals(this, obj))
//         {
//             return true;
//         }
//
//         var otherSymbol = obj as SmalltalkSymbolValue;
//
//         return otherSymbol != null && Value == otherSymbol.Value;
//     }
//
//     public override int GetHashCode()
//     {
//         return Value.GetHashCode();
//     }
//
//     public override string GetTypename()
//     {
//         return "symbol";
//     }
//
//     public override bool IsSymbol()
//     {
//         return true;
//     }
// }
