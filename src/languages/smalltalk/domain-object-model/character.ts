// character.ts

// SmalltalkCharacterValue objects are immutable.

// public class SmalltalkCharacterValue : SmalltalkValueBase
// {
//     public readonly char Value;
//
//     public SmalltalkCharacterValue(char value)
//         : base(SmalltalkObjectClassKeeper.ObjectClass)
//     {
//
//         if (value == '\0')
//         {
//             throw new ArgumentException("SmalltalkCharacterValue constructor: value is the null character.", "value");
//         }
//
//         Value = value;
//     }
//
//     public override string ToString()
//     {
//         return Value.ToString();
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
//         SmalltalkCharacterValue otherCharVal = obj as SmalltalkCharacterValue;
//
//         return otherCharVal != null && Value == otherCharVal.Value;
//     }
//
//     public override int GetHashCode()
//     {
//         return Value.GetHashCode();
//     }
//
//     public override string GetTypename()
//     {
//         return "char";
//     }
//
//     public override bool IsCharacter()
//     {
//         return true;
//     }
// }
