// string.ts

// SmalltalkStringValue objects are immutable.

// public class SmalltalkStringValue : SmalltalkValueBase // TODO: Is this class identical to SmalltalkSymbolValue?
// {
//     public readonly string Value;
//
//     public SmalltalkStringValue(string value)
//         : base(SmalltalkObjectClassKeeper.ObjectClass)
//     {
//
//         if (value == null) // 2013/12/05 : We will allow an empty string, but not a null reference.
//         {
//             throw new ArgumentException("SmalltalkStringValue constructor: value is null.", "value");
//         }
//
//         Value = value;
//     }
//
//     public override string ToString()
//     {
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
//         SmalltalkStringValue otherStringVal = obj as SmalltalkStringValue;
//
//         return otherStringVal != null && Value == otherStringVal.Value;
//     }
//
//     public override int GetHashCode()
//     {
//         return Value.GetHashCode();
//     }
//
//     public override string GetTypename()
//     {
//         return "string";
//     }
//
//     public override bool IsString()
//     {
//         return true;
//     }
//
//     public ISmalltalkValue Index(ISmalltalkValue idx)
//     {
//         var i = ((ISmalltalkNumber)idx).ToInteger();
//
//         if (i <= 0 || i > Value.Length)
//         {
//             throw new Exception(string.Format("SmalltalkStringValue.Index(): Index {0} is not in the range from 1 to {1}",
//                 i, Value.Length));
//         }
//
//         return new SmalltalkCharacterValue(Value[i - 1]);
//     }
// }
