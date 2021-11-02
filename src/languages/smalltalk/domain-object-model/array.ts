// array.ts

// SmalltalkArrayValue objects are mutable.

// public class SmalltalkArrayValue : SmalltalkValueBase
// {
//     public readonly ISmalltalkValue[] Value;
//
//     public SmalltalkArrayValue(int size)
//         : base(SmalltalkObjectClassKeeper.ObjectClass)
//     {
//
//         if (size < 0)
//         {
//             throw new ArgumentException("SmalltalkStringValue constructor: size < 0", "size");
//         }
//
//         Value = new ISmalltalkValue[size];
//
//         var zero = new SmalltalkIntegerValue(0);
//
//         for (var i = 0; i < size; ++i)
//         {
//             Value[i] = zero;
//         }
//     }
//
//     public override string ToString()
//     {
//         return string.Join(" ", (IEnumerable<ISmalltalkValue>)Value);
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
//         return "array";
//     }
//
//     public override bool IsArray()
//     {
//         return true;
//     }
//
//     public ISmalltalkValue GetElement(int i) // Indexing starts at 1
//     {
//
//         if (i <= 0 || i > Value.Length)
//         {
//             throw new ArgumentException(
//                 string.Format("SmalltalkArrayValue.GetElement() : i is not in the range from 1 to {0}", Value.Length),
//                 "i");
//         }
//
//         return Value[i - 1];
//     }
//
//     public ISmalltalkValue SetElement(int i, ISmalltalkValue elementValue) // Indexing starts at 1
//     {
//
//         if (i <= 0 || i > Value.Length)
//         {
//             throw new ArgumentException(
//                 string.Format("SmalltalkArrayValue.GetElement() : i is not in the range from 1 to {0}", Value.Length),
//                 "i");
//         }
//
//         Value[i - 1] = elementValue;
//         return elementValue;
//     }
// }
