// tom-weatherhead/thaw-grammar/src/languages/scheme/domain-object-model/primitive-operator.ts

import { EvaluationException, Name } from 'thaw-interpreter-core';

import { EnvironmentFrame } from '../../../common/domain-object-model/environment-frame';
import { ExpressionList } from '../../../common/domain-object-model/expression-list';
import { IExpression } from '../../../common/domain-object-model/iexpression';
import { IGlobalInfo } from '../../../common/domain-object-model/iglobal-info';
// import { Name } from '../../../common/domain-object-model/name';

import { ISExpression } from '../../lisp/domain-object-model/isexpression';
import { LISPOperatorUsage } from '../../lisp/domain-object-model/lisp-operator-usage';
import { SExpressionBase } from '../../lisp/domain-object-model/sexpression-base';

import { ICallableSExpression } from './icallable-sexpression';

// TODO? Create static methods in common/domain-object-model/operator-usage
// that export the following functionality:

// - getNumExpectedArgs(operator: string): number | undefined
//   - return -1 for 'any number of arguments is acceptable'
//   - return undefined for 'unknown operator'
// - getBinaryNumericalOperator(operator: string) : (a: number, b: number) => number
// - getBinaryNumericalPredicate(operator: string) : (a: number, b: number) => boolean
// - enum OperatorType { BinaryNumericalOperator, BinaryNumericalPredicate, ..., Unknown }
// - getOperatorType(operator: string): OperatorType | undefined
// - Then PrimOp.evaluate() looks like this:

// switch (operatorType) {
// 	case OperatorType.BinaryNumericalOperator:
// 		const fn: (a: number, b: number) => number = getBinaryNumericalOperator(operatorName.value);

// 		// return new NumberLiteral(fn((evaluatedArguments[0] as IntegerLiteral).value, (evaluatedArguments[1] as IntegerLiteral).value));
// 		// or:

// 		const evaluatedArgumentsAsNumbers: number[] = evaluatedArguments.map((arg: ISExpression) => (arg as IntegerLiteral).value);

// 		return new NumberLiteral(fn(...evaluatedArgumentsAsNumbers));

// 	case ...
// }

export class PrimOp extends SExpressionBase implements ICallableSExpression {
	// Old (C#) comment: We cannot inherit from SExpressionBase here because we already inherit from LISPOperatorUsage.
	public readonly name: Name;
	public readonly line: number;
	public readonly column: number;

	constructor(name: Name) {
		super();
		// super(name, new ExpressionList<ISExpression>());

		// if (['+', '-', '*', '/', '=', '<', '>'].indexOf(name.value) < 0) {
		// 	throw new ArgumentException(`Primitive operator '${name.value}' not yet supported`, 'name', name.line, name.column);
		// }

		this.name = name;
		// this.expectedNumArgs = 2; // Hard-coded for the operator +
		this.line = name.line;
		this.column = name.column;
	}

	public call(
		expressionList: ExpressionList<ISExpression>,
		localEnvironment: EnvironmentFrame<ISExpression>,
		globalInfo: IGlobalInfo<ISExpression>
	): ISExpression {
		if (
			[
				'+',
				'-',
				'*',
				'/',
				'=',
				'<',
				'>',
				'number?',
				'symbol?',
				'list?',
				'null?',
				'string?',
				'cons',
				'car',
				'cdr',
				'list',
				'print',
				'floor',
				'random'
			].indexOf(this.name.value) >= 0
		) {
			const operatorUsage = new LISPOperatorUsage(this.name, expressionList);

			return operatorUsage.evaluate(localEnvironment, globalInfo);
		}

		// First, check the number of arguments. (TODO)
		// Then check the argument types. (TODO)
		// Then:
		const evaluatedArguments = expressionList.value.map((expr: IExpression<ISExpression>) =>
			expr.evaluate(localEnvironment, globalInfo)
		);

		switch (this.name.value) {
			case 'primop?':
				return evaluatedArguments[0].isPrimOp()
					? globalInfo.trueValue
					: globalInfo.falseValue;

			case 'closure?':
				return evaluatedArguments[0].isClosure()
					? globalInfo.trueValue
					: globalInfo.falseValue;

			default:
				throw new EvaluationException(
					`PrimOp.call() : Unknown operator name '${this.name.value}'`,
					this.name.line,
					this.name.column
				);
		}

		// return globalInfo.falseValue;
	}

	public override isPrimOp(): boolean {
		return true;
	}

	public toString(): string {
		return this.name.value;
	}

	public override evaluate(
		/* eslint-disable @typescript-eslint/no-unused-vars */
		localEnvironment: EnvironmentFrame<ISExpression>,
		globalInfo: IGlobalInfo<ISExpression>
		/* eslint-enable @typescript-eslint/no-unused-vars */
	): ISExpression {
		return this;
	}
}

// public class PrimOp : LISPOperatorUsage, ICallableSExpression // We cannot inherit from SExpressionBase here bacause we already inherit from LISPOperatorUsage.
// {
//     public PrimOp(Name operatorName)
//         : base(operatorName, new ExpressionList<ISExpression>())
//     {
//         int expectedNumArgs = 0;
//         bool gotExpectedNumArgs = false;

//         try
//         {
//             // We can pass in null for the globalInfo because no PrimOp is a global function or a macro; the grammar protects us from crashes.
//             // TODO: Or we could check for a null globalInfo in OperatorUsage<T>.
//             gotExpectedNumArgs = TryGetExpectedNumArgs(null, out expectedNumArgs);
//         }
//         catch
//         {
//         }

//         if (!gotExpectedNumArgs)
//         {
//             throw new Exception(string.Format("PrimOp constructor: Failed to get ExpectedNumArgs for operator '{0}'.", operatorName.Value));
//         }

//         ExpectedNumArgs = expectedNumArgs;
//     }

//     public int ExpectedNumArgs { get; private set; }

//     public int Line
//     {
//         get
//         {
//             return OperatorName.Line;
//         }
//     }

//     public int Column
//     {
//         get
//         {
//             return OperatorName.Column;
//         }
//     }

//     public bool IsNumber()
//     {
//         return false;
//     }

//     public bool IsSymbol()
//     {
//         return false;
//     }

//     public bool IsList()
//     {
//         return false;
//     }

//     public bool IsNull()
//     {
//         return false;
//     }

//     public bool IsPrimOp()
//     {
//         return true;
//     }

//     public bool IsClosure()
//     {
//         return false;
//     }

//     public bool IsString()
//     {
//         return false;
//     }

//     public override ISExpression Evaluate(EnvironmentFrame<ISExpression> localEnvironment, IGlobalInfo<ISExpression> globalInfo)
//     {
//         return this;
//     }

//     protected override bool TryGetExpectedNumArgs(IGlobalInfo<ISExpression> globalInfo, out int result)
//     {

//         switch (OperatorName.Value)
//         {
//             case "primop?":
//             case "closure?":
//                 result = 1;
//                 return true;

//             default:
//                 return base.TryGetExpectedNumArgs(globalInfo, out result);
//         }
//     }

//     protected override ISExpression EvaluateAux(List<ISExpression> evaluatedArguments, EnvironmentFrame<ISExpression> localEnvironment, IGlobalInfo<ISExpression> globalInfo)
//     {

//         switch (OperatorName.Value)
//         {
//             case "primop?":
//                 return evaluatedArguments[0].IsPrimOp() ? globalInfo.TrueValue : globalInfo.FalseValue;

//             case "closure?":
//                 return evaluatedArguments[0].IsClosure() ? globalInfo.TrueValue : globalInfo.FalseValue;

//             default:
//                 return base.EvaluateAux(evaluatedArguments, localEnvironment, globalInfo);
//         }
//     }

//     public virtual ISExpression Call(ExpressionList<ISExpression> arguments, EnvironmentFrame<ISExpression> localEnvironment, IGlobalInfo<ISExpression> globalInfo)
//     {
//         // TODO: This function looks a lot like OperatorUsage<T>.Evaluate(), except for the macro handling.  See if we can unify them.
//         // (First, we would need to set the PrimOp's arguments list.)
//         var actualNumArgs = arguments.Value.Count;
// #if DEAD_CODE
//         int expectedNumArgs;

//         if (!TryGetExpectedNumArgs(globalInfo, out expectedNumArgs))
//         {
//             throw new EvaluationException(
//                 string.Format("PrimOp : Unknown operator name '{0}'", OperatorName.Value),
//                 OperatorName.Line, OperatorName.Column);
//         }
//         else if (expectedNumArgs >= 0 && actualNumArgs != expectedNumArgs)
//         {
//             throw new EvaluationException(
//                 string.Format("PrimOp : Expected {0} argument(s) for operator '{1}', instead of the actual {2} argument(s)",
//                     expectedNumArgs, OperatorName.Value, actualNumArgs),
//                 OperatorName.Line, OperatorName.Column);
//         }
// #else
//         if (ExpectedNumArgs >= 0 && actualNumArgs != ExpectedNumArgs)
//         {
//             throw new EvaluationException(
//                 string.Format("PrimOp : Expected {0} argument(s) for operator '{1}', instead of the actual {2} argument(s)",
//                     ExpectedNumArgs, OperatorName.Value, actualNumArgs),
//                 OperatorName.Line, OperatorName.Column);
//         }
// #endif

//         var evaluatedArguments = arguments.Value.Select(expr => expr.Evaluate(localEnvironment, globalInfo)).ToList();
//         var argTypesErrorMessage = CheckArgTypes(evaluatedArguments);

//         if (!string.IsNullOrEmpty(argTypesErrorMessage))
//         {
//             throw new EvaluationException(
//                 string.Format("Operator '{0}': {1}", OperatorName.Value, argTypesErrorMessage),
//                 OperatorName.Line, OperatorName.Column);
//         }

//         // It is safe to pass a null localEnvironment to EvaluateAux(), since it only uses localEnvironment to evaluate user-defined LISP functions.
//         // TODO: Could we pass in a null globalInfo too?  See the PrimOp constructor.
//         return EvaluateAux(evaluatedArguments, null, globalInfo);
//     }
// }
