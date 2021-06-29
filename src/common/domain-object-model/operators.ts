// tom-weatherhead/thaw-grammar/src/common/domain-object-model/operators.ts

export enum OperatorType {
	// Unknown,
	BinaryNumericOperator,
	BinaryNumericPredicate,
	TypePredicate,
	UnlimitedNumericOperator
}

export class Operators {
	// This class is a Singleton.

	public static getInstance(): Operators {
		if (!Operators.instance) {
			Operators.instance = new Operators();
		}

		return Operators.instance;
	}

	private static instance: Operators;
	private readonly mapOperatorNameToDetails = new Map<
		string,
		[OperatorType, number, any]
	>();

	private constructor() {
		this.mapOperatorNameToDetails.set('+', [
			OperatorType.UnlimitedNumericOperator,
			-1,
			(a: number, b: number) => a + b
		]);
		this.mapOperatorNameToDetails.set('-', [
			OperatorType.BinaryNumericOperator,
			2,
			(a: number, b: number) => a - b
		]);
		this.mapOperatorNameToDetails.set('*', [
			OperatorType.UnlimitedNumericOperator,
			-1,
			(a: number, b: number) => a * b
		]);
		this.mapOperatorNameToDetails.set('/', [
			OperatorType.BinaryNumericOperator,
			2,
			(a: number, b: number) => Math.floor(a / b)
		]);
		this.mapOperatorNameToDetails.set('=', [
			OperatorType.BinaryNumericPredicate,
			2,
			(a: number, b: number) => (a === b ? 1 : 0)
		]);
		this.mapOperatorNameToDetails.set('>', [
			OperatorType.BinaryNumericPredicate,
			2,
			(a: number, b: number) => (a > b ? 1 : 0)
		]);
		this.mapOperatorNameToDetails.set('<', [
			OperatorType.BinaryNumericPredicate,
			2,
			(a: number, b: number) => (a < b ? 1 : 0)
		]);
		// this.mapOperatorNameToDetails.set('isNumber?', [OperatorType.TypePredicate, 1, () => ]);
		// this.mapOperatorNameToDetails.set('', [OperatorType., 0, () => ]);
	}

	public getOperator(name: string): [OperatorType, number, any] | undefined {
		return this.mapOperatorNameToDetails.get(name);
	}
}
