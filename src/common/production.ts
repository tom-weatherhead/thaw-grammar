// tom-weatherhead/thaw-grammar/src/common/production.ts

'use strict';

import { Symbol } from './symbol';

export class Production {
	public lhs: number; // Symbol;
	public rhs: Array<number | string>;
	private readonly num: number;

	constructor(l: number, r: Array<number | string>, n = 0) {
		this.lhs = l;
		this.rhs = r;
		this.num = n;
	}

	// public override bool Equals(object obj)
	// {

	// 	if (object.ReferenceEquals(this, obj))
	// 	{
	// 		return true;
	// 	}

	// 	Production that = obj as Production;

	// 	// return that != null && lhs.Equals(that.lhs) && rhs.Equals(that.rhs);

	// 	if (that == null || !lhs.Equals(that.lhs) || rhs.Count != that.rhs.Count)
	// 	{
	// 		return false;
	// 	}

	// 	for (int i = 0; i < rhs.Count; ++i)
	// 	{

	// 		if (!rhs[i].Equals(that.rhs[i]))
	// 		{
	// 			return false;
	// 		}
	// 	}

	// 	return true;
	// }

	// public override int GetHashCode()
	// {
	// 	return rhs
	// 		.Select(o => o.GetHashCode())
	// 		.Aggregate(lhs.GetHashCode(), (accumulator, hashCode) => accumulator * 101 + hashCode);
	// }

	// public override string ToString()
	// {
	// 	return string.Format("{0}: {1} -> {2}", num, lhs, string.Join(" ", rhs));
	// }
	public toString(): string {
		const lhsAsString: string = Symbol[this.lhs];
		const rhsAsString: string = this.rhs
			.map((o: number | string) => {
				if (typeof o === 'string') {
					return o;
				} else {
					return Symbol[o];
				}
			})
			.join(' ');

		// return `${this.num}: ${this.lhs} -> ${this.rhs}`;

		return `${this.num}: ${lhsAsString} -> ${rhsAsString}`;
	}

	public RHSWithNoSemanticActions(): number[] {
		// return this.rhs.filter((o: any) => o is Symbol).Select(o => (Symbol)o).ToList();

		// return this.rhs
		// 	.filter((o: any) => o as number)
		// 	.filter((o: number) => o !== undefined);

		const result: number[] = [];

		this.rhs.forEach((o: string | number) => {
			// if (o instanceof Number) {
			// if (typeof o !== 'string') {
			if (typeof o === 'number') {
				result.push(o);
			}
		});

		// console.log(`RHSWithNoSemanticActions() : Input : ${this.rhs}`);
		// console.log(`RHSWithNoSemanticActions() : Output: ${result}`);

		return result;
	}

	public StripOutSemanticActions(): Production {
		// var newRHS = rhs.Where(o => o is Symbol).ToList();
		const newRHS = this.rhs.filter((o) => (o as number) !== undefined);

		return new Production(this.lhs, newRHS, this.num);
	}

	public ContainsSymbol(symbol: number): boolean {
		return (
			this.lhs === symbol ||
			this.rhs.find((o) => o === symbol) !== undefined
		);
	}
}
