// tom-weatherhead/thaw-grammar/src/common/production.ts

import { IEqualityComparable } from 'thaw-common-utilities.ts';

import { Symbol } from './symbol';

/* eslint-disable @typescript-eslint/ban-types */

export type ProductionRhsElementType = Symbol | string;

// A user-defined type guard. See e.g. https://www.typescriptlang.org/docs/handbook/2/narrowing.html#using-type-predicates

export function isProduction(obj: unknown): obj is Production {
	// eslint-disable-next-line @typescript-eslint/no-explicit-any
	const aaaargh = obj as any;
	const lhs = aaaargh.lhs;
	const rhs = aaaargh.rhs;
	const num = aaaargh.num;

	return (
		typeof lhs !== 'undefined' &&
		typeof lhs === 'number' &&
		typeof rhs !== 'undefined' &&
		rhs instanceof Array &&
		rhs.every(
			(element: unknown) => typeof element === 'number' || typeof element === 'string'
		) &&
		typeof num !== 'undefined' &&
		typeof num === 'number'
	);
}

export class Production implements IEqualityComparable {
	public lhs: Symbol;
	public rhs: ProductionRhsElementType[];
	private readonly num: number;

	constructor(l: Symbol, r: ProductionRhsElementType[], n = 0) {
		this.lhs = l;
		this.rhs = r;
		this.num = n;
	}

	public toString(): string {
		const lhsAsString: string = Symbol[this.lhs];
		const rhsAsString: string = this.rhs
			.map((s: ProductionRhsElementType) => {
				if (typeof s === 'string') {
					return s;
				} else {
					return Symbol[s];
				}
			})
			.join(' ');

		return `${this.num}: ${lhsAsString} -> ${rhsAsString}`;
	}

	// public equals(otherProduction: Production): boolean {
	public equals(other: unknown): boolean {
		const otherProduction = other as Production;

		if (
			// typeof otherProduction === 'undefined' ||
			!isProduction(other) ||
			// !(other instanceof Production) ||
			// otherProduction.constructor.name !== this.constructor.name ||
			this.lhs !== otherProduction.lhs ||
			this.rhs.length !== otherProduction.rhs.length
		) {
			//  || this.num !== otherProduction.num // Ignore the production number in this equality comparison.
			return false;
		}

		for (let i = 0; i < this.rhs.length; i++) {
			if (this.rhs[i] !== otherProduction.rhs[i]) {
				return false;
			}
		}

		return true;
	}

	public RHSWithNoSemanticActions(): Symbol[] {
		return this.rhs
			.filter((s: ProductionRhsElementType) => typeof s !== 'string')
			.map((s: ProductionRhsElementType) => s as Symbol);
	}

	public StripOutSemanticActions(): Production {
		return new Production(this.lhs, this.RHSWithNoSemanticActions(), this.num);
	}

	public ContainsSymbol(symbol: Symbol): boolean {
		return (
			this.lhs === symbol ||
			this.rhs.find((s: ProductionRhsElementType) => s === symbol) !== undefined
		);
	}
}
/* eslint-enable @typescript-eslint/ban-types */
