// tom-weatherhead/thaw-grammar/src/common/production.ts

import { IEqualityComparable } from 'thaw-common-utilities.ts';

import { Symbol } from './symbol';

/* eslint-disable @typescript-eslint/ban-types */

export type ProductionRhsElementType = Symbol | string;

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

	public equals(other: unknown): boolean {
		const otherProduction = other as Production;

		if (
			typeof otherProduction === 'undefined' ||
			!(other instanceof Production) ||
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
			this.lhs === symbol || this.rhs.find((s: ProductionRhsElementType) => s === symbol) !== undefined
		);
	}
}
/* eslint-enable @typescript-eslint/ban-types */
