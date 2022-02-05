// tom-weatherhead/thaw-grammar/src/languages/lisp/domain-object-model/sexpression-list.ts

import { ISExpression } from './isexpression';
import { NullSExpression } from './null-sexpression';
import { SExpressionBase } from './sexpression-base';

const typenameSExpressionList = 'SExpressionList';

export function isSExpressionList(obj: unknown): obj is SExpressionList {
	const intlit = obj as SExpressionList;

	return typeof intlit !== 'undefined' && intlit.typename === typenameSExpressionList;
}

export class SExpressionList extends SExpressionBase {
	public static makeFromList(l: ISExpression[]): ISExpression {
		return this.makeFromListHelper(l, 0);
	}

	private static makeFromListHelper(l: ISExpression[], i: number): ISExpression {
		if (i >= l.length) {
			return new NullSExpression();
		}

		return new SExpressionList(l[i], this.makeFromListHelper(l, i + 1));
	}

	public readonly typename: string = typenameSExpressionList;

	// These two data members cannot be readonly, because of thunk evaluation.
	public head: ISExpression;
	public tail: ISExpression;

	constructor(head: ISExpression, tail: ISExpression) {
		super();

		this.head = head;
		this.tail = tail;
	}

	public toString(): string {
		// return `(${this.ToStringWithoutBrackets()})`;

		return '(' + this.toStringWithoutBrackets() + ')';
	}

	// public override bool Equals(object obj)
	// {
	// 	var otherSExprList = obj as SExpressionList;

	// 	return otherSExprList != null && head.Equals(otherSExprList.head) && tail.Equals(otherSExprList.tail);
	// }

	public override isList(): boolean {
		return true;
	}

	public getLength(): number {
		const tail = this.tail as SExpressionList;

		return tail === undefined ? 1 : tail.getLength() + 1;
	}

	private toStringWithoutBrackets(): string {
		if (this.tail.isNull()) {
			return this.head.toString();
		} else if (isSExpressionList(this.tail)) {
			return `${this.head} ${this.tail.toStringWithoutBrackets()}`;
		} else {
			// tail is a symbol, an integer literal, a string, a closure, etc.
			return `${this.head} . ${this.tail.toString()}`;
		}
	}
}
