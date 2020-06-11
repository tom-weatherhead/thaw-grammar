// tom-weatherhead/thaw-grammar/src/languages/lisp/domain-object-model/sexpression-list.ts

'use strict';

import { ISExpression } from './isexpression';
import { NullSExpression } from './null-sexpression';
import { SExpressionBase } from './sexpression-base';

export class SExpressionList extends SExpressionBase {
	public static makeFromList(l: ISExpression[]): ISExpression {
		return this.makeFromListHelper(l, 0);
	}

	private static makeFromListHelper(
		l: ISExpression[],
		i: number
	): ISExpression {
		if (i >= l.length) {
			return new NullSExpression();
		}

		return new SExpressionList(l[i], this.makeFromListHelper(l, i + 1));
	}

	// These two data members cannot be readonly, because of thunk evaluation.
	public head: ISExpression;
	public tail: ISExpression;

	constructor(head: ISExpression, tail: ISExpression) {
		super();

		this.head = head;
		this.tail = tail;
	}

	// public override string ToString()
	// {
	// 	return "(" + ToStringWithoutBrackets() + ")";
	// }

	public toString(): string {
		// return `(${this.ToStringWithoutBrackets()})`;

		return '(' + this.toStringWithoutBrackets() + ')';
	}

	// public override bool Equals(object obj)
	// {

	// 	if (object.ReferenceEquals(this, obj))
	// 	{
	// 		return true;
	// 	}

	// 	var otherSExprList = obj as SExpressionList;

	// 	return otherSExprList != null && head.Equals(otherSExprList.head) && tail.Equals(otherSExprList.tail);
	// }

	// public override int GetHashCode()
	// {
	// 	// The + 1 is in case this is a list of one element; we don't want (list of head) to have the same hash code as head itself,
	// 	// since they are both ISExpressions, and could be together in the same set or dictionary.
	// 	return head.GetHashCode() + 101 * tail.GetHashCode() + 1;
	// }

	public isList(): boolean {
		return true;
	}

	public getLength(): number {
		const tail = this.tail as SExpressionList;

		return tail === undefined ? 1 : tail.getLength() + 1;
	}

	private toStringWithoutBrackets(): string {
		const headAsString = this.head.toString();
		// const tailAsNullSExpression = this.tail as NullSExpression;
		// const tailAsSExpressionList = this.tail as SExpressionList;

		// console.log();
		// console.log('toStringWithoutBrackets() : tailAsNullSExpression is', typeof tailAsNullSExpression, tailAsNullSExpression);
		// console.log('toStringWithoutBrackets() : tailAsNullSExpression instanceof NullSExpression is', tailAsNullSExpression instanceof NullSExpression);
		// console.log('toStringWithoutBrackets() : tailAsSExpressionList is', typeof tailAsSExpressionList, tailAsSExpressionList);
		// console.log('toStringWithoutBrackets() : tailAsNullSExpression instanceof SExpressionList is', tailAsNullSExpression instanceof SExpressionList);

		// if ((this.tail as NullSExpression) !== undefined) {
		if (this.tail instanceof NullSExpression) {
			return headAsString;
			// } else if (tail is Thunk) {
			// 	return string.Format("{0} {1}", headAsString, tail);
			// } else if ((this.tail as SExpressionList) !== undefined) {
		} else if (this.tail instanceof SExpressionList) {
			const tail = this.tail as SExpressionList;

			return `${headAsString} ${tail.toStringWithoutBrackets()}`;
		} else {
			// tail is a symbol, an integer literal, a string, a closure, etc.
			return `${headAsString} ${this.tail.toString()}`;
		}
	}
}
