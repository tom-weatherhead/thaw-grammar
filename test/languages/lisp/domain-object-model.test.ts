// tom-weatherhead/thaw-grammar/test/languages/lisp/domain-object-model.test.ts

'use strict';

import { FloatLiteral } from '../../../src/languages/lisp/domain-object-model/float-literal';
import { IntegerLiteral } from '../../../src/languages/lisp/domain-object-model/integer-literal';
import { LISPString } from '../../../src/languages/lisp/domain-object-model/lisp-string';
import { LISPSymbol } from '../../../src/languages/lisp/domain-object-model/lisp-symbol';
import { NullSExpression } from '../../../src/languages/lisp/domain-object-model/null-sexpression';
import { SExpressionList } from '../../../src/languages/lisp/domain-object-model/sexpression-list';

test('LISP list-to-string test', () => {
	// Arrange
	const nullSExpr = new NullSExpression();
	const int3 = new IntegerLiteral(3);
	const list3 = new SExpressionList(int3, nullSExpr);
	const int2 = new IntegerLiteral(2);
	const list2 = new SExpressionList(int2, list3);
	const int1 = new IntegerLiteral(1);
	const list1 = new SExpressionList(int1, list2);
	const float1 = new FloatLiteral(1.25);
	const float2 = new FloatLiteral(-1.0);
	const fooRawString = 'foo';
	const fooString = new LISPString(fooRawString);
	const fooSymbol = new LISPSymbol(fooRawString);

	// Act
	const nullSExprAsString = nullSExpr.toString();
	const int3AsString = int3.toString();
	const list1AsString = list1.toString();
	const list2AsString = list2.toString();
	const list3AsString = list3.toString();

	// Assert
	expect(nullSExprAsString).toBe('()');
	expect(int3AsString).toBe('3');
	expect(list1AsString).toBe('(1 2 3)');
	expect(list2AsString).toBe('(2 3)');
	expect(list3AsString).toBe('(3)');
	expect(() => new IntegerLiteral('a')).toThrow('ArgumentException');
	expect(() => new IntegerLiteral(Number.NaN)).toThrow('ArgumentException');

	// HACK ThAW 2020-01-06 : The value in an IntegerLiteral can be int or float.
	// In the future, we will need to properly support FloatLiterals
	// and distinguish them from IntegerLiterals.

	// expect(() => new IntegerLiteral(1.5)).toThrow('ArgumentException');

	expect(float1.toString()).toBe('1.25');
	expect(float2.toString()).toBe('-1.0');
	expect(() => new FloatLiteral('a')).toThrow('ArgumentException');
	expect(() => new FloatLiteral(Number.NaN)).toThrow('ArgumentException');
	expect(fooString.toString()).toBe('"' + fooRawString + '"');
	expect(fooSymbol.toString()).toBe(fooRawString);
});
