// tom-weatherhead/thaw-grammar/test/languages/scheme/domain-object-model.test.ts

'use strict';

import { GlobalInfoBase } from '../../../src/common/domain-object-model/global-info-base';
import { IGlobalInfo } from '../../../src/common/domain-object-model/iglobal-info';
import { Name } from '../../../src/common/domain-object-model/name';

import { FloatLiteral } from '../../../src/languages/lisp/domain-object-model/float-literal';
import { IntegerLiteral } from '../../../src/languages/lisp/domain-object-model/integer-literal';
import { ISExpression } from '../../../src/languages/lisp/domain-object-model/isexpression';
import { LISPString } from '../../../src/languages/lisp/domain-object-model/lisp-string';
import { LISPSymbol } from '../../../src/languages/lisp/domain-object-model/lisp-symbol';
import { NullSExpression } from '../../../src/languages/lisp/domain-object-model/null-sexpression';
import { SExpressionList } from '../../../src/languages/lisp/domain-object-model/sexpression-list';

// import { Closure } from '../../../src/languages/scheme/domain-object-model/closure';
import { PrimOp } from '../../../src/languages/scheme/domain-object-model/primitive-operator';
import { SchemeGlobalInfo } from '../../../src/languages/scheme/domain-object-model/scheme-global-info';

test('Dummy test', () => {
	// Arrange
	// Act
	// Assert
	expect(true).toBeTruthy();
});

test('SchemeGlobalInfo number to and from IntegerLiteral and FloatLiteral test', () => {
	// Arrange
	const globalInfo = new SchemeGlobalInfo();

	const exampleInteger = 13;
	const exampleFloat = 7.125;
	const exampleFloatAsInteger = 7;
	const exampleIntegerLiteral = new IntegerLiteral(exampleInteger);
	const exampleFloatLiteral = new FloatLiteral(exampleFloat);
	const exampleNull = new NullSExpression();
	const exampleSymbol1 = new LISPSymbol('a');
	const exampleSExpressionList1 = new SExpressionList(exampleSymbol1, exampleNull);
	const exampleString = new LISPString('str');
	const examplePrimOp = new PrimOp(new Name('+'));
	// const exampleClosure = new Closure(...);

	const exampleSExpression1: ISExpression = new NullSExpression();
	const exampleSExpression2: ISExpression = new LISPSymbol('a');

	// Act
	// Assert
	expect(true).toBeTruthy();

	expect(globalInfo instanceof SchemeGlobalInfo).toBeTruthy();
	expect(globalInfo instanceof GlobalInfoBase).toBeTruthy();
	// expect(globalInfo instanceof IGlobalInfo).toBeTruthy(); // instanceof doesn't seem to work for interface types.
	// expect(globalInfo instanceof ISExpression).toBeFalsy(); // instanceof doesn't seem to work for interface types.
	expect(globalInfo instanceof IntegerLiteral).toBeFalsy();
	expect(exampleSExpression1 instanceof NullSExpression).toBeTruthy();
	expect(exampleSExpression2 instanceof NullSExpression).toBeFalsy();

	expect(globalInfo as SchemeGlobalInfo).toBeTruthy();
	expect(globalInfo as GlobalInfoBase<ISExpression>).toBeTruthy();
	expect(globalInfo as IGlobalInfo<ISExpression>).toBeTruthy();
	// expect(globalInfo as ISExpression).toBeFalsy(); // Typescript diagnostics catches this as an error.
	// expect(globalInfo as IntegerLiteral).toBeFalsy(); // Typescript diagnostics catches this as an error.
	expect(exampleSExpression1 as NullSExpression).toBeTruthy();
	// expect(exampleSExpression2 as NullSExpression).toBeFalsy(); // Fails: Received: {"value": "a"}, which is truthy.
	expect(typeof exampleSExpression1).toBe('object');
	expect(typeof (exampleSExpression1 as NullSExpression)).toBe('object');
	expect(typeof exampleSExpression2).toBe('object');
	expect(typeof (exampleSExpression2 as NullSExpression)).toBe('object'); // !!! Note: It is *not* 'undefined' !

	expect(globalInfo.valueIsInteger(exampleIntegerLiteral)).toBeTruthy();
	expect(globalInfo.valueIsInteger(exampleFloatLiteral)).toBeFalsy();
	expect(globalInfo.valueIsInteger(exampleNull)).toBeFalsy();
	expect(globalInfo.valueIsInteger(exampleSymbol1)).toBeFalsy();
	expect(globalInfo.valueIsInteger(exampleSExpressionList1)).toBeFalsy();
	expect(globalInfo.valueIsInteger(exampleString)).toBeFalsy();
	expect(globalInfo.valueIsInteger(examplePrimOp)).toBeFalsy();
	// expect(globalInfo.valueIsInteger(exampleClosure)).toBeFalsy();

	expect(globalInfo.valueAsInteger(exampleIntegerLiteral)).toBe(exampleInteger);
	expect(globalInfo.valueAsInteger(exampleFloatLiteral)).toBe(exampleFloatAsInteger);
	expect(() => globalInfo.valueAsInteger(exampleNull)).toThrow('ArgumentException');
	expect(() => globalInfo.valueAsInteger(exampleSymbol1)).toThrow('ArgumentException');
	expect(() => globalInfo.valueAsInteger(exampleSExpressionList1)).toThrow('ArgumentException');
	expect(() => globalInfo.valueAsInteger(exampleString)).toThrow('ArgumentException');
	expect(() => globalInfo.valueAsInteger(examplePrimOp)).toThrow('ArgumentException');
	// expect(() => globalInfo.valueAsInteger(exampleClosure)).toThrow('ArgumentException');

	expect(globalInfo.valueIsNumber(exampleIntegerLiteral)).toBeTruthy();
	expect(globalInfo.valueIsNumber(exampleFloatLiteral)).toBeTruthy();
	expect(globalInfo.valueIsNumber(exampleNull)).toBeFalsy();
	expect(globalInfo.valueIsNumber(exampleSymbol1)).toBeFalsy();
	expect(globalInfo.valueIsNumber(exampleSExpressionList1)).toBeFalsy();
	expect(globalInfo.valueIsNumber(exampleString)).toBeFalsy();
	expect(globalInfo.valueIsNumber(examplePrimOp)).toBeFalsy();
	// expect(globalInfo.valueIsNumber(exampleClosure)).toBeFalsy();

	expect(globalInfo.valueAsNumber(exampleIntegerLiteral)).toBe(exampleInteger);
	expect(globalInfo.valueAsNumber(exampleFloatLiteral)).toBe(exampleFloat);
	expect(() => globalInfo.valueAsNumber(exampleNull)).toThrow('ArgumentException');
	expect(() => globalInfo.valueAsNumber(exampleSymbol1)).toThrow('ArgumentException');
	expect(() => globalInfo.valueAsNumber(exampleSExpressionList1)).toThrow('ArgumentException');
	expect(() => globalInfo.valueAsNumber(exampleString)).toThrow('ArgumentException');
	expect(() => globalInfo.valueAsNumber(examplePrimOp)).toThrow('ArgumentException');
	// expect(() => globalInfo.valueAsNumber(exampleClosure)).toThrow('ArgumentException');

	expect(globalInfo.numberAsIntegerValue(exampleInteger) instanceof IntegerLiteral).toBeTruthy();
	expect(globalInfo.numberAsFloatValue(exampleFloat) instanceof FloatLiteral).toBeTruthy();
});
