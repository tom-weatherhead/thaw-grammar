// global-info.ts

import { IParser, ITokenizer } from 'thaw-interpreter-types';

import { ISExpression } from '../../lisp/domain-object-model/isexpression';

import { SchemeGlobalInfo } from '../../scheme/domain-object-model/scheme-global-info';

import { isThunk, Thunk } from './thunk';

export class SASLGlobalInfo extends SchemeGlobalInfo {
	constructor(
		options: {
			parser?: IParser;
			tokenizer?: ITokenizer;
		} = {}
	) {
		super(options);
	}

	public dethunk(sexpr: ISExpression): ISExpression {
		if (!isThunk(sexpr)) {
			return sexpr;
		}

		return (sexpr as Thunk).dethunk(this);
	}

	// public loadPresets(): void {
	//     this.loadSASLSafePresets();
	//
	//     this.evaluate("(set force (lambda (x) (if (list? x) (if (force (car x)) (force (cdr x)) '()) 'T)))");    // Un-thunk x.
	//     this.evaluate("(set ints-from (lambda (i) (cons i (ints-from (+1 i)))))");
	//     this.evaluate("(set ints (ints-from 0))");
	//     this.evaluate(@"(set first-n (lambda (n l)
	//                     (if (or (null? l) (= n 0)) '()
	//                         (cons (car l) (first-n (- n 1) (cdr l))))))");
	//
	//     // Define commonly-used lambda expressions here.
	//     /* TODO:
	//     Evaluate("");
	//      */
	// }
}
