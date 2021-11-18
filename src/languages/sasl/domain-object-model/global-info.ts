// global-info.ts

import { IParser, ITokenizer } from 'thaw-interpreter-types';

// import { EnvironmentFrame } from '../../../common/domain-object-model/environment-frame';
// import { ExpressionList } from '../../../common/domain-object-model/expression-list';
// import { IExpression } from '../../../common/domain-object-model/iexpression';
// import { IGlobalInfo } from '../../../common/domain-object-model/iglobal-info';
// import { VariableList } from '../../../common/domain-object-model/variable-list';

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
	//     Evaluate("(set force (lambda (x) (if (list? x) (if (force (car x)) (force (cdr x)) '()) 'T)))");    // Un-thunk x.
	//     Evaluate("(set ints-from (lambda (i) (cons i (ints-from (+1 i)))))");
	//     Evaluate("(set ints (ints-from 0))");
	//     Evaluate(@"(set first-n (lambda (n l)
	//                     (if (or (null? l) (= n 0)) '()
	//                         (cons (car l) (first-n (- n 1) (cdr l))))))");
	//
	//     // Define commonly-used lambda expressions here.
	//     /* TODO:
	//     Evaluate("");
	//      */
	// }
}
