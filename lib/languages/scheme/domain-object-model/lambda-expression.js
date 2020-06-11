// tom-weatherhead/thaw-grammar/src/languages/scheme/domain-object-model/lambda-expression.ts
'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
exports.LambdaExpression = void 0;
// import { SExpressionBase } from './sexpression-base';
const closure_1 = require("./closure");
class LambdaExpression {
    constructor(argList, body, line = 0, column = 0) {
        // console.log('Creating an instance of LambdaExpression...');
        // console.log(`-> argList is ${typeof argList} ${argList}`);
        // console.log(`-> body is ${typeof body} ${body}`);
        this.argList = argList;
        this.body = body;
        this.line = line;
        this.column = column;
    }
    toString() {
        return `(lambda ${this.argList} ${this.body})`;
    }
    evaluate(localEnvironment, globalInfo) {
        // console.log('Evaluating an instance of LambdaExpression...');
        return new closure_1.Closure(this.argList, this.body, localEnvironment, this.line, this.column);
    }
}
exports.LambdaExpression = LambdaExpression;
