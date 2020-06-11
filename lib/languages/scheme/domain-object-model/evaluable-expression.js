// tom-weatherhead/thaw-grammar/src/languages/scheme/domain-object-model/evaluable-expression.ts
'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
exports.EvaluableExpression = void 0;
const variable_1 = require("../../../common/domain-object-model/variable");
// import { ArgumentException } from '../../../common/exceptions/argument-exception';
const evaluation_exception_1 = require("../../../common/exceptions/evaluation-exception");
class EvaluableExpression {
    constructor(firstExpression, expressionList) {
        // console.log('Creating an instance of EvaluableExpression...');
        this.firstExpression = firstExpression;
        this.expressionList = expressionList;
    }
    evaluate(localEnvironment, globalInfo) {
        // console.log('Evaluating an instance of EvaluableExpression...');
        // const firstExprAsVariable = this.firstExpression as Variable<ISExpression>;
        // console.log('this.firstExpression =', this.firstExpression);
        // console.log('this.firstExpression as Variable<ISExpression> =', this.firstExpression as Variable<ISExpression>);
        // console.log('this.firstExpression instanceof Variable<ISExpression> =', this.firstExpression instanceof Variable);
        // if (firstExprAsVariable === undefined || localEnvironment.isDefined(firstExprAsVariable)) {
        // if ((this.firstExpression instanceof Variable<ISExpression>) || localEnvironment.isDefined(firstExprAsVariable)) {
        if (!(this.firstExpression instanceof variable_1.Variable) ||
            localEnvironment.isDefined(this.firstExpression)) {
            const firstExprValue = this.firstExpression.evaluate(localEnvironment, globalInfo);
            // firstExprValue = DeThunkSExpression(firstExprValue, globalInfo);
            // console.log('firstExprValue as ICallableSExpression =', firstExprValue as ICallableSExpression);
            // console.log('firstExprValue instanceof ICallableSExpression =', (firstExprValue instanceof ICallableSExpression));
            // console.log('firstExprValue.isPrimOp() =', firstExprValue.isPrimOp());
            // console.log('firstExprValue.isClosure() =', firstExprValue.isClosure());
            const callableSExpr = firstExprValue;
            if (callableSExpr === undefined) {
                throw new evaluation_exception_1.EvaluationException('EvaluableExpression.evaluate() : FirstExpression is not a callable S-Expression');
            }
            // console.log('firstExprValue is callable. Calling it...');
            return callableSExpr.call(this.expressionList, localEnvironment, globalInfo);
        }
        // throw new EvaluationException('EvaluableExpression.evaluate() : This expression is either bad or not yet supported');
        const firstExprValueX = this.firstExpression.evaluate(localEnvironment, globalInfo);
        const firstExprValueIsCallable = firstExprValueX !== undefined;
        throw new evaluation_exception_1.EvaluationException(`EvaluableExpression.evaluate() : Foo. firstExprValueIsCallable: ${firstExprValueIsCallable}`);
    }
}
exports.EvaluableExpression = EvaluableExpression;
