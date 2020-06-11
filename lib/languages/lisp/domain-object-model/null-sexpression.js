// tom-weatherhead/thaw-grammar/src/languages/lisp/domain-object-model/null-sexpression.ts
'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
exports.NullSExpression = void 0;
const sexpression_base_1 = require("./sexpression-base");
class NullSExpression extends sexpression_base_1.SExpressionBase {
    toString() {
        return '()';
    }
    // public override bool Equals(object obj)
    // {
    // 	if (object.ReferenceEquals(this, obj))
    // 	{
    // 		return true;
    // 	}
    // 	return obj is NullSExpression;
    // }
    // public override int GetHashCode()
    // {
    // 	return 0;
    // }
    isNull() {
        return true;
    }
}
exports.NullSExpression = NullSExpression;
