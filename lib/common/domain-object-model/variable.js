// tom-weatherhead/thaw-grammar/src/common/domain-object-model/variable.ts
'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
exports.Variable = void 0;
const argument_null_exception_1 = require("../exceptions/argument-null-exception");
const evaluation_exception_1 = require("../exceptions/evaluation-exception");
class Variable {
    constructor(name, line, column) {
        if (!name) {
            throw new argument_null_exception_1.ArgumentNullException('A Variable cannot have a null or empty name', 'name');
        }
        // Console.WriteLine("Creating a Variable named '{0}'.", name);
        // if (name.StartsWith("Inference")) {
        // 	throw new Exception(string.Format("Error: Creating variable named '{0}'.", name));
        // }
        this.name = name;
        this.line = line;
        this.column = column;
    }
    toString() {
        return this.name;
    }
    evaluate(localEnvironment, globalInfo) {
        try {
            return localEnvironment.lookup(this);
        }
        catch (KeyNotFoundException) {
            throw new evaluation_exception_1.EvaluationException(`Variable<T>.Evaluate() : No value found for variable ${this.name}`, this.line, this.column);
        }
    }
}
exports.Variable = Variable;
