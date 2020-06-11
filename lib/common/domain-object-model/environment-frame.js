// tom-weatherhead/thaw-grammar/src/common/domain-object-model/environment-frame.ts
'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
exports.EnvironmentFrame = void 0;
const argument_exception_1 = require("../exceptions/argument-exception");
const key_not_found_exception_1 = require("../exceptions/key-not-found-exception");
class EnvironmentFrame {
    constructor(next = null) {
        this.dict = new Map();
        this.next = next;
    }
    isDefined(key) {
        if (this.dictionaryContainsKey(key)) {
            return true;
        }
        if (this.next !== null) {
            return this.next.isDefined(key);
        }
        return false;
    }
    lookup(key) {
        // console.log(`EnvironmentFrame.lookup() : Looking up the value of ${key.name}...`);
        if (this.dictionaryContainsKey(key)) {
            // console.log(`EnvironmentFrame.lookup() : Found the value of ${key.name}.`);
            // Console.WriteLine("Lookup: The value of {0} in {1} environment frame is {2}",
            // 	key, (next != null) ? "a local" : "the global", dict[key]);
            // return this.dict.get(key.name) as T;
            const lookupResult = this.dict.get(key.name);
            // if (lookupResult === undefined) {
            // 	console.error('Error in EnvironmentFrame.lookup() : lookupResult is defined, but is not of the expected type.');
            // }
            return lookupResult;
        }
        if (this.next !== null) {
            return this.next.lookup(key);
        }
        throw new key_not_found_exception_1.KeyNotFoundException(`EnvironmentFrame.lookup() : No value found for variable ${key.name}.`);
    }
    add(key, value) {
        this.dict.set(key.name, value);
    }
    addBubbleDown(key, value) {
        // console.log(`EnvironmentFrame<T>.AddBubbleDown() : var is ${key.name}; value is ${value}`);
        // if (value === undefined) {
        // 	console.log('Warning in EnvironmentFrame.addBubbleDown() : The value being added is falsy.');
        // }
        if (!this.dictionaryContainsKey(key) && this.next !== null) {
            this.next.addBubbleDown(key, value); // Bubble down towards the global environment.
        }
        else {
            // Bug fix: Before 2013/12/04, the "else" above was absent, and the code below was executed unconditionally.
            // Console.WriteLine("AddBubbleDown: The new value of {0} in {1} environment frame is {2}",
            // 	key, (next != null) ? "a local" : "the global", value);
            this.add(key, value);
        }
    }
    compose(keys, values) {
        if (keys.length !== values.length) {
            throw new argument_exception_1.ArgumentException('Environment.Compose() : The keys list and the values list have different lengths.', '');
        }
        keys.forEach((key, i) => {
            this.add(key, values[i]);
        });
    }
    dictionaryContainsKey(key) {
        return typeof this.dict.get(key.name) !== 'undefined';
    }
}
exports.EnvironmentFrame = EnvironmentFrame;
