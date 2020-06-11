// tom-weatherhead/thaw-grammar/src/common/domain-object-model/global-info-base.ts
'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
exports.GlobalInfoBase = void 0;
const argument_exception_1 = require("../exceptions/argument-exception");
const environment_frame_1 = require("./environment-frame");
// import { IGlobalInfoOps } from './iglobal-info-ops';
// export abstract class GlobalInfoBase<T> implements IGlobalInfo<T>, IGlobalInfoOps {
class GlobalInfoBase {
    constructor() {
        this.globalEnvironment = new environment_frame_1.EnvironmentFrame(null);
        this.functionDefinitions = new Map();
        this.dynamicScoping = false;
        this.debug = false;
        this.printedText = '';
    }
    initialize() {
        // Restore the state of the interpreter to its newly-created state.
        this.globalEnvironment.dict.clear();
        this.functionDefinitions.clear();
        // if (MacroDefinitions !== null) {
        // 	MacroDefinitions.clear();
        // }
        this.setScoping(false); // Set the scope rules to "static" rather than "dynamic".
        this.setDebug(false); // Turn debug mode off.
        this.clearPrintedText();
    }
    // public abstract valueIsFloat(value: T): boolean;
    // public abstract valueAsFloat(value: T): number;
    // public abstract floatAsValue(value: number): T;
    // public abstract valueIsNumber(value: T): boolean;
    // public abstract valueAsNumber(value: T): number;
    // public abstract numberAsIntegerValue(value: number): T; // Convert to the language's native integer data type
    // public abstract numberAsFloatValue(value: number): T; // Convert to the language's native floating-point number data type
    valueIsFalse(value) {
        return value === this.falseValue;
    }
    loadPreset(presetName) {
        throw new argument_exception_1.ArgumentException(`GlobalInfoBase<T>.loadPreset() : Unknown preset name '${presetName}'.`, 'presetName');
    }
    loadPresets() { }
    setScoping(dynamicScoping) {
        this.dynamicScoping = dynamicScoping;
    }
    setDebug(debug) {
        this.debug = debug;
    }
    clearPrintedText() {
        this.printedText = '';
    }
    print(evaluatedArguments) {
        this.printedText =
            this.printedText +
                evaluatedArguments
                    .map((evaluatedArgument) => `${evaluatedArgument}`)
                    .join(', ') +
                '\n';
    }
    getPrintedText() {
        return this.printedText;
    }
}
exports.GlobalInfoBase = GlobalInfoBase;
