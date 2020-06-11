"use strict";
// import { ITokenizer } from 'thaw-lexical-analyzer';
Object.defineProperty(exports, "__esModule", { value: true });
exports.MinimalLanguageGlobalInfo = void 0;
const global_info_base_1 = require("../../../common/domain-object-model/global-info-base");
class MinimalLanguageGlobalInfo extends global_info_base_1.GlobalInfoBase {
    // constructor(tokenizer: ITokenizer, parser: IParser) {
    // 	super(tokenizer, parser);
    // }
    constructor() {
        super();
        this.trueValueForAccessor = 1;
        this.falseValueForAccessor = 0;
    }
    get falseValue() {
        return this.falseValueForAccessor;
    }
    get trueValue() {
        return this.trueValueForAccessor;
    }
    valueIsFalse(value) {
        return value === this.falseValue;
    }
    valueIsInteger(value) {
        return true;
    }
    valueAsInteger(value) {
        return value;
    }
    integerAsValue(value) {
        return value;
    }
}
exports.MinimalLanguageGlobalInfo = MinimalLanguageGlobalInfo;
