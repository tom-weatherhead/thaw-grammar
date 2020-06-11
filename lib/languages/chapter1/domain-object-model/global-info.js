"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.Chapter1GlobalInfo = void 0;
const global_info_base_1 = require("../../../common/domain-object-model/global-info-base");
class Chapter1GlobalInfo extends global_info_base_1.GlobalInfoBase {
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
exports.Chapter1GlobalInfo = Chapter1GlobalInfo;
