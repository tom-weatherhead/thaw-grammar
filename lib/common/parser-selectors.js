// tom-weatherhead/thaw-grammar/src/common/parser-selectors.ts
'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
exports.ParserSelector = void 0;
var ParserSelector;
(function (ParserSelector) {
    ParserSelector[ParserSelector["LL1"] = 0] = "LL1";
    ParserSelector[ParserSelector["LR0"] = 1] = "LR0";
    ParserSelector[ParserSelector["LR1"] = 2] = "LR1";
    ParserSelector[ParserSelector["SLR1"] = 3] = "SLR1";
    ParserSelector[ParserSelector["LALR1"] = 4] = "LALR1";
})(ParserSelector = exports.ParserSelector || (exports.ParserSelector = {}));
