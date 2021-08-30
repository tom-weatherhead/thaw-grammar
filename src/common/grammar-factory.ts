// tom-weatherhead/thaw-grammar/src/common/grammar-factory.ts

import { IGrammar, LanguageSelector } from 'thaw-interpreter-types';

import { APLGrammar } from '../languages/apl/apl-grammar';
import { Chapter1Grammar } from '../languages/chapter1/chapter1-grammar';
import { CluGrammar } from '../languages/clu/clu-grammar';
import { InferenceGrammar } from '../languages/inference/inference-grammar';
import { JSONGrammar } from '../languages/json/json-grammar';
import { LambdaCalculusGrammar } from '../languages/lambda-calculus/lambda-calculus-grammar';
import { LambdaCalculusIntegerExtensionGrammar } from '../languages/lambda-calculus-integer-extension/lambda-calculus-integer-extension-grammar';
import { LISPGrammar } from '../languages/lisp/lisp-grammar';
import { MicroGrammar } from '../languages/micro/micro-grammar';
import { MinimalLanguageGrammar } from '../languages/minimal/minimal-language-grammar';
import { PrologGrammar } from '../languages/prolog/prolog-grammar';
import { SASLGrammar } from '../languages/sasl/sasl-grammar';
import { SchemeGrammar } from '../languages/scheme/scheme-grammar';
import { SmalltalkGrammar } from '../languages/smalltalk/smalltalk-grammar';

import { ArgumentException } from './exceptions/argument-exception';

export function createGrammar(ls: LanguageSelector): IGrammar {
	switch (ls) {
		case LanguageSelector.MinimalLanguage:
			return new MinimalLanguageGrammar();

		case LanguageSelector.Micro:
			return new MicroGrammar();

		case LanguageSelector.Chapter1:
			return new Chapter1Grammar();

		case LanguageSelector.LISP:
			return new LISPGrammar();

		case LanguageSelector.APL:
			return new APLGrammar();

		case LanguageSelector.Scheme:
			return new SchemeGrammar();

		case LanguageSelector.SASL:
			return new SASLGrammar();

		case LanguageSelector.CLU:
			return new CluGrammar();

		case LanguageSelector.Smalltalk:
			return new SmalltalkGrammar();

		// case LanguageSelector.Prolog:
		case LanguageSelector.Prolog2: // 'Real' Prolog
			return new PrologGrammar();

		case LanguageSelector.Inference:
			return new InferenceGrammar();

		case LanguageSelector.JSON:
			return new JSONGrammar();

		case LanguageSelector.LambdaCalculus:
			return new LambdaCalculusGrammar();

		case LanguageSelector.LambdaCalculusIntegerExtension:
			return new LambdaCalculusIntegerExtensionGrammar();

		default:
			throw new ArgumentException(
				`createGrammar(): Unsupported language selector ${ls} ${LanguageSelector[ls]}`,
				'ls'
			);
	}
}
