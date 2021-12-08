// src/common/global-info-factory.ts

import { IParser, ITokenizer, LanguageSelector } from 'thaw-interpreter-types';

import { ArgumentException } from 'thaw-interpreter-core';

import { IGlobalInfoForInterpreter } from './domain-object-model/iglobal-info';

import { APLGlobalInfo } from '../languages/apl/domain-object-model/global-info';
import { Chapter1GlobalInfo } from '../languages/chapter1/domain-object-model/global-info';
import { CLUGlobalInfo } from '../languages/clu/domain-object-model/global-info';
import { LISPGlobalInfo } from '../languages/lisp/domain-object-model/lisp-global-info';
import { MinimalLanguageGlobalInfo } from '../languages/minimal/domain-object-model/global-info';
import { PrologGlobalInfo } from '../languages/prolog/domain-object-model/prolog-global-info';
import { SASLGlobalInfo } from '../languages/sasl/domain-object-model/global-info';
import { SchemeGlobalInfo } from '../languages/scheme/domain-object-model/scheme-global-info';
import { SmalltalkGlobalInfo } from '../languages/smalltalk/domain-object-model/global-info';
// import { GlobalInfo } from '../languages//domain-object-model/global-info';

export function createGlobalInfo(
	ls: LanguageSelector,
	options: { parser?: IParser; tokenizer?: ITokenizer } = {}
): IGlobalInfoForInterpreter {
	switch (ls) {
		case LanguageSelector.MinimalLanguage:
			return new MinimalLanguageGlobalInfo(options);

		// case LanguageSelector.Micro:
		// 	return new MicroGlobalInfo(options);

		case LanguageSelector.Chapter1:
			return new Chapter1GlobalInfo(options);

		case LanguageSelector.LISP:
			return new LISPGlobalInfo(options);

		case LanguageSelector.APL:
			return new APLGlobalInfo(options);

		case LanguageSelector.Scheme:
			return new SchemeGlobalInfo(options);

		case LanguageSelector.SASL:
			return new SASLGlobalInfo(options);

		case LanguageSelector.CLU:
			return new CLUGlobalInfo(options);

		case LanguageSelector.Smalltalk:
			return new SmalltalkGlobalInfo(options);

		// case LanguageSelector.Prolog: // Kamin's LISP-like Prolog syntax
		case LanguageSelector.Prolog2: // 'Real' Prolog syntax
			return new PrologGlobalInfo(options);

		// case LanguageSelector.Inference:
		// 	return new InferenceGlobalInfo(options);
		//
		// case LanguageSelector.JSON:
		// 	return new JSONGlobalInfo(options);
		//
		// case LanguageSelector.LambdaCalculus:
		// 	return new LambdaCalculusGlobalInfo(options);
		//
		// case LanguageSelector.LambdaCalculusWithAugmentedSyntax:
		// 	return new LambdaCalculusWithAugmentedSyntaxGlobalInfo(options);
		//
		// case LanguageSelector.LambdaCalculusIntegerExtension:
		// 	return new LambdaCalculusIntegerExtensionGlobalInfo(options);

		default:
			throw new ArgumentException(
				`createGlobalInfo(): Unsupported language selector ${ls} ${LanguageSelector[ls]}`,
				'ls'
			);
	}
}
