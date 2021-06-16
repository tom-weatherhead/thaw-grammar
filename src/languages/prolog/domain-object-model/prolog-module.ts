// tom-weatherhead/thaw-grammar/src/languages/prolog/domain-object-model/prolog-module.ts

import { PrologClause } from './prolog-clause';
import { StringIntKey } from './string-int-key';

export class PrologModule {
	public readonly ExportList: StringIntKey[] = [];
	public readonly ImportList = new Map<string, PrologModule>();
	public readonly ClauseList: PrologClause[] = [];

	constructor(exportList: StringIntKey[] | undefined = undefined) {
		if (typeof exportList !== 'undefined') {
			this.ExportList = exportList;
		}
	}

	public toString(): string {
		return 'PrologModule.toString()';
	}

	// public void Clear() {
	//     ImportList.Clear();
	//     ClauseList.Clear();
	// }
}
