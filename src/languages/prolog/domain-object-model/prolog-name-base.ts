// tom-weatherhead/thaw-grammar/src/languages/prolog/domain-object-model/prolog-name-base.ts

export class PrologNameBase {
	public readonly Name: string;

	constructor(name: string) {
		if (!name) {
			throw new Error(
				'A PrologNameBase cannot have a null or empty name'
			);
		}

		this.Name = name;
	}

	public toString(): string {
		return this.Name;
	}

	public equals(obj: unknown): boolean {
		const otherNameBase = obj as PrologNameBase;

		return (
			typeof otherNameBase !== 'undefined' &&
			this.Name === otherNameBase.Name
		);
	}
}
