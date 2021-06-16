// prolog-name-base.ts

export class PrologNameBase {
	public readonly Name: string;

	constructor(name: string) {
		// if (string.IsNullOrEmpty(name))
		// {
		//     throw new ArgumentNullException("name", "A PrologNameBase cannot have a null or empty name");
		// }

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

	public Equals(obj: unknown): boolean {
		// if (obj == null || !GetType().Equals(obj.GetType()))
		// {
		//     return false;
		// }

		const otherNameBase = obj as PrologNameBase;

		return (
			typeof otherNameBase !== 'undefined' &&
			this.Name === otherNameBase.Name
		);
	}

	// public override int GetHashCode()
	// {
	//     return Name.GetHashCode();
	// }
}
