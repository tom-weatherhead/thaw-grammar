// string-int-key.ts

export class StringIntKey {
	public static from(input: string): StringIntKey {
		const regex = /(.+)\/([0-9]+)/;
		const match = input.match(regex);

		if (!match || typeof match === 'undefined' || match.length !== 2) {
			throw new Error('StringIntKey.from() parse error.');
		}

		const match1 = match[1];
		const match2 = parseInt(match[2]);

		return new StringIntKey(match1, match2);
	}

	private readonly str: string;
	private readonly n: number;

	constructor(str: string, n: number) {
		// if (string.IsNullOrEmpty(str))
		if (!str) {
			throw new Error('StringIntKey constructor: str is null or empty.');
		}

		this.str = str;
		this.n = n;
	}

	public toString(): string {
		return `${this.str}/${this.n}`;
	}

	public Equals(obj: unknown): boolean {
		// if (object.ReferenceEquals(this, obj))
		// {
		//     return true;
		// }

		const otherKey = obj as StringIntKey;

		return (
			typeof otherKey !== 'undefined' &&
			otherKey.str === this.str &&
			otherKey.n === this.n
		);
	}

	// public override int GetHashCode()
	// {
	//     return str.GetHashCode() * 101 + n.GetHashCode();
	// }
}
