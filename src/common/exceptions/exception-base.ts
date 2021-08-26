// tom-weatherhead/thaw-grammar/src/common/exceptions/exception-base.ts

// export abstract class ExceptionBase {
// 	public readonly message: string;
// 	public readonly line: number;
// 	public readonly column: number;
//
// 	protected constructor(typeName: string, message: string, line = 0, column = 0) {
// 		const lineAndColumnText =
// 			line > 0 && column > 0 ? ` at line ${line}, column ${column}` : '';
//
// 		this.message = `${typeName}${lineAndColumnText}: ${message}`;
// 		this.line = line;
// 		this.column = column;
// 	}
//
// 	public toString(): string {
// 		return this.message;
// 	}
// }
