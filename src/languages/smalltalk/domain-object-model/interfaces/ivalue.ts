// tom-weatherhead/thaw-grammar/src/languages/smalltalk/domain-object-model/interfaces/ivalue.ts

export interface ISmalltalkValue {
	isInteger: boolean;

	toInteger(): number | undefined;
	toFloat(): number | undefined;
	toStringX(): string | undefined;
}
