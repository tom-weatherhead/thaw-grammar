// tom-weatherhead/thaw-grammar/src/languages/smalltalk/domain-object-model/interfaces/ivalue.ts

// public interface ISmalltalkValue
// {
// 	SmalltalkClass Owner { get; }
// 	string GetTypename();
// 	bool IsNumber();
// 	bool IsSymbol();
// 	bool IsCharacter();
// 	bool IsString();
// 	bool IsObject();
// 	bool IsArray();
// }

// public abstract class SmalltalkValueBase : ISmalltalkValue, ISmalltalkExpression
// {
// 	public SmalltalkClass Owner { get; private set; }
//
// 	protected SmalltalkValueBase(SmalltalkClass owner)
// 	{
// 		Owner = owner;
// 	}
//
// 	public abstract string GetTypename();
//
// 	public virtual bool IsNumber()
// 	{
// 		return false;
// 	}
//
// 	public virtual bool IsSymbol()
// 	{
// 		return false;
// 	}
//
// 	public virtual bool IsCharacter()
// 	{
// 		return false;
// 	}
//
// 	public virtual bool IsString()
// 	{
// 		return false;
// 	}
//
// 	public virtual bool IsObject()
// 	{
// 		return false;
// 	}
//
// 	public virtual bool IsArray()
// 	{
// 		return false;
// 	}
//
// 	public ISmalltalkValue Evaluate(SmalltalkEnvironmentFrame localEnvironment, ISmalltalkValue receiver, SmalltalkClass c, SmalltalkGlobalInfo globalInfo)
// 	{
// 		return this;
// 	}
// }

// export interface ISmalltalkValue {
// 	isInteger: boolean;
// 	owner: ISmalltalkClass;
//
// 	getTypename(): string;
// 	isNumber(): boolean;
// 	isSymbol(): boolean;
// 	isCharacter(): boolean;
// 	isString(): boolean;
// 	isObject(): boolean;
// 	isArray(): boolean;
//
// 	toInteger(): number | undefined;
// 	toFloat(): number | undefined;
// 	toStringX(): string | undefined;
// }
