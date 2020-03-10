package jvm;

import java.NativeArray;

@:native("haxe.jvm.Function")
@:nativeGen
extern class Function implements IFunction {
	public function new():Void;
	public function invokeDynamic(args:NativeArray<Dynamic>):Dynamic;
	public function equals(other:java.lang.Object):Bool;
	public function invokeObject(arg1:java.lang.Object):java.lang.Object;
}

@:native("haxe.jvm.IFunction")
@:nativeGen
extern interface IFunction {}
