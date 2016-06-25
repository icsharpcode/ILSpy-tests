// Copyright (c) 2016 Daniel Grunwald
// 
// Permission is hereby granted, free of charge, to any person obtaining a copy of this
// software and associated documentation files (the "Software"), to deal in the Software
// without restriction, including without limitation the rights to use, copy, modify, merge,
// publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons
// to whom the Software is furnished to do so, subject to the following conditions:
// 
// The above copyright notice and this permission notice shall be included in all copies or
// substantial portions of the Software.
// 
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
// INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
// PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE
// FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
// OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
// DEALINGS IN THE SOFTWARE.

using System;
using System.Collections.Generic;
using System.Linq;
using Mono.Cecil;
using Mono.Cecil.Cil;

namespace TestCaseGenerator
{
	class Program
	{
		static Random random = new Random(0);
		static MethodDefinition method;
		static ModuleDefinition module;
		static List<StackType> parameterStackTypes;
		static ILProcessor processor;
		
		static readonly Type[] csharpIntegerTypes = {
			typeof(sbyte),
			typeof(byte),
			typeof(short),
			typeof(ushort),
			typeof(int),
			typeof(uint),
			typeof(long),
			typeof(ulong)
		};
		
		static readonly Type[] integerTypes = {
			typeof(bool),
			typeof(sbyte),
			typeof(byte),
			typeof(short),
			typeof(ushort),
			typeof(char),
			typeof(int),
			typeof(uint),
			typeof(IntPtr),
			typeof(UIntPtr),
			typeof(long),
			typeof(ulong),
		};
		
		static readonly List<TypeDefinition> enumTypes = new List<TypeDefinition>();
		
		public static void Main(string[] args)
		{
			module = ModuleDefinition.CreateModule("TestCase-1", ModuleKind.Console);
			
			foreach (var type in csharpIntegerTypes) {
				var enumDefinition = new TypeDefinition("Enums", type.Name, TypeAttributes.Public | TypeAttributes.Sealed);
				enumDefinition.BaseType = module.ImportReference(typeof(Enum));
				enumDefinition.Fields.Add(new FieldDefinition("__value", FieldAttributes.Public | FieldAttributes.SpecialName, module.ImportReference(type)));
				enumTypes.Add(enumDefinition);
				module.Types.Add(enumDefinition);
			}
			
			var typeDefinition = new TypeDefinition(string.Empty, "TestCases", TypeAttributes.Public);
			typeDefinition.BaseType = module.TypeSystem.Object;
			module.Types.Add(typeDefinition);
			
			var main = new MethodDefinition("Main", MethodAttributes.Public | MethodAttributes.Static, module.TypeSystem.Void);
			var mainProcessor = main.Body.GetILProcessor();
			typeDefinition.Methods.Add(main);
			
			for (int i = 0; i < 100; i++) {
				method = new MethodDefinition("M" + i, MethodAttributes.Public | MethodAttributes.Static, module.TypeSystem.Object);
				parameterStackTypes = new List<StackType>();
				processor = method.Body.GetILProcessor();
				EmitObject();
				processor.Emit(OpCodes.Ret);
				
				processor = mainProcessor;
				EmitCallsInMain();
				typeDefinition.Methods.Add(method);
			}
			
			mainProcessor.Emit(OpCodes.Ret);
			module.EntryPoint = main;
			module.Write($"../../../TestCases/{module.Name}.exe");
		}
		
		static bool RandomBool(double pTrue)
		{
			return random.NextDouble() < pTrue;
		}
		
		static T RandomElement<T>(IReadOnlyList<T> inputs)
		{
			return inputs[random.Next(inputs.Count)];
		}
		
		enum StackType {
			I4,
			I,
			I8,
		}
		
		static TypeReference RandomType(out StackType st)
		{
			if (RandomBool(0.3)) {
				int index = random.Next(enumTypes.Count);
				TypeDefinition td = RandomElement(enumTypes);
				if (td.Name == "Int64" || td.Name == "UInt64") {
					st = StackType.I8;
				} else {
					st = StackType.I4;
				}
				return td;
			} else {
				Type type = RandomElement(integerTypes);
				if (type == typeof(IntPtr) || type == typeof(UIntPtr)) {
					st = StackType.I;
				} else if (type == typeof(long) || type == typeof(ulong)) {
					st = StackType.I8;
				} else {
					st = StackType.I4;
				}
				return module.ImportReference(type);
			}
		}
		
		static TypeReference RandomType(StackType expectedType)
		{
			StackType st;
			TypeReference tr;
			do {
				tr = RandomType(out st);
			} while (st != expectedType);
			return tr;
		}
		
		static void EmitObject()
		{
			StackType st;
			var tr = RandomType(out st);
			if (RandomBool(0.9))
				EmitBinaryIntegerOperation(st);
			else
				EmitInteger(st, allowConstants: true);
			processor.Emit(OpCodes.Box, tr);
		}
		
		static void EmitComplexInteger(StackType st)
		{
			double what = random.NextDouble();
			if (what < 0.9) {
				EmitBinaryIntegerOperation(st);
			} else if (what < 0.95) {
				EmitUnaryIntegerOperation(st, allowConstants: true);
			} else {
				EmitNullaryIntegerOperation(st, allowConstants: true);
			}
		}
		
		static void EmitInteger(StackType st, bool allowConstants)
		{
			double what = random.NextDouble();
			if (what < 0.3) {
				EmitBinaryIntegerOperation(st);
			} else if (what < 0.6) {
				EmitUnaryIntegerOperation(st, allowConstants);
			} else {
				EmitNullaryIntegerOperation(st, allowConstants);
			}
		}

		static readonly OpCode[] binaryIntegerOpCodes = {
			// double entries for some non-throwing opcodes,
			// so that we don't get as many OverflowExceptions
			OpCodes.Add, OpCodes.Add, OpCodes.Add_Ovf, OpCodes.Add_Ovf_Un,
			OpCodes.And, OpCodes.And,
			OpCodes.Div, OpCodes.Div_Un,
			OpCodes.Mul, OpCodes.Mul, OpCodes.Mul_Ovf, OpCodes.Mul_Ovf_Un,
			OpCodes.Or, OpCodes.Or,
			OpCodes.Rem, OpCodes.Rem_Un,
			OpCodes.Shl, OpCodes.Shr, OpCodes.Shr_Un,
			OpCodes.Sub, OpCodes.Sub, OpCodes.Sub_Ovf, OpCodes.Sub_Ovf_Un,
			OpCodes.Xor, OpCodes.Xor,
		};
		
		static readonly OpCode[] comparisonOpCodes = {
			OpCodes.Ceq, OpCodes.Cgt, OpCodes.Cgt_Un, OpCodes.Clt, OpCodes.Clt_Un,
		};
		
		static void EmitBinaryIntegerOperation(StackType st)
		{
			if (st == StackType.I4 && RandomBool(0.3)) {
				StackType sourceType = StackType.I4;
				if (RandomBool(0.1))
					sourceType = StackType.I8;
				else if (RandomBool(0.1))
					sourceType = StackType.I;
				EmitInteger(sourceType, allowConstants: true);
				EmitInteger(sourceType, allowConstants: true);
				processor.Emit(RandomElement(comparisonOpCodes));
			} else {
				var opCode = RandomElement(binaryIntegerOpCodes);
				bool constantAllowedOnLeft = RandomBool(0.5);
				EmitInteger(st, constantAllowedOnLeft);
				if (opCode == OpCodes.Shl || opCode == OpCodes.Shr || opCode == OpCodes.Shr_Un) {
					// these don't support I8 on the RHS
					EmitInteger(RandomBool(0.75) ? StackType.I4 : StackType.I, !constantAllowedOnLeft);
				} else {
					EmitInteger(st, !constantAllowedOnLeft);
				}
				processor.Emit(opCode);
			}
		}
		
		static readonly OpCode[] unaryIntegerOpCodes = {
			OpCodes.Neg, OpCodes.Not,
		};
		
		static readonly OpCode[] convToI4 = {
			OpCodes.Conv_I1, OpCodes.Conv_I2, OpCodes.Conv_I4,
			OpCodes.Conv_U1, OpCodes.Conv_U2, OpCodes.Conv_U4,
			OpCodes.Conv_Ovf_I1, OpCodes.Conv_Ovf_I2, OpCodes.Conv_Ovf_I4,
			OpCodes.Conv_Ovf_U1, OpCodes.Conv_Ovf_U2, OpCodes.Conv_Ovf_U4,
			OpCodes.Conv_Ovf_I1_Un, OpCodes.Conv_Ovf_I2_Un, OpCodes.Conv_Ovf_I4_Un,
			OpCodes.Conv_Ovf_U1_Un, OpCodes.Conv_Ovf_U2_Un, OpCodes.Conv_Ovf_U4_Un,
		};
		
		static readonly OpCode[] convToI = {
			OpCodes.Conv_I, OpCodes.Conv_U,
			OpCodes.Conv_Ovf_I, OpCodes.Conv_Ovf_U,
			OpCodes.Conv_Ovf_I_Un, OpCodes.Conv_Ovf_U_Un,
		};
		
		static readonly OpCode[] convToI8 = {
			OpCodes.Conv_I8, OpCodes.Conv_U8,
			OpCodes.Conv_Ovf_I8, OpCodes.Conv_Ovf_U8,
			OpCodes.Conv_Ovf_I8_Un, OpCodes.Conv_Ovf_U8_Un,
		};
		
		static bool ConvOpCodeHasOverflowCheck(OpCode opCode)
		{
			switch (opCode.Code) {
				case Code.Conv_I1:
				case Code.Conv_U1:
				case Code.Conv_I2:
				case Code.Conv_U2:
				case Code.Conv_I4:
				case Code.Conv_U4:
				case Code.Conv_I:
				case Code.Conv_U:
				case Code.Conv_I8:
				case Code.Conv_U8:
					return false;
				default:
					return true;
			}
		}
		
		static void EmitUnaryIntegerOperation(StackType targetType, bool allowConstants)
		{
			double what = random.NextDouble();
			if (what < 0.75) {
				// Conversion
				StackType sourceType = StackType.I4;
				if (what < 0.1)
					sourceType = StackType.I;
				else if (what < 0.2)
					sourceType = StackType.I8;
				OpCode convOpCode;
				switch (targetType) {
					case StackType.I4:
						convOpCode = RandomElement(convToI4);
						break;
					case StackType.I:
						convOpCode = RandomElement(convToI);
						break;
					case StackType.I8:
						convOpCode = RandomElement(convToI8);
						break;
					default:
						throw new ArgumentOutOfRangeException();
				}
				EmitInteger(sourceType, allowConstants && !ConvOpCodeHasOverflowCheck(convOpCode));
				processor.Emit(convOpCode);
			} else {
				var opCode = RandomElement(unaryIntegerOpCodes);
				EmitInteger(targetType, allowConstants && opCode != OpCodes.Neg);
				processor.Emit(opCode);
			}
		}
		
		static readonly int[] commonIntegers = {
			0, 2, 100,
			sbyte.MinValue, sbyte.MaxValue,
			byte.MaxValue,
			short.MinValue, short.MaxValue,
			ushort.MaxValue,
			int.MinValue, int.MaxValue,
		};
		
		static void EmitNullaryIntegerOperation(StackType st, bool allowConstants)
		{
			if (!allowConstants || RandomBool(0.5)) {
				// declare a parameter instead of using a constant
				method.Parameters.Add(new ParameterDefinition(RandomType(st)));
				parameterStackTypes.Add(st);
				processor.Emit(OpCodes.Ldarg, method.Parameters.Last());
			} else {
				EmitConstantInteger(st);
			}
		}
		
		static void EmitConstantInteger(StackType st)
		{
			switch (st) {
				case StackType.I4:
					if (RandomBool(0.9))
						processor.Emit(OpCodes.Ldc_I4, unchecked(RandomElement(commonIntegers) + random.Next(-1, 2)));
					else
						processor.Emit(OpCodes.Ldc_I4, random.Next());
					break;
				case StackType.I:
					if (RandomBool(0.5)) {
						goto case StackType.I4;
					} else {
						EmitConstantInteger(StackType.I8);
						processor.Emit(OpCodes.Conv_I);
						break;
					}
				case StackType.I8:
					if (RandomBool(0.75))
						processor.Emit(OpCodes.Ldc_I8, (long)unchecked(RandomElement(commonIntegers) + random.Next(-1, 2)));
					else if (RandomBool(0.2))
						processor.Emit(OpCodes.Ldc_I8, RandomBool(0.5) ? long.MaxValue : long.MinValue);
					else
						processor.Emit(OpCodes.Ldc_I8, (long)random.Next() << 32 | (uint)random.Next());
					break;
				default:
					throw new ArgumentOutOfRangeException();
			}
		}

		static void EmitCallsInMain()
		{
			var writeLine = module.ImportReference(typeof(Console).GetMethod("WriteLine", new[] { typeof(object) }));
			processor.Body.InitLocals = true;
			processor.Body.Variables.Add(new VariableDefinition(module.TypeSystem.Object));
			var v = processor.Body.Variables.Last();
			processor.Emit(OpCodes.Ldstr, method.Name);
			processor.Emit(OpCodes.Call, writeLine);
			for (int i = 0; i < 10; i++) {
				var nop = processor.Create(OpCodes.Nop);
				int startTry = processor.Body.Instructions.Count;
				foreach (var st in parameterStackTypes) {
					EmitConstantInteger(st);
				}
				processor.Emit(OpCodes.Call, method);
				processor.Emit(OpCodes.Stloc, v);
				processor.Emit(OpCodes.Leave, nop);
				int endTry = processor.Body.Instructions.Count;
				processor.Emit(OpCodes.Callvirt, module.ImportReference(typeof(object).GetMethod("GetType")));
				processor.Emit(OpCodes.Callvirt, module.ImportReference(typeof(Type).GetProperty("Name").GetGetMethod()));
				processor.Emit(OpCodes.Stloc, v);
				processor.Emit(OpCodes.Leave, nop);
				int endCatch = processor.Body.Instructions.Count;
				processor.Append(nop);
				processor.Emit(OpCodes.Ldloc, v);
				processor.Emit(OpCodes.Call, writeLine);
				processor.Body.ExceptionHandlers.Add(
					new ExceptionHandler(ExceptionHandlerType.Catch) {
						CatchType = module.ImportReference(typeof(Exception)),
						TryStart = processor.Body.Instructions[startTry],
						TryEnd = processor.Body.Instructions[endTry],
						HandlerStart = processor.Body.Instructions[endTry],
						HandlerEnd = processor.Body.Instructions[endCatch]
					});
				if (method.Parameters.Count == 0)
					break;
			}
		}
	}
}