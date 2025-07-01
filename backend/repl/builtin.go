package repl

import (
	"fmt"
	"strconv"
	"strings"

	"main.go/value"
)

type BuiltInFunction struct {
	Name string
	Exec func(context *ReplContext, args []*Argument) (value.IVOR, bool, string)
}

// implementing ivor

func (b BuiltInFunction) Type() string {
	return value.IVOR_BUILTIN_FUNCTION
}

func (b BuiltInFunction) Value() interface{} {
	return b
}

func (b BuiltInFunction) Copy() value.IVOR {
	return b
}

// * Print Function
func Print(context *ReplContext, args []*Argument) (value.IVOR, bool, string) {
	return PrintCore(context, args, false)
}

func PrintLn(context *ReplContext, args []*Argument) (value.IVOR, bool, string) {
	return PrintCore(context, args, true)
}

func PrintCore(context *ReplContext, args []*Argument, newLine bool) (value.IVOR, bool, string) {

	var output string

	for i, arg := range args {

		// Verificar si es un tipo primitivo O un vector
		if !value.IsPrimitiveType(arg.Value.Type()) &&
			!IsVectorType(arg.Value.Type()) &&
			!IsMatrixType(arg.Value.Type()) &&
			!IsStructType(arg.Value) {
			return value.DefaultNilValue, false, "La función print solo acepta tipos primitivos, vectores y matrices"
		}

		fmt.Printf("DEBUG: Argumento recibido - Nombre: %s, Tipo: %s, Valor Go: %T\n", arg.Name, arg.Value.Type(), arg.Value)

		switch arg.Value.Type() {

		case value.IVOR_BOOL:
			output += strconv.FormatBool(arg.Value.Value().(bool))
		case value.IVOR_INT:
			output += strconv.Itoa(arg.Value.Value().(int))
		case value.IVOR_FLOAT:
			output += strconv.FormatFloat(arg.Value.Value().(float64), 'f', 4, 64) // 4 digits of precision
		case value.IVOR_STRING:
			output += arg.Value.Value().(string)
		case value.IVOR_CHARACTER:
			output += arg.Value.Value().(string)
		case value.IVOR_NIL:
			output += "nil"
		default:
			// Si es vector
			if IsVectorType(arg.Value.Type()) {
				vectorOutput := formatVector(arg.Value.(*VectorValue))
				output += vectorOutput
			} else if IsMatrixType(arg.Value.Type()) {
				matrixOutput := formatMatrix(arg.Value.(*MatrixValue))
				output += matrixOutput
			} else if structVal, ok := arg.Value.(*value.StructValue); ok {
				structOutput := formatStruct(structVal)
				output += structOutput
			} else {
				return value.DefaultNilValue, false, "Tipo no soportado para print: " + arg.Value.Type()
			}
		}

		// Add a space between each argument
		if i < len(args)-1 {
			output += " "
		}
	}

	if newLine {
		context.Console.Print(output + "\n") // println agrega doble salto si así lo deseas
	} else {
		context.Console.Print(output)
	}

	return value.DefaultNilValue, true, ""
}

func formatMatrix(matrix *MatrixValue) string {
	if len(matrix.Items) == 0 {
		return "[ ]"
	}

	var result strings.Builder
	result.WriteString("[ ")

	for i, row := range matrix.Items {
		result.WriteString("[ ")
		for j, item := range row {
			switch item.Type() {
			case value.IVOR_BOOL:
				result.WriteString(strconv.FormatBool(item.Value().(bool)))
			case value.IVOR_INT:
				result.WriteString(strconv.Itoa(item.Value().(int)))
			case value.IVOR_FLOAT:
				result.WriteString(strconv.FormatFloat(item.Value().(float64), 'f', 4, 64))
			case value.IVOR_STRING:
				result.WriteString(item.Value().(string))
			case value.IVOR_CHARACTER:
				result.WriteString(item.Value().(string))
			case value.IVOR_NIL:
				result.WriteString("nil")
			default:
				result.WriteString(item.Type())
			}
			if j < len(row)-1 {
				result.WriteString(" ")
			}
		}
		result.WriteString(" ]")
		if i < len(matrix.Items)-1 {
			result.WriteString(" ")
		}
	}

	result.WriteString(" ]")
	return result.String()
}

// Función auxiliar para formatear vectores
func formatVector(vector *VectorValue) string {
	if len(vector.InternalValue) == 0 {
		return "[ ]"
	}

	var result strings.Builder
	result.WriteString("[ ")

	for i, item := range vector.InternalValue {
		switch item.Type() {
		case value.IVOR_BOOL:
			result.WriteString(strconv.FormatBool(item.Value().(bool)))
		case value.IVOR_INT:
			result.WriteString(strconv.Itoa(item.Value().(int)))
		case value.IVOR_FLOAT:
			result.WriteString(strconv.FormatFloat(item.Value().(float64), 'f', 4, 64))
		case value.IVOR_STRING:
			result.WriteString(item.Value().(string))
		case value.IVOR_CHARACTER:
			result.WriteString(item.Value().(string))
		case value.IVOR_NIL:
			result.WriteString("nil")
		default:
			// Para vectores anidados u otros tipos
			if IsVectorType(item.Type()) {
				result.WriteString(formatVector(item.(*VectorValue)))
			} else {
				result.WriteString(item.Type()) // Mostrar el tipo si no se puede formatear
			}
		}

		// Agregar espacio entre elementos (excepto el último)
		if i < len(vector.InternalValue)-1 {
			result.WriteString(" ")
		}
	}

	result.WriteString(" ]")
	return result.String()
}

func formatStruct(structVal *value.StructValue) string {
	if structVal == nil || structVal.Instance == nil {
		return "nil"
	}

	var result strings.Builder
	result.WriteString(structVal.Instance.StructName)
	result.WriteString("{")

	first := true
	for fieldName, fieldValue := range structVal.Instance.Fields {
		if !first {
			result.WriteString(", ")
		}
		first = false

		result.WriteString(fieldName)
		result.WriteString(": ")

		// Formatear el valor del campo según su tipo
		switch fieldValue.Type() {
		case value.IVOR_BOOL:
			result.WriteString(strconv.FormatBool(fieldValue.Value().(bool)))
		case value.IVOR_INT:
			result.WriteString(strconv.Itoa(fieldValue.Value().(int)))
		case value.IVOR_FLOAT:
			result.WriteString(strconv.FormatFloat(fieldValue.Value().(float64), 'f', 4, 64))
		case value.IVOR_STRING:
			result.WriteString("\"" + fieldValue.Value().(string) + "\"")
		case value.IVOR_CHARACTER:
			result.WriteString("'" + fieldValue.Value().(string) + "'")
		case value.IVOR_NIL:
			result.WriteString("nil")
		default:
			// Para estructuras anidadas
			if nestedStruct, ok := fieldValue.(*value.StructValue); ok {
				result.WriteString(formatStruct(nestedStruct))
			} else if IsVectorType(fieldValue.Type()) {
				result.WriteString(formatVector(fieldValue.(*VectorValue)))
			} else if IsMatrixType(fieldValue.Type()) {
				result.WriteString(formatMatrix(fieldValue.(*MatrixValue)))
			} else {
				result.WriteString("[" + fieldValue.Type() + "]")
			}
		}
	}

	result.WriteString("}")
	return result.String()
}

// * Atoi Function

func Atoi(context *ReplContext, args []*Argument) (value.IVOR, bool, string) {

	if len(args) != 1 {
		return value.DefaultNilValue, false, "La función int solo acepta un argumento"
	}

	argValue := args[0].Value

	if !(argValue.Type() == value.IVOR_STRING || argValue.Type() == value.IVOR_FLOAT) {
		return value.DefaultNilValue, false, "La función Int solo acepta un argumento de tipo string o float"
	}

	if argValue.Type() == value.IVOR_STRING {
		floatValue, err := strconv.ParseFloat(argValue.Value().(string), 64)

		if err != nil {
			return value.DefaultNilValue, false, "No se pudo convertir el valor a int"
		}

		return &value.IntValue{
			InternalValue: int(floatValue),
		}, true, ""
	}

	if argValue.Type() == value.IVOR_FLOAT {
		// truncate the float

		floatValue := argValue.Value().(float64)

		return &value.IntValue{
			InternalValue: int(floatValue),
		}, true, ""
	}

	return value.DefaultNilValue, false, "No se pudo convertir el valor a int"
}

// * Float Function

func ParseFloat(context *ReplContext, args []*Argument) (value.IVOR, bool, string) {

	if len(args) != 1 {
		return value.DefaultNilValue, false, "La función float solo acepta un argumento"
	}

	argValue := args[0].Value

	if !(argValue.Type() == value.IVOR_STRING) {
		return value.DefaultNilValue, false, "La función float solo acepta un argumento de tipo string"
	}

	floatValue, err := strconv.ParseFloat(argValue.Value().(string), 64)

	if err != nil {
		return value.DefaultNilValue, false, "No se pudo convertir el valor a float"
	}

	return &value.FloatValue{
		InternalValue: floatValue,
	}, true, ""
}

// * TypeOf Function
func TypeOf(context *ReplContext, args []*Argument) (value.IVOR, bool, string) {

	if len(args) != 1 {
		return value.DefaultNilValue, false, "La función typeOf solo acepta un argumento"
	}

	argValue := args[0].Value

	typeName := argValue.Type()

	return &value.StringValue{
		InternalValue: typeName,
	}, true, ""
}

func IndexOf(context *ReplContext, args []*Argument) (value.IVOR, bool, string) {
	if len(args) != 2 {
		return value.DefaultNilValue, false, "La función indexOf requiere dos argumentos: un vector y un valor a buscar"
	}

	// Verificar que el primer argumento es un vector
	vecArg, ok := args[0].Value.(*VectorValue)
	if !ok {
		return value.DefaultNilValue, false, "El primer argumento debe ser un vector"
	}

	searchValue := args[1].Value

	// Recorrer el vector y comparar valores
	for idx, item := range vecArg.InternalValue {
		if item.Type() == searchValue.Type() && item.Value() == searchValue.Value() {
			return &value.IntValue{InternalValue: idx}, true, ""
		}
	}
	// No encontrado
	return &value.IntValue{InternalValue: -1}, true, ""
}

func Join(context *ReplContext, args []*Argument) (value.IVOR, bool, string) {
	if len(args) != 2 {
		return value.DefaultNilValue, false, "La función join requiere dos argumentos: un vector de strings y un separador string o carácter"
	}

	vecArg, ok := args[0].Value.(*VectorValue)
	if !ok {
		return value.DefaultNilValue, false, "El primer argumento debe ser un vector"
	}

	separatorVal := args[1].Value

	var separator string
	switch separatorVal.Type() {
	case value.IVOR_STRING:
		separator = separatorVal.Value().(string)
	case value.IVOR_CHARACTER:
		separator = separatorVal.Value().(string)
	default:
		return value.DefaultNilValue, false, "El segundo argumento debe ser un string o un carácter"
	}

	// Validar que todos los elementos del vector sean strings
	var parts []string
	for _, item := range vecArg.InternalValue {
		if item.Type() != value.IVOR_STRING {
			return value.DefaultNilValue, false, "Todos los elementos del vector deben ser strings"
		}
		parts = append(parts, item.Value().(string))
	}

	result := strings.Join(parts, separator)

	return &value.StringValue{
		InternalValue: result,
	}, true, ""
}

func Len(context *ReplContext, args []*Argument) (value.IVOR, bool, string) {
	if len(args) != 1 {
		return value.DefaultNilValue, false, "La función len requiere un solo argumento"
	}

	val := args[0].Value

	raw := val.Value()

	// referencia a vector
	if ref, ok := raw.(*VectorItemReference); ok {
		val = ref.Value
	}

	// referencia a matriz
	if ref, ok := raw.(*MatrixItemReference); ok {
		val = ref.Value
	}

	switch real := val.(type) {
	case *VectorValue:
		return &value.IntValue{
			InternalValue: len(real.InternalValue),
		}, true, ""

	case *MatrixValue:
		return &value.IntValue{
			InternalValue: len(real.Items),
		}, true, ""

	default:
		return value.DefaultNilValue, false, "La función len solo puede aplicarse a vectores o matrices"
	}
}

func Append(context *ReplContext, args []*Argument) (value.IVOR, bool, string) {
	if len(args) != 2 {
		return value.DefaultNilValue, false, "La función append requiere dos argumentos"
	}

	target := args[0].Value
	toAppend := args[1].Value

	// Caso 1: target es vector
	if vec, ok := target.(*VectorValue); ok {
		newItems := make([]value.IVOR, len(vec.InternalValue))
		copy(newItems, vec.InternalValue)

		newItems = append(newItems, toAppend.Copy())
		return NewVectorValue(newItems, vec.FullType, vec.ItemType), true, ""
	}

	// Caso 2: target es matriz (append fila nueva)
	if matrix, ok := target.(*MatrixValue); ok {
		rowVector, ok := toAppend.(*VectorValue)
		if !ok {
			return value.DefaultNilValue, false, "Para matrices, el segundo argumento debe ser un vector (una fila)"
		}

		if rowVector.ItemType != matrix.ItemType {
			return value.DefaultNilValue, false, "Tipo incompatible: fila es de tipo " + rowVector.ItemType + ", matriz es de tipo " + matrix.ItemType
		}
		newItems := make([][]value.IVOR, len(matrix.Items))
		for i := range matrix.Items {
			newItems[i] = make([]value.IVOR, len(matrix.Items[i]))
			copy(newItems[i], matrix.Items[i])
		}
		newItems = append(newItems, rowVector.InternalValue)
		return NewMatrixValue(newItems, matrix.FullType, matrix.ItemType), true, ""
	}

	return value.DefaultNilValue, false, "El primer argumento debe ser un vector o matriz"
}

var DefaultBuiltInFunctions = map[string]*BuiltInFunction{
	"print": {
		Name: "print",
		Exec: Print,
	},
	"println": {
		Name: "println",
		Exec: PrintLn,
	},
	"atoi": {
		Name: "atoi",
		Exec: Atoi,
	},
	"parseFloat": {
		Name: "parseFloat",
		Exec: ParseFloat,
	},
	"TypeOf": {
		Name: "TypeOf",
		Exec: TypeOf,
	},
	"indexOf": {
		Name: "indexOf",
		Exec: IndexOf,
	},
	"join": {
		Name: "join",
		Exec: Join,
	},
	"len": {
		Name: "len",
		Exec: Len,
	},
	"append": {
		Name: "append",
		Exec: Append,
	},
}
