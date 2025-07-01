package value

// Tipos de Datos IVOR (Internal Value Object Representation)
// Representa los tipos de datos que pueden ser utilizados en el lenguaje
const (
	IVOR_INT              = "int"
	IVOR_FLOAT            = "float"
	IVOR_STRING           = "string"
	IVOR_BOOL             = "bool"
	IVOR_CHARACTER        = "rune"
	IVOR_NIL              = "nil"
	IVOR_BUILTIN_FUNCTION = "builtinFunction"
	IVOR_FUNCTION         = "function"
	IVOR_VECTOR           = "vector"
	IVOR_OBJECT           = "object"
	IVOR_ANY              = "any"
	IVOR_POINTER          = "pointer"
	IVOR_MATRIX           = "matrix"
	IVOR_SELF             = "self"
	IVOR_UNINITIALIZED    = "uninitialized"
)

// IVOR Es la Representación Interna de Objeto de Valor
// Representa un valor en el lenguaje, puede ser un número, cadena, booleano, etc.
type IVOR interface {
	Value() interface{}
	Type() string
	Copy() IVOR
}
