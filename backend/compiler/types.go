package compiler

import (
	"strings"

	"main.go/value"
)

// === TIPOS Y ESTRUCTURAS DEL COMPILADOR ===

// CompilerError representa un error durante la compilación
type CompilerError struct {
	Line    int    `json:"line"`
	Column  int    `json:"column"`
	Message string `json:"message"`
	Type    string `json:"type"`
}

// NewCompilerError crea un nuevo error del compilador
func NewCompilerError(line, column int, message, errorType string) CompilerError {
	return CompilerError{
		Line:    line,
		Column:  column,
		Message: message,
		Type:    errorType,
	}
}

// Variable representa una variable en el ámbito del compilador
type Variable struct {
	Name   string `json:"name"`
	Type   string `json:"type"`
	Offset int    `json:"offset"` // Offset en el stack
	Size   int    `json:"size"`   // Tamaño en bytes
}

// Function representa una función definida por el usuario
type Function struct {
	Name       string      `json:"name"`
	Parameters []Parameter `json:"parameters"`
	ReturnType string      `json:"returnType"`
	StartLabel string      `json:"startLabel"`
	EndLabel   string      `json:"endLabel"`
}

// Parameter representa un parámetro de función
type Parameter struct {
	Name string `json:"name"`
	Type string `json:"type"`
}

// Scope representa un ámbito de variables
type Scope struct {
	Variables map[string]*Variable `json:"variables"`
	Parent    *Scope               `json:"-"` // No incluir en JSON para evitar ciclos
	Level     int                  `json:"level"`
}

// NewScope crea un nuevo ámbito
func NewScope(parent *Scope) *Scope {
	level := 0
	if parent != nil {
		level = parent.Level + 1
	}

	return &Scope{
		Variables: make(map[string]*Variable),
		Parent:    parent,
		Level:     level,
	}
}

// AddVariable agrega una variable al ámbito
func (s *Scope) AddVariable(name, varType string, offset, size int) {
	s.Variables[name] = &Variable{
		Name:   name,
		Type:   varType,
		Offset: offset,
		Size:   size,
	}
}

// GetVariable busca una variable en este ámbito y los padres
func (s *Scope) GetVariable(name string) *Variable {
	if variable, exists := s.Variables[name]; exists {
		return variable
	}

	if s.Parent != nil {
		return s.Parent.GetVariable(name)
	}

	return nil
}

// === UTILIDADES PARA TIPOS ===

// VlangTypeToSize retorna el tamaño en bytes de un tipo VlangCherry
func VlangTypeToSize(vlangType string) int {
	switch vlangType {
	case "int":
		return 8 // 64 bits en ARM64
	case "bool":
		return 8 // Tratado como entero en ARM64
	case "float":
		return 8 // 64 bits
	case "string":
		return 8 // Puntero a string
	case "rune":
		return 8 // Carácter en ARM64
	default:
		return 8 // Valor por defecto
	}
}

// VlangTypeToARM64 convierte tipos VlangCherry a tipos ARM64
func VlangTypeToARM64(vlangType string) string {
	switch vlangType {
	case "int":
		return "x" // Registro de 64 bits
	case "bool":
		return "x" // Tratado como entero
	case "float":
		return "d" // Registro de punto flotante
	case "string":
		return "x" // Puntero
	case "rune":
		return "w" // Registro de 32 bits para caracteres
	default:
		return "x" // Por defecto
	}
}

// IsNumericType verifica si un tipo es numérico
func IsNumericType(vlangType string) bool {
	switch vlangType {
	case "int", "float":
		return true
	default:
		return false
	}
}

// IsComparableType verifica si un tipo puede compararse
func IsComparableType(vlangType string) bool {
	switch vlangType {
	case "int", "float", "bool", "string", "rune":
		return true
	default:
		return false
	}
}

// GetDefaultValue retorna el valor por defecto para un tipo
func GetDefaultValue(vlangType string) interface{} {
	switch vlangType {
	case "int":
		return 0
	case "float":
		return 0.0
	case "bool":
		return false
	case "string":
		return ""
	default:
		return nil
	}
}

// === UTILIDADES PARA CONVERSIÓN DE CONTEXTOS ===

// GetTypeFromContext extrae el tipo de un contexto ANTLR
func GetTypeFromContext(ctx interface{}) string {
	if ctx == nil {
		return "unknown"
	}

	// Aquí podrías agregar lógica específica para diferentes contextos
	// Por ahora, convertimos a string directamente
	if typeStr, ok := ctx.(string); ok {
		return typeStr
	}

	return "unknown"
}

// GetExpressionType determina el tipo de una expresión (simplificado)
func GetExpressionType(exprText string) string {
	// Lógica simple para determinar tipos
	// En una implementación real, esto sería más sofisticado

	if strings.Contains(exprText, "\"") {
		return "string"
	}

	if strings.Contains(exprText, ".") {
		return "float"
	}

	if exprText == "true" || exprText == "false" {
		return "bool"
	}

	// Por defecto, asumimos entero
	return "int"
}

// === CONSTANTES ÚTILES ===

const (
	// Tamaños en ARM64
	WORD_SIZE = 8 // 8 bytes = 64 bits

	// Tipos base
	TYPE_INT    = "int"
	TYPE_FLOAT  = "float"
	TYPE_BOOL   = "bool"
	TYPE_STRING = "string"

	// Registros comunes
	RESULT_REGISTER = "x0"
	TEMP_REGISTER   = "x1"

	// Etiquetas comunes
	MAIN_FUNCTION = "_start"
	EXIT_FUNCTION = "_exit"
)

// === MAPEO CON VALORES EXISTENTES ===

// ValueTypeToVlangType convierte tipos del paquete value a tipos VlangCherry
func ValueTypeToVlangType(valueType string) string {
	switch valueType {
	case value.IVOR_INT:
		return TYPE_INT
	case value.IVOR_FLOAT:
		return TYPE_FLOAT
	case value.IVOR_BOOL:
		return TYPE_BOOL
	case value.IVOR_STRING:
		return TYPE_STRING
	default:
		return "unknown"
	}
}

// VlangTypeToValueType convierte tipos VlangCherry a tipos del paquete value
func VlangTypeToValueType(vlangType string) string {
	switch vlangType {
	case TYPE_INT:
		return value.IVOR_INT
	case TYPE_FLOAT:
		return value.IVOR_FLOAT
	case TYPE_BOOL:
		return value.IVOR_BOOL
	case TYPE_STRING:
		return value.IVOR_STRING
	default:
		return value.IVOR_NIL
	}
}
