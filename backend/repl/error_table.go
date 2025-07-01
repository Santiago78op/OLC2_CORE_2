package repl

import "github.com/antlr4-go/antlr/v4"

/*
ErrorTable es una estructura que almacena errores encontrados durante el análisis de código.
Esta tabla permite registrar errores léxicos, sintácticos, semánticos y de tiempo de ejecución.
*/
const (
	LexicalError  = "lexical" // Cambiar a identificadores más cortos para JSON
	SyntaxError   = "syntax"
	SemanticError = "semantic"
	RuntimeError  = "runtime"
)

// Mapeo de tipos para mostrar en español
var ErrorTypeNames = map[string]string{
	LexicalError:  "Error Léxico",
	SyntaxError:   "Error Sintáctico",
	SemanticError: "Error Semántico",
	RuntimeError:  "Error en Tiempo de Ejecución",
}

// Error representa un error encontrado durante el análisis
type Error struct {
	Line     int    `json:"line"`
	Column   int    `json:"column"`
	Msg      string `json:"message"` // Cambiar a "message" para el frontend
	Type     string `json:"type"`
	Severity string `json:"severity"` // Agregar severidad para el frontend
	Source   string `json:"source"`   // Agregar fuente del error
}

// GetDisplayName retorna el nombre en español del tipo de error
func (e *Error) GetDisplayName() string {
	if name, exists := ErrorTypeNames[e.Type]; exists {
		return name
	}
	return "Error Desconocido"
}

// GetSeverity retorna la severidad basada en el tipo de error
func (e *Error) GetSeverity() string {
	switch e.Type {
	case LexicalError, SyntaxError:
		return "error"
	case SemanticError:
		return "error"
	case RuntimeError:
		return "error"
	default:
		return "error"
	}
}

// La tabla de errores
type ErrorTable struct {
	Errors []Error `json:"errors"`
}

// AddError agrega un error a la tabla de errores con la información proporcionada.
func (et *ErrorTable) AddError(line int, column int, msg string, errorType string) {
	error := Error{
		Line:     line,
		Column:   column,
		Msg:      msg,
		Type:     errorType,
		Severity: getSeverityByType(errorType),
		Source:   "compiler",
	}
	et.Errors = append(et.Errors, error)
}

// getSeverityByType retorna la severidad basada en el tipo de error
func getSeverityByType(errorType string) string {
	switch errorType {
	case LexicalError, SyntaxError, SemanticError, RuntimeError:
		return "error"
	default:
		return "error"
	}
}

// NewLexicalError crea un nuevo error léxico y lo agrega a la tabla de errores.
func (et *ErrorTable) NewLexicalError(line int, column int, msg string) {
	et.AddError(line, column, msg, LexicalError)
}

// NewSyntaxError crea un nuevo error sintáctico y lo agrega a la tabla de errores.
func (et *ErrorTable) NewSyntaxError(line int, column int, msg string) {
	et.AddError(line, column, msg, SyntaxError)
}

// NewSemanticError crea un nuevo error semántico y lo agrega a la tabla de errores.
func (et *ErrorTable) NewSemanticError(token antlr.Token, msg string) {
	et.AddError(token.GetLine(), token.GetColumn(), msg, SemanticError)
}

// NewRuntimeError crea un nuevo error de tiempo de ejecución y lo agrega a la tabla de errores.
func (et *ErrorTable) NewRuntimeError(line int, column int, msg string) {
	et.AddError(line, column, msg, RuntimeError)
}

// HasErrors retorna true si hay errores en la tabla
func (et *ErrorTable) HasErrors() bool {
	return len(et.Errors) > 0
}

// GetErrorCount retorna el número total de errores
func (et *ErrorTable) GetErrorCount() int {
	return len(et.Errors)
}

// GetErrorsByType retorna errores filtrados por tipo
func (et *ErrorTable) GetErrorsByType(errorType string) []Error {
	var filtered []Error
	for _, err := range et.Errors {
		if err.Type == errorType {
			filtered = append(filtered, err)
		}
	}
	return filtered
}

// GetErrorsSummary retorna un resumen de errores por tipo
func (et *ErrorTable) GetErrorsSummary() map[string]int {
	summary := make(map[string]int)
	for _, err := range et.Errors {
		summary[err.Type]++
	}
	return summary
}

// NewErrorTable crea una nueva instancia de ErrorTable.
func NewErrorTable() *ErrorTable {
	return &ErrorTable{
		Errors: make([]Error, 0),
	}
}
