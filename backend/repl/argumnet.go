package repl

import (
	"github.com/antlr4-go/antlr/v4"
	"main.go/value"
)

// Argument representa un argumento en una función o método.
type Argument struct {
	Name            string      // Nombre del argumento
	Value           value.IVOR  // Valor del argumento, puede ser nulo o no inicializado
	PassByReference bool        // Indica si el argumento se pasa por referencia
	Token           antlr.Token // Token asociado al argumento, útil para el análisis sintáctico
	VariableRef     *Variable   // Referencia a una variable asociada al argumento, si existe
}
