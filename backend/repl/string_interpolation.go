package repl

import (
	"fmt"
	"regexp"
	"strconv"
	"strings"

	"github.com/antlr4-go/antlr/v4"
	"main.go/value"
)

// InterpolateString procesa una cadena con interpolación de variables
// Busca patrones como $variable y los reemplaza con sus valores
func (v *ReplVisitor) InterpolateString(input string, token antlr.Token) string {
	// Regex para encontrar patrones $variable o ${variable}
	// Soporta tanto $n como ${variable_name}
	patterns := []*regexp.Regexp{
		regexp.MustCompile(`\$\{([a-zA-Z_][a-zA-Z0-9_]*)\}`), // ${variable}
		regexp.MustCompile(`\$([a-zA-Z_][a-zA-Z0-9_]*)`),     // $variable
	}

	result := input

	for _, pattern := range patterns {
		result = pattern.ReplaceAllStringFunc(result, func(match string) string {
			// Extraer el nombre de la variable
			var varName string
			if strings.HasPrefix(match, "${") {
				// ${variable} - quitar ${ y }
				varName = match[2 : len(match)-1]
			} else {
				// $variable - quitar $
				varName = match[1:]
			}

			// Buscar la variable en el scope
			variable := v.ScopeTrace.GetVariable(varName)
			if variable == nil {
				v.ErrorTable.NewSemanticError(token, fmt.Sprintf("Variable '%s' no encontrada en interpolación de string", varName))
				return match // Retornar el patrón original si hay error
			}

			// Convertir el valor a string
			return v.ValueToString(variable.Value)
		})
	}

	return result
}

// ValueToString convierte un valor IVOR a su representación de string
func (v *ReplVisitor) ValueToString(val value.IVOR) string {
	if val == nil {
		return "nil"
	}

	switch val.Type() {
	case value.IVOR_INT:
		return strconv.Itoa(val.Value().(int))
	case value.IVOR_FLOAT:
		return strconv.FormatFloat(val.Value().(float64), 'f', -1, 64)
	case value.IVOR_STRING:
		return val.Value().(string)
	case value.IVOR_CHARACTER:
		return val.Value().(string)
	case value.IVOR_BOOL:
		return strconv.FormatBool(val.Value().(bool))
	case value.IVOR_NIL:
		return "nil"
	default:
		// Para vectores, matrices u otros tipos complejos
		if IsVectorType(val.Type()) {
			return v.formatVectorForInterpolation(val.(*VectorValue))
		}
		if IsMatrixType(val.Type()) {
			return v.formatMatrixForInterpolation(val.(*MatrixValue))
		}
		// Para otros tipos, usar el tipo como representación
		return fmt.Sprintf("[%s]", val.Type())
	}
}

// formatVectorForInterpolation formatea un vector para interpolación
func (v *ReplVisitor) formatVectorForInterpolation(vector *VectorValue) string {
	if len(vector.InternalValue) == 0 {
		return "[]"
	}

	var elements []string
	for _, item := range vector.InternalValue {
		elements = append(elements, v.ValueToString(item))
	}

	return "[" + strings.Join(elements, ", ") + "]"
}

// formatMatrixForInterpolation formatea una matriz para interpolación
func (v *ReplVisitor) formatMatrixForInterpolation(matrix *MatrixValue) string {
	if len(matrix.Items) == 0 {
		return "[[]]"
	}

	var rows []string
	for _, row := range matrix.Items {
		var elements []string
		for _, item := range row {
			elements = append(elements, v.ValueToString(item))
		}
		rows = append(rows, "["+strings.Join(elements, ", ")+"]")
	}

	return "[" + strings.Join(rows, ", ") + "]"
}

// HasInterpolation verifica si una cadena contiene patrones de interpolación
func HasInterpolation(input string) bool {
	// Buscar patrones $variable o ${variable}
	patterns := []string{
		`\$\{[a-zA-Z_][a-zA-Z0-9_]*\}`, // ${variable}
		`\$[a-zA-Z_][a-zA-Z0-9_]*`,     // $variable (pero no seguido de caracteres que podrían ser parte de un identificador)
	}

	for _, pattern := range patterns {
		matched, _ := regexp.MatchString(pattern, input)
		if matched {
			return true
		}
	}

	return false
}
