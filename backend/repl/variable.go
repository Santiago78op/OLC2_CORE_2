package repl

import (
	"github.com/antlr4-go/antlr/v4"
	"main.go/value"
)

// Variable representa una variable en el entorno REPL.
type Variable struct {
	Name     string      // Nombre de la variable
	Value    value.IVOR  // Valor de la variable
	Type     string      // Tipo de la variable
	IsConst  bool        // Indica si la variable es constante
	AllowNil bool        // Indica si la variable permite valores nulos
	Token    antlr.Token // Token asociado a la variable
	isProp   bool        // Indica si la variable es una propiedad
}

func (v *Variable) TypeValidation() (bool, string) {

	// Verifica sel valor sea igual a default uninitialized value
	if v.Value == value.DefaultUnInitializedValue {
		v.Value = value.DefaultValue(v.Type, v.Value)
		return true, ""
	}

	// Verifica si el valor de la variable es nulo
	if v.Value == value.DefaultNilValue {
		if v.AllowNil {
			return true, ""
		}
	}

	// *** VALIDACIÓN ESPECÍFICA PARA VECTORES ***
	if IsVectorType(v.Type) && IsVectorType(v.Value.Type()) {
		// Extraer tipos de elementos para comparar
		varItemType := RemoveBrackets(v.Type)           // ej: "int" de "[]int"
		valueItemType := RemoveBrackets(v.Value.Type()) // ej: "int" de "[]int"

		// Si los tipos de elementos coinciden, la asignación es válida
		if varItemType == valueItemType {
			return true, ""
		} else {
			msg := "Type mismatch: No se puede asignar un vector de tipo " + v.Value.Type() + " a una variable de tipo " + v.Type
			v.Value = value.DefaultNilValue
			return false, msg
		}
	}

	// *** VALIDACIÓN PARA VECTORES VACÍOS ***
	if IsVectorType(v.Type) && v.Value.Type() == "[]" {
		return true, ""
	}

	// Comparación de tipos exactos para otros casos
	if v.Type != v.Value.Type() {
		// Trata de hacer una conversión implícita
		convertedValue, ok := value.ImplicitCast(v.Type, v.Value)

		if !ok {
			msg := "Type mismatch: No se puede asignar un valor de tipo " + v.Value.Type() + " a una variable de tipo " + v.Type
			v.Value = value.DefaultNilValue
			return false, msg
		}

		v.Value = convertedValue
	}

	return true, ""
}

func (v *Variable) AssignValue(val value.IVOR, isMutatingContext bool) (bool, string) {
	// Si la variable es constante y se intenta modificar, retorna un error
	if v.IsConst {
		msg := "No se puede asignar un valor a una variable constante: " + v.Name
		return false, msg
	}

	// Si la variable es una propiedad, no se puede asignar un valor directamente
	if v.isProp && !isMutatingContext {
		msg := "No se puede asignar valor a una propiedad fuera de contexto mutable: " + v.Name
		return false, msg
	}

	// Asigna el valor a la variable
	v.Value = val

	// Si la validación de tipo es exitosa, retorna v.Value.Type() == v.Type, "Variable assigned successfully"
	return v.TypeValidation()
}
