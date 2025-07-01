package value

import "fmt"

type StructInstance struct {
	StructName string
	Fields     map[string]IVOR
}

type StructValue struct {
	Instance *StructInstance
}

// Implementa Value() para IVOR
func (sv *StructValue) Value() interface{} {
	return sv.Instance
}

// Implementa Type() para IVOR
func (sv *StructValue) Type() string {
	return IVOR_OBJECT // o algún string que represente structs en tu sistema
}

// Implementa Copy() para IVOR
func (sv *StructValue) Copy() IVOR {
	// Copia superficial: crea nuevo StructValue con mismo contenido
	// Para copia profunda, clona Fields también
	newFields := make(map[string]IVOR)
	for k, v := range sv.Instance.Fields {
		newFields[k] = v.Copy()
	}
	newInstance := &StructInstance{
		StructName: sv.Instance.StructName,
		Fields:     newFields,
	}

	return &StructValue{
		Instance: newInstance,
	}
}

// Método adicional para imprimir la instancia (no parte de IVOR)
func (sv *StructValue) ToString() string {
	return fmt.Sprintf("Struct %s %v", sv.Instance.StructName, sv.Instance.Fields)
}
