package value

func IsPrimitiveType(t string) bool {
	switch t {
	case IVOR_BOOL, IVOR_INT, IVOR_FLOAT, IVOR_STRING, IVOR_NIL, IVOR_CHARACTER:
		return true
	default:
		return false
	}
}

func ImplicitCast(targetType string, value IVOR) (IVOR, bool) {

	if targetType == value.Type() {
		return value, true
	}

	// Casteo implícito de tipos primitivos

	// 1. Enteros pueden convertirse a flotantes
	if targetType == IVOR_FLOAT && value.Type() == IVOR_INT {
		return &FloatValue{
			InternalValue: float64(value.(*IntValue).InternalValue),
		}, true
	}

	// 2. Los caracteres pueden convertirse a cadenas
	if targetType == IVOR_STRING && value.Type() == IVOR_CHARACTER {
		return &StringValue{
			InternalValue: value.(*CharacterValue).InternalValue,
		}, true
	}

	return nil, false

}

// DefaultValue devuelve un valor por defecto para un tipo dado
func DefaultValue(targetType string, value IVOR) IVOR {
	// Valores por defecto para tipos primitivos
	switch targetType {
	case IVOR_INT:
		return &IntValue{InternalValue: 0}
	case IVOR_FLOAT:
		return &FloatValue{InternalValue: 0.0}
	case IVOR_STRING:
		return &StringValue{InternalValue: ""}
	case IVOR_BOOL:
		return &BoolValue{InternalValue: false}
	default:
		return DefaultUnInitializedValue
	}
}
