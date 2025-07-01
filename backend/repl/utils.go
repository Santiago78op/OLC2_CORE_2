package repl

import (
	"regexp"
	"strings"

	"main.go/value"
)

func StringToVector(s *value.StringValue) *VectorValue {

	items := make([]value.IVOR, 0)

	for _, c := range s.InternalValue {
		items = append(items, &value.CharacterValue{InternalValue: string(c)})
	}

	return NewVectorValue(items, "["+value.IVOR_CHARACTER+"]", value.IVOR_CHARACTER)

}

func IsVectorType(_type string) bool {

	// Vector starts with only one [ and ends with only one ]
	// el verctorPattern ahora valida la espresion []tipo
	vectorPattern := "^\\[\\](int|float|bool|string)"

	// Matrix starts with AT LEAST two [[ and ends with at least two ]]
	//matrixPattern := "^\\[\\[.*\\]\\](\\[.*\\]\\])*$"

	// match vector pattern but not matrix pattern

	match, _ := regexp.MatchString(vectorPattern, _type)
	//match2, _ := regexp.MatchString(matrixPattern, _type)

	//return match && !match2
	return match
}

func RemoveBrackets(s string) string {
	return strings.Trim(s, "[]")
}

func IsMatrixType(typ string) bool {
	return strings.HasPrefix(typ, "[[]]")
}

// Función auxiliar para verificar si es un struct
func IsStructType(structValue value.IVOR) bool {
	_, ok := structValue.(*value.StructValue)
	return ok
}

func RemoveMatrixBrackets(typ string) string {
	return strings.Replace(typ, "[[]]", "", 1)
}
