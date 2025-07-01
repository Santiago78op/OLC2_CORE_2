package repl

import (
	"fmt"

	"main.go/value"
)

type MatrixValue struct {
	Items    [][]value.IVOR // bidimensional puro
	ItemType string
	FullType string
}

func NewMatrixValue(items [][]value.IVOR, fullType string, itemType string) *MatrixValue {
	return &MatrixValue{
		Items:    items,
		ItemType: itemType,
		FullType: fullType,
	}
}

func (v MatrixValue) Value() interface{} {
	return v.Items
}

func (v MatrixValue) Type() string {
	return v.FullType
}

func (v MatrixValue) Copy() value.IVOR {
	copyItems := make([][]value.IVOR, len(v.Items))
	for i := range v.Items {
		copyItems[i] = make([]value.IVOR, len(v.Items[i]))
		for j := range v.Items[i] {
			copyItems[i][j] = v.Items[i][j].Copy()
		}
	}

	return NewMatrixValue(copyItems, v.FullType, v.ItemType)
}

func (v *MatrixValue) Set(index []int, val value.IVOR) bool {
	if len(index) != 2 {
		return false
	}

	i, j := index[0], index[1]

	if i < 0 || i >= len(v.Items) {
		return false
	}
	if j < 0 || j >= len(v.Items[i]) {
		return false
	}

	v.Items[i][j] = val
	return true
}

func removeBuiltinsFromVector(vectorItems []value.IVOR) {
	for i := 0; i < len(vectorItems); i++ {
		if item, ok := vectorItems[i].(*VectorValue); ok {
			item.ObjectValue.InternalScope.Reset()
			// removeBuiltinsFromVector(item.InternalValue)
		} else {
			break
		}
	}
}

type MatrixItemReference struct {
	Matrix *MatrixValue
	Index  []int
	Value  value.IVOR
}

func (m *MatrixValue) String() string {
	result := "[ "
	for i, fila := range m.Items {
		if i > 0 {
			result += " "
		}
		result += "["
		for j, item := range fila {
			if j > 0 {
				result += " "
			}
			result += fmt.Sprintf("%v", item.Value())
		}
		result += "]"
	}
	result += " ]"
	return result
}
