package repl

import "main.go/value"

type PointerValue struct {
	AssocVariable *Variable
}

func (p PointerValue) Value() interface{} {
	return p
}

func (p PointerValue) Type() string {
	return value.IVOR_POINTER
}

func (p PointerValue) Copy() value.IVOR {
	return p
}
