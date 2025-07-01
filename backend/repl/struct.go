package repl

import (
	"github.com/antlr4-go/antlr/v4"
	compiler "main.go/grammar"
)

type Struct struct {
	Name   string
	Fields []compiler.IStruct_propContext
	Token  antlr.Token
}
