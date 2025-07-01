package errors

import (
	"github.com/antlr4-go/antlr/v4"
	"main.go/repl"
)

/*
SyntaxErrorListener -> Es una estructura que implementa la interfaz de escucha de errores de ANTLR.
Permite capturar errores de sintaxis durante el análisis léxico y sintáctico.
Implementa la interfaz de ANTLR DefaultErrorListener para proporcionar un comportamiento personalizado.
*/
type SyntaxErrorListener struct {
	*antlr.DefaultErrorListener
	ErrorTable *repl.ErrorTable
}

/*
NewSyntaxErrorListener -> Es una función que crea una nueva instancia de SyntaxErrorListener.
Devuelve un puntero a SyntaxErrorListener.
Es el constructor de la estructura SyntaxErrorListener.
*/
func NewSyntaxErrorListener(errorTable *repl.ErrorTable) *SyntaxErrorListener {
	return &SyntaxErrorListener{
		ErrorTable: errorTable,
	}
}

/*
SyntaxError -> Es un método que se llama cuando se detecta un error de sintaxis.
Recibe información sobre el error, como la línea, la columna y el mensaje de error.
Este método agrega el error a la tabla de errores.
*/
func (e *SyntaxErrorListener) SyntaxError(_ antlr.Recognizer, _ interface{}, line, column int, msg string, _ antlr.RecognitionException) {

	e.ErrorTable.AddError(
		line,
		column,
		msg,
		repl.SyntaxError,
	)
}

/*
LexicalErrorListener -> Es una estructura que implementa la interfaz de escucha de errores de ANTLR.
Permite capturar errores léxicos durante el análisis léxico.
Implementa la interfaz de ANTLR DefaultErrorListener para proporcionar un comportamiento personalizado.
*/
type LexicalErrorListener struct {
	*antlr.DefaultErrorListener
	ErrorTable *repl.ErrorTable
}

/*
NewLexicalErrorListener -> Es una función que crea una nueva instancia de LexicalErrorListener.
Devuelve un puntero a LexicalErrorListener.
Es el constructor de la estructura LexicalErrorListener.
*/
func NewLexicalErrorListener() *LexicalErrorListener {
	return &LexicalErrorListener{
		ErrorTable: repl.NewErrorTable(),
	}
}

/*
LexicalError -> Es un método que se llama cuando se detecta un error léxico.
Recibe información sobre el error, como la línea, la columna y el mensaje de error.
Este método agrega el error a la tabla de errores.
*/
func (e *LexicalErrorListener) SyntaxError(_ antlr.Recognizer, _ interface{}, line, column int, msg string, _ antlr.RecognitionException) {

	e.ErrorTable.AddError(
		line,
		column,
		msg,
		repl.LexicalError,
	)

}
