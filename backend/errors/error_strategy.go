package errors

import (
	// Traer de parser/parser generado por ANTLR
	"github.com/antlr4-go/antlr/v4"
)

/*
CustomErrorStrategy -> Es una estructura que implementa la interfaz de estrategia de error de ANTLR.
Permite personalizar el manejo de errores durante el análisis sintáctico.
Implementa la interfaz de ANTLR DefaultErrorStrategy para proporcionar un comportamiento personalizado.
*/
type CustomErrorStrategy struct {
	*antlr.DefaultErrorStrategy
}

/*
NewCustomErrorStrategy -> Es una función que crea una nueva instancia de CustomErrorStrategy.
Devuelve un puntero a CustomErrorStrategy.

Es el constructor de la estructura CustomErrorStrategy.
*/
func NewCustomErrorStrategy() *CustomErrorStrategy {
	return &CustomErrorStrategy{
		DefaultErrorStrategy: antlr.NewDefaultErrorStrategy(),
	}
}

/*
Funcion para traducir mensajes de error de ANTLR a un formato más amigable.
En este caso se traduce al español.
*/
func (es *CustomErrorStrategy) ReportInputMisMatch(recognizer antlr.Parser, e *antlr.InputMisMatchException) {
	t1 := recognizer.GetTokenStream().LT(-1)
	msg := "Se recibió " + t1.GetText() + ", se esperaba " + es.GetExpectedTokens(recognizer).String()
	recognizer.NotifyErrorListeners(msg, e.GetOffendingToken(), e)
}
