// backend/repl/console.go
package repl

import (
	"fmt"
	"strings"
	"time"
)

/*
ConsoleMessage representa un mensaje individual en la consola
*/
type ConsoleMessage struct {
	Content   string    `json:"content"`
	Type      string    `json:"type"` // "output", "error", "info", "warning"
	Timestamp time.Time `json:"timestamp"`
	Line      int       `json:"line,omitempty"`
}

/*
Console es una estructura que simula un entorno de consola para mostrar mensajes.
Ahora mantiene mensajes estructurados en lugar de un string plano.
*/
type Console struct {
	messages []ConsoleMessage
	output   string // Para retrocompatibilidad
}

/*
Print es un método de la estructura Console que agrega un mensaje a la salida.
Este método simula el comportamiento de imprimir en una consola.
*/
func (c *Console) Print(s string) {
	// Agregar a output plano para retrocompatibilidad
	c.output += s

	// Procesar el string para identificar saltos de línea y crear mensajes separados
	lines := strings.Split(s, "\n")

	for i, line := range lines {
		// Solo agregar líneas no vacías, excepto si es la única línea
		if line != "" || len(lines) == 1 {
			message := ConsoleMessage{
				Content:   line,
				Type:      "output",
				Timestamp: time.Now(),
				Line:      len(c.messages) + 1,
			}
			c.messages = append(c.messages, message)
		}

		// Si no es la última línea y hay contenido, agregar salto
		if i < len(lines)-1 && line != "" {
			c.output += "\n"
		}
	}
}

/*
PrintError agrega un mensaje de error a la consola
*/
func (c *Console) PrintError(s string) {
	c.output += "[ERROR] " + s

	message := ConsoleMessage{
		Content:   s,
		Type:      "error",
		Timestamp: time.Now(),
		Line:      len(c.messages) + 1,
	}
	c.messages = append(c.messages, message)
}

/*
PrintInfo agrega un mensaje informativo a la consola
*/
func (c *Console) PrintInfo(s string) {
	c.output += "[INFO] " + s

	message := ConsoleMessage{
		Content:   s,
		Type:      "info",
		Timestamp: time.Now(),
		Line:      len(c.messages) + 1,
	}
	c.messages = append(c.messages, message)
}

/*
PrintWarning agrega un mensaje de advertencia a la consola
*/
func (c *Console) PrintWarning(s string) {
	c.output += "[WARNING] " + s

	message := ConsoleMessage{
		Content:   s,
		Type:      "warning",
		Timestamp: time.Now(),
		Line:      len(c.messages) + 1,
	}
	c.messages = append(c.messages, message)
}

/*
Show es un método de la estructura Console que muestra el contenido actual de la salida.
*/
func (c *Console) Show() {
	fmt.Println(c.output)
}

/*
Clear es un método de la estructura Console que limpia el contenido de la salida.
*/
func (c *Console) Clear() {
	c.output = ""
	c.messages = []ConsoleMessage{}
}

/*
NewConsole es una función que crea una nueva instancia de Console.
*/
func NewConsole() *Console {
	return &Console{
		messages: make([]ConsoleMessage, 0),
		output:   "",
	}
}

/*
GetOutput es un método de la estructura Console que devuelve el contenido actual de la salida.
*/
func (c *Console) GetOutput() string {
	return c.output
}

/*
GetMessages devuelve todos los mensajes estructurados de la consola
*/
func (c *Console) GetMessages() []ConsoleMessage {
	return c.messages
}

/*
GetFormattedOutput devuelve el output formateado con saltos de línea preservados
*/
func (c *Console) GetFormattedOutput() string {
	var result strings.Builder

	for i, msg := range c.messages {
		switch msg.Type {
		case "error":
			result.WriteString("❌ " + msg.Content)
		case "warning":
			result.WriteString("⚠️ " + msg.Content)
		case "info":
			result.WriteString("ℹ️ " + msg.Content)
		default:
			result.WriteString(msg.Content)
		}

		// Agregar salto de línea excepto en el último mensaje
		if i < len(c.messages)-1 {
			result.WriteString("\n")
		}
	}

	return result.String()
}
