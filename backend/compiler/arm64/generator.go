package arm64

import (
	"fmt"
	"strings"
)

// ARM64Generator maneja la generación de código ARM64
type ARM64Generator struct {
	instructions []string          // Lista de instrucciones generadas
	labelCount   int               // Contador para etiquetas únicas
	variables    map[string]int    // Offset de variables en el stack
	stackOffset  int               // Offset actual del stack (crece hacia abajo)
	stringData   []string          // Para almacenar datos de strings
	stringCount  int               // Contador para strings únicos
	stringMap    map[string]string // texto -> etiqueta Elimina duplicados
}

// NewARM64Generator crea un nuevo generador
func NewARM64Generator() *ARM64Generator {
	return &ARM64Generator{
		instructions: make([]string, 0),
		labelCount:   0,
		variables:    make(map[string]int),
		stackOffset:  0,
		stringData:   make([]string, 0),
		stringCount:  0,
		stringMap:    make(map[string]string),
	}
}

// Método para agregar strings a la sección .data
func (g *ARM64Generator) AddStringLiteral(text string) string {
	// NUEVO: Verificar si el string ya existe
	if existingLabel, exists := g.stringMap[text]; exists {
		fmt.Printf("🔄 String \"%s\" ya existe como %s, reutilizando\n", text, existingLabel)
		return existingLabel
	}

	// Si no existe, crear nuevo
	stringLabel := fmt.Sprintf("str_%d", g.stringCount)
	g.stringCount++

	// Agregar al mapa para evitar duplicados futuros
	g.stringMap[text] = stringLabel

	// Agregar definición del string a la lista
	stringDef := fmt.Sprintf("%s: .asciz \"%s\"", stringLabel, text)
	g.stringData = append(g.stringData, stringDef)

	fmt.Printf("✅ Nuevo string \"%s\" creado como %s\n", text, stringLabel)
	return stringLabel
}

// === GESTIÓN DE INSTRUCCIONES ===

// Emit añade una instrucción ARM64 con indentación
func (g *ARM64Generator) Emit(instruction string) {
	g.instructions = append(g.instructions, "    "+instruction)
}

// EmitRaw añade una instrucción sin indentación (para etiquetas)
func (g *ARM64Generator) EmitRaw(instruction string) {
	g.instructions = append(g.instructions, instruction)
}

// Comment añade un comentario explicativo
func (g *ARM64Generator) Comment(comment string) {
	g.instructions = append(g.instructions, "    // "+comment)
}

// === GESTIÓN DE ETIQUETAS ===

// GetLabel genera una etiqueta única
func (g *ARM64Generator) GetLabel() string {
	label := fmt.Sprintf("L%d", g.labelCount)
	g.labelCount++
	return label
}

// SetLabel coloca una etiqueta en el código
func (g *ARM64Generator) SetLabel(label string) {
	g.EmitRaw(label + ":")
}

// === GESTIÓN DE VARIABLES ===

// DeclareVariable reserva espacio para una variable en el stack
func (g *ARM64Generator) DeclareVariable(name string) {
	g.stackOffset += 8 // Cada variable ocupa 8 bytes en ARM64
	g.variables[name] = g.stackOffset
	g.Comment(fmt.Sprintf("Variable '%s' declarada en offset %d", name, g.stackOffset))
}

// GetVariableOffset obtiene el offset de una variable
func (g *ARM64Generator) GetVariableOffset(name string) int {
	if offset, exists := g.variables[name]; exists {
		return offset
	}
	return 0 // Si no existe, retorna 0 (esto debería manejarse como error)
}

// VariableExists verifica si una variable ya fue declarada
func (g *ARM64Generator) VariableExists(name string) bool {
	_, exists := g.variables[name]
	return exists
}

// === OPERACIONES BÁSICAS ===

// LoadImmediate carga un valor inmediato en un registro
func (g *ARM64Generator) LoadImmediate(register string, value int) {
	g.Comment(fmt.Sprintf("Cargar valor %d en %s", value, register))
	g.Emit(fmt.Sprintf("mov %s, #%d", register, value))
}

// LoadVariable carga una variable del stack a un registro
func (g *ARM64Generator) LoadVariable(register, varName string) {
	offset := g.GetVariableOffset(varName)
	g.Comment(fmt.Sprintf("Cargar variable '%s' en %s", varName, register))
	g.Emit(fmt.Sprintf("ldr %s, [sp, #%d]", register, offset))
}

// StoreVariable guarda un registro en una variable del stack
func (g *ARM64Generator) StoreVariable(register, varName string) {
	offset := g.GetVariableOffset(varName)
	g.Comment(fmt.Sprintf("Guardar %s en variable '%s'", register, varName))
	g.Emit(fmt.Sprintf("str %s, [sp, #%d]", register, offset))
}

// === OPERACIONES ARITMÉTICAS ===

// Add suma dos registros: result = reg1 + reg2
func (g *ARM64Generator) Add(result, reg1, reg2 string) {
	g.Comment(fmt.Sprintf("Sumar: %s = %s + %s", result, reg1, reg2))
	g.Emit(fmt.Sprintf("add %s, %s, %s", result, reg1, reg2))
}

// Sub resta dos registros: result = reg1 - reg2
func (g *ARM64Generator) Sub(result, reg1, reg2 string) {
	g.Comment(fmt.Sprintf("Restar: %s = %s - %s", result, reg1, reg2))
	g.Emit(fmt.Sprintf("sub %s, %s, %s", result, reg1, reg2))
}

// Mul multiplica dos registros: result = reg1 * reg2
func (g *ARM64Generator) Mul(result, reg1, reg2 string) {
	g.Comment(fmt.Sprintf("Multiplicar: %s = %s * %s", result, reg1, reg2))
	g.Emit(fmt.Sprintf("mul %s, %s, %s", result, reg1, reg2))
}

// Div divide dos registros: result = reg1 / reg2
func (g *ARM64Generator) Div(result, reg1, reg2 string) {
	g.Comment(fmt.Sprintf("Dividir: %s = %s / %s", result, reg1, reg2))
	g.Emit(fmt.Sprintf("sdiv %s, %s, %s", result, reg1, reg2))
}

// Mod módulo de dos registros: result = reg1 % reg2
func (g *ARM64Generator) Mod(result, reg1, reg2 string) {
	g.Comment(fmt.Sprintf("Módulo: %s = %s %% %s", result, reg1, reg2))
	g.Emit(fmt.Sprintf("sdiv x3, %s, %s", reg1, reg2))             // División entera
	g.Emit(fmt.Sprintf("msub %s, x3, %s, %s", result, reg2, reg1)) // x1 - x3*x2
}

// === OPERACIONES DE COMPARACIÓN ===

// Compare compara dos registros
func (g *ARM64Generator) Compare(reg1, reg2 string) {
	g.Comment(fmt.Sprintf("Comparar %s con %s", reg1, reg2))
	g.Emit(fmt.Sprintf("cmp %s, %s", reg1, reg2))
}

// === OPERACIONES DE SALTO ===

// Jump salta incondicionalmente a una etiqueta
func (g *ARM64Generator) Jump(label string) {
	g.Comment(fmt.Sprintf("Saltar a %s", label))
	g.Emit(fmt.Sprintf("b %s", label))
}

// JumpIfEqual salta si la última comparación fue igual
func (g *ARM64Generator) JumpIfEqual(label string) {
	g.Comment(fmt.Sprintf("Saltar a %s si son iguales", label))
	g.Emit(fmt.Sprintf("beq %s", label))
}

// JumpIfZero salta si el registro es cero
func (g *ARM64Generator) JumpIfZero(register, label string) {
	g.Comment(fmt.Sprintf("Saltar a %s si %s es cero", label, register))
	g.Emit(fmt.Sprintf("cbz %s, %s", register, label))
}

// === OPERACIONES DE STACK ===

// Push guarda un registro en el stack
func (g *ARM64Generator) Push(register string) {
	g.Comment(fmt.Sprintf("Push %s al stack", register))
	g.Emit(fmt.Sprintf("str %s, [sp, #-8]!", register))
}

// Pop recupera un valor del stack a un registro
func (g *ARM64Generator) Pop(register string) {
	g.Comment(fmt.Sprintf("Pop del stack a %s", register))
	g.Emit(fmt.Sprintf("ldr %s, [sp], #8", register))
}

// === LLAMADAS A FUNCIONES ===

// CallFunction llama a una función
func (g *ARM64Generator) CallFunction(funcName string) {
	g.Comment(fmt.Sprintf("Llamar función %s", funcName))
	g.Emit(fmt.Sprintf("bl %s", funcName))
}

// === GENERACIÓN DE PROGRAMA COMPLETO ===

// GenerateHeader genera el header del programa ARM64
func (g *ARM64Generator) GenerateHeader() {
	g.EmitRaw(".data")

	// NUEVO: Agregar todos los strings a la sección .data
	for _, stringDef := range g.stringData {
		g.EmitRaw(stringDef)
	}

	g.EmitRaw("") // Línea vacía para separación
	g.EmitRaw(".text")
	g.EmitRaw(".global _start")
	g.EmitRaw("")
	g.EmitRaw("_start:")
	g.Comment("=== INICIO DEL PROGRAMA ===")

	// Configurar el stack inicial si hay variables
	if g.stackOffset > 0 {
		g.Comment(fmt.Sprintf("Reservar %d bytes para variables locales", g.stackOffset))
		g.Emit(fmt.Sprintf("sub sp, sp, #%d", g.stackOffset))
	}
}

// GenerateFooter genera el footer del programa ARM64
func (g *ARM64Generator) GenerateFooter() {
	g.Comment("=== FIN DEL PROGRAMA ===")

	// Limpiar el stack si se reservó espacio
	if g.stackOffset > 0 {
		g.Comment("Limpiar variables del stack")
		g.Emit(fmt.Sprintf("add sp, sp, #%d", g.stackOffset))
	}

	g.Comment("Terminar programa con código de salida 0")
	g.Emit("mov x0, #0")  // Código de salida 0
	g.Emit("mov x8, #93") // Número de syscall para exit
	g.Emit("svc #0")      // Llamada al sistema
}

// === SALIDA FINAL ===

// GetCode retorna todo el código generado como string
func (g *ARM64Generator) GetCode() string {
	return strings.Join(g.instructions, "\n")
}

// Reset limpia el generador para empezar un nuevo programa
func (g *ARM64Generator) Reset() {
	g.instructions = make([]string, 0)
	g.labelCount = 0
	g.variables = make(map[string]int)
	g.stackOffset = 0
	g.stringData = make([]string, 0)
	g.stringCount = 0
	g.stringMap = make(map[string]string) // NUEVO
}

// === UTILIDADES DE DEBUG ===

// PrintVariables muestra las variables declaradas (para debug)
func (g *ARM64Generator) PrintVariables() {
	g.Comment("=== VARIABLES DECLARADAS ===")
	for name, offset := range g.variables {
		g.Comment(fmt.Sprintf("Variable '%s' en offset %d", name, offset))
	}
	g.Comment("=== FIN VARIABLES ===")
}

// GetVariables retorna todas las variables declaradas (para debug)
func (g *ARM64Generator) GetVariables() map[string]int {
	return g.variables
}

// AppendCharToBufferRoutine genera el código de la función append_char_to_buffer
func (g *ARM64Generator) AppendCharToBufferRoutine() {
	g.EmitRaw("append_char_to_buffer")
	g.Emit("stp x29, x30, [sp, #-16]!")
	g.Emit("add x3, x0, x1") // x3 = buffer + index
	g.Emit("strb w2, [x3]")  // buffer[index] = char
	g.Emit("add x1, x1, #1") // index++
	g.Emit("ldp x29, x30, [sp], #16")
	g.Emit("ret")
}
