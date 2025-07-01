package arm64

// StandardLibrary contiene todas las funciones de la librería estándar ARM64
type StandardLibrary struct {
	usedFunctions map[string]bool // Funciones que se han usado
}

// NewStandardLibrary crea una nueva instancia de la librería estándar
func NewStandardLibrary() *StandardLibrary {
	return &StandardLibrary{
		usedFunctions: make(map[string]bool),
	}
}

// MarkUsed marca una función como usada
func (sl *StandardLibrary) MarkUsed(functionName string) {
	sl.usedFunctions[functionName] = true
}

// IsUsed verifica si una función ha sido usada
func (sl *StandardLibrary) IsUsed(functionName string) bool {
	return sl.usedFunctions[functionName]
}

// GetAllFunctions retorna el código de todas las funciones usadas
func (sl *StandardLibrary) GetAllFunctions() string {
	var code string

	// Agregar funciones según las que se hayan usado
	if sl.IsUsed("print_integer") {
		code += sl.GetPrintInteger() + "\n"
	}

	if sl.IsUsed("print_char") {
		code += sl.GetPrintChar() + "\n"
	}

	if sl.IsUsed("print_string") {
		code += sl.GetPrintString() + "\n"
	}

	if sl.IsUsed("print_float") {
		code += sl.GetPrintFloat() + "\n"
	}

	if sl.IsUsed("print_bool") {
		code += sl.GetPrintBool() + "\n"
	}

	// Agregar datos de la librería estándar al final
	code += sl.GetStandardData()

	return code
}

// === FUNCIONES INDIVIDUALES ===

// GetPrintInteger retorna la función para imprimir enteros
func (sl *StandardLibrary) GetPrintInteger() string {
	return `
print_integer:
    // Función para imprimir enteros con signo
    // Input: x0 = número a imprimir
    // Destruye: x0-x7 (caller-saved registers)
    // Preserva: x8+ (callee-saved registers)
    
    stp x29, x30, [sp, #-16]!    // Guardar frame pointer y link register
    stp x19, x20, [sp, #-16]!    // Guardar registros que vamos a usar
    stp x21, x22, [sp, #-16]!
    stp x23, x24, [sp, #-16]!
    
    mov x19, x0                   // x19 = número original
    
    // Caso especial: cero
    cbnz x19, check_negative
    mov x0, #48                   // ASCII '0'
    bl print_char
    b print_integer_done
    
check_negative:
    // Verificar si es negativo
    tbnz x19, #63, handle_negative
    b convert_positive
    
handle_negative:
    // Imprimir signo negativo
    mov x0, #45                   // ASCII '-'
    bl print_char
    neg x19, x19                  // Hacer el número positivo
    
convert_positive:
    // Preparar buffer para dígitos (máximo 20 dígitos para 64-bit)
    sub sp, sp, #32               // Reservar espacio en stack
    mov x20, sp                   // x20 = puntero al buffer
    mov x21, #0                   // x21 = contador de dígitos
    
digit_conversion_loop:
    mov x22, #10                  // Divisor
    udiv x23, x19, x22           // x23 = quotient (x19 / 10)
    msub x24, x23, x22, x19      // x24 = remainder (x19 % 10)
    
    add x24, x24, #48            // Convertir a ASCII
    strb w24, [x20, x21]         // Guardar dígito en buffer
    add x21, x21, #1             // Incrementar contador
    
    mov x19, x23                 // x19 = quotient para siguiente iteración
    cbnz x19, digit_conversion_loop  // Continuar si quotient != 0
    
    // Imprimir dígitos en orden reverso
print_digits_loop:
    sub x21, x21, #1             // Decrementar contador
    ldrb w0, [x20, x21]          // Cargar dígito
    bl print_char                // Imprimir dígito
    cbnz x21, print_digits_loop  // Continuar si quedan dígitos
    
    add sp, sp, #32              // Limpiar buffer del stack
    
print_integer_done:
    ldp x23, x24, [sp], #16      // Restaurar registros
    ldp x21, x22, [sp], #16
    ldp x19, x20, [sp], #16
    ldp x29, x30, [sp], #16
    ret`
}

// GetPrintChar retorna la función para imprimir caracteres
func (sl *StandardLibrary) GetPrintChar() string {
	return `
print_char:
    // Función para imprimir un carácter
    // Input: x0 = carácter ASCII (en los 8 bits bajos)
    // Preserva todos los registros excepto x0-x2, x8
    
    stp x29, x30, [sp, #-16]!    // Guardar frame pointer y link register
    
    // Crear buffer temporal en el stack para el carácter
    sub sp, sp, #16              // Reservar 16 bytes (alineación)
    strb w0, [sp]                // Guardar carácter en el stack
    
    // Syscall write(1, buffer, 1)
    mov x0, #1                   // File descriptor: stdout
    mov x1, sp                   // Buffer: puntero al carácter
    mov x2, #1                   // Length: 1 byte
    mov x8, #64                  // Syscall number: write
    svc #0                       // Llamada al sistema
    
    add sp, sp, #16              // Limpiar buffer del stack
    ldp x29, x30, [sp], #16      // Restaurar registros
    ret`
}

// GetPrintString retorna la función para imprimir strings
func (sl *StandardLibrary) GetPrintString() string {
	return `
print_string:
    // Función para imprimir string terminado en null
    // Input: x0 = puntero al string
    // El string debe terminar en null (\\0)
    
    stp x29, x30, [sp, #-16]!    // Guardar registros
    stp x19, x20, [sp, #-16]!
    
    mov x19, x0                  // x19 = puntero al string
    
    // Encontrar la longitud del string
    mov x20, #0                  // x20 = contador de longitud
    
strlen_loop:
    ldrb w1, [x19, x20]          // Cargar byte
    cbz w1, strlen_done          // Si es 0, terminar
    add x20, x20, #1             // Incrementar contador
    b strlen_loop
    
strlen_done:
    // Verificar si el string está vacío
    cbz x20, print_string_done
    
    // Syscall write(1, string, length)
    mov x0, #1                   // File descriptor: stdout
    mov x1, x19                  // Buffer: puntero al string
    mov x2, x20                  // Length: longitud calculada
    mov x8, #64                  // Syscall number: write
    svc #0                       // Llamada al sistema
    
print_string_done:
    ldp x19, x20, [sp], #16      // Restaurar registros
    ldp x29, x30, [sp], #16
    ret`
}

// GetPrintFloat retorna la función para imprimir flotantes (simplificada)
func (sl *StandardLibrary) GetPrintFloat() string {
	return `
print_float:
    // Función simplificada para imprimir flotantes
    // Input: d0 = número flotante
    // Por simplicidad, convertimos a entero y agregamos ".0"
    
    stp x29, x30, [sp, #-16]!    // Guardar registros
    
    // Convertir flotante a entero (truncar)
    fcvtzs x0, d0                // x0 = (int)d0
    bl print_integer             // Imprimir parte entera
    
    // Imprimir punto decimal
    mov x0, #46                  // ASCII '.'
    bl print_char
    
    // Imprimir "0" (simplificación)
    mov x0, #48                  // ASCII '0'
    bl print_char
    
    ldp x29, x30, [sp], #16      // Restaurar registros
    ret`
}

// GetPrintBool retorna la función para imprimir booleanos
func (sl *StandardLibrary) GetPrintBool() string {
	return `
print_bool:
    // Función para imprimir valores booleanos
    // Input: x0 = valor booleano (0=false, cualquier otra cosa=true)
    
    stp x29, x30, [sp, #-16]!    // Guardar registros
    
    // Verificar si es true o false
    cbnz x0, print_true
    
print_false:
    // Imprimir "false"
    adr x0, false_str
    bl print_string
    b print_bool_done
    
print_true:
    // Imprimir "true"
    adr x0, true_str
    bl print_string
    
print_bool_done:
    ldp x29, x30, [sp], #16      // Restaurar registros
    ret`
}

// GetStandardData retorna los datos necesarios para la librería estándar
func (sl *StandardLibrary) GetStandardData() string {
	data := "\n// === DATOS DE LA LIBRERÍA ESTÁNDAR ===\n"
	data += ".data\n"

	if sl.IsUsed("print_bool") {
		data += `
true_str:   .asciz "true"
false_str:  .asciz "false"
`
	}

	// Agregar otros datos según sea necesario
	data += `
newline:    .asciz "\n"
space:      .asciz " "
`

	return data
}

// === FUNCIONES DE UTILIDAD ===

// GetInitCode retorna código de inicialización si es necesario
func (sl *StandardLibrary) GetInitCode() string {
	// Por ahora no necesitamos inicialización especial
	return ""
}

// GetExitCode retorna el código para terminar el programa
func (sl *StandardLibrary) GetExitCode() string {
	return `
_exit:
    // Función para terminar el programa
    // Input: x0 = código de salida
    mov x8, #93                  // Syscall number: exit
    svc #0                       // Llamada al sistema
    // Esta función no retorna
`
}

// === CONSTANTES ÚTILES ===

const (
	// Números de syscalls ARM64 Linux
	SYSCALL_READ  = 63
	SYSCALL_WRITE = 64
	SYSCALL_EXIT  = 93

	// File descriptors estándar
	STDIN  = 0
	STDOUT = 1
	STDERR = 2

	// Códigos ASCII comunes
	ASCII_NEWLINE = 10
	ASCII_SPACE   = 32
	ASCII_ZERO    = 48
	ASCII_NINE    = 57
	ASCII_MINUS   = 45
	ASCII_DOT     = 46
)
