package arm64

// Registros ARM64 - Constantes para facilitar el uso

// === REGISTROS DE PROPÓSITO GENERAL (64-bit) ===

const (
	// Registros principales para operaciones
	X0  = "x0"  // Registro para valores de retorno y primer argumento
	X1  = "x1"  // Segundo argumento / registro temporal
	X2  = "x2"  // Tercer argumento / registro temporal
	X3  = "x3"  // Cuarto argumento / registro temporal
	X4  = "x4"  // Registro temporal
	X5  = "x5"  // Registro temporal
	X6  = "x6"  // Registro temporal
	X7  = "x7"  // Registro temporal
	X8  = "x8"  // Registro para número de syscall
	X9  = "x9"  // Registro temporal
	X10 = "x10" // Registro temporal
	X11 = "x11" // Registro temporal
	X12 = "x12" // Registro temporal
	X13 = "x13" // Registro temporal
	X14 = "x14" // Registro temporal
	X15 = "x15" // Registro temporal

	// Registros callee-saved (preservados por funciones llamadas)
	X19 = "x19" // Primer registro callee-saved
	X20 = "x20" // Segundo registro callee-saved
	X21 = "x21" // Tercer registro callee-saved
	X22 = "x22" // Cuarto registro callee-saved
	X23 = "x23" // Quinto registro callee-saved
	X24 = "x24" // Sexto registro callee-saved
	X25 = "x25" // Séptimo registro callee-saved
	X26 = "x26" // Octavo registro callee-saved
	X27 = "x27" // Noveno registro callee-saved
	X28 = "x28" // Décimo registro callee-saved

	// Registros especiales
	X29 = "x29" // Frame Pointer (FP)
	X30 = "x30" // Link Register (LR)

	// Aliases para registros especiales
	FP = "x29" // Frame Pointer
	LR = "x30" // Link Register

	// Stack Pointer y registros especiales
	SP  = "sp"  // Stack Pointer
	XZR = "xzr" // Zero Register (siempre contiene 0)
)

// === REGISTROS DE 32-BIT ===
// Para operaciones que solo necesitan 32 bits

const (
	W0 = "w0" // 32-bit version of x0
	W1 = "w1" // 32-bit version of x1
	W2 = "w2" // 32-bit version of x2
	W3 = "w3" // 32-bit version of x3
	W8 = "w8" // 32-bit version of x8 (syscalls)
)

// === REGISTROS DE PUNTO FLOTANTE ===
// Para operaciones con números decimales

const (
	D0 = "d0" // Primer registro de punto flotante (64-bit)
	D1 = "d1" // Segundo registro de punto flotante
	D2 = "d2" // Tercer registro de punto flotante
	D3 = "d3" // Cuarto registro de punto flotante

	S0 = "s0" // Primer registro de punto flotante (32-bit)
	S1 = "s1" // Segundo registro de punto flotante (32-bit)
	S2 = "s2" // Tercer registro de punto flotante (32-bit)
	S3 = "s3" // Cuarto registro de punto flotante (32-bit)
)

// === FUNCIONES AUXILIARES ===

// GetTempRegister retorna un registro temporal seguro
// Estos registros pueden modificarse libremente
func GetTempRegister(index int) string {
	temps := []string{X1, X2, X3, X4, X5, X6, X7}
	if index >= 0 && index < len(temps) {
		return temps[index]
	}
	return X1 // Default
}

// GetCalleeSavedRegister retorna un registro que debe preservarse
// Útil para valores que necesitan sobrevivir llamadas a funciones
func GetCalleeSavedRegister(index int) string {
	saved := []string{X19, X20, X21, X22, X23, X24, X25, X26, X27, X28}
	if index >= 0 && index < len(saved) {
		return saved[index]
	}
	return X19 // Default
}

// === CONVENCIONES DE USO ===

/*
CONVENCIONES PARA NUESTRO TRADUCTOR:

1. X0: Registro principal para valores de expresiones
   - Resultado de operaciones aritméticas
   - Valores de retorno de funciones
   - Valor a imprimir

2. X1: Registro auxiliar para operaciones binarias
   - Operando derecho en sumas, restas, etc.
   - Valores temporales

3. X8: Número de syscall
   - 64: write (para print)
   - 93: exit (para terminar programa)

4. X19-X28: Para valores que necesitan preservarse
   - Variables importantes durante llamadas a funciones
   - Contadores de bucles

5. SP: Stack Pointer
   - Para almacenar variables locales
   - Gestión del stack

EJEMPLO DE USO:
```go
// Cargar valor 10 en x0
g.LoadImmediate(X0, 10)

// Mover x0 a x1 y cargar 20 en x0
g.Emit(fmt.Sprintf("mov %s, %s", X1, X0))
g.LoadImmediate(X0, 20)

// Sumar x0 + x1 y guardar en x0
g.Add(X0, X0, X1)
```
*/
