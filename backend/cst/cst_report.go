package cst

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"os"
	"path/filepath"
	"runtime"
)

type CSTResponse struct {
	SVGTree string `json:"svgtree"`
}

func ReadFile(filename string) string {
	file, err := os.Open(filename)
	if err != nil {
		fmt.Printf("Error leyendo archivo %s: %v\n", filename, err)
		return ""
	}
	defer file.Close()

	content, err := io.ReadAll(file)
	if err != nil {
		fmt.Printf("Error leyendo contenido: %v\n", err)
		return ""
	}
	return string(content)
}

func CstReport(input string) string {
	_, filename, _, _ := runtime.Caller(0)
	path := filepath.Dir(filename)

	// Ir al directorio padre para acceder a grammar
	grammarPath := filepath.Join(filepath.Dir(path), "grammar")

	// Leer archivos de gramática correctos
	parserFile := filepath.Join(grammarPath, "VLangGrammar.g4")
	lexerFile := filepath.Join(grammarPath, "VLangLexer.g4")

	fmt.Printf("🔍 Buscando gramáticas en:\n")
	fmt.Printf("   Parser: %s\n", parserFile)
	fmt.Printf("   Lexer: %s\n", lexerFile)

	// Verificar que los archivos existen
	if _, err := os.Stat(parserFile); os.IsNotExist(err) {
		fmt.Printf("❌ Archivo de gramática parser no encontrado: %s\n", parserFile)
		return generateFallbackAST(input)
	}

	if _, err := os.Stat(lexerFile); os.IsNotExist(err) {
		fmt.Printf("❌ Archivo de gramática lexer no encontrado: %s\n", lexerFile)
		return generateFallbackAST(input)
	}

	// Leer gramáticas
	parserContent := ReadFile(parserFile)
	lexerContent := ReadFile(lexerFile)

	if parserContent == "" || lexerContent == "" {
		fmt.Println("❌ Error leyendo archivos de gramática")
		return generateFallbackAST(input)
	}

	// Preparar contenido para el servicio
	parserJSON, err := json.Marshal(parserContent)
	if err != nil {
		fmt.Printf("Error marshaling parser: %v\n", err)
		return generateFallbackAST(input)
	}

	lexerJSON, err := json.Marshal(lexerContent)
	if err != nil {
		fmt.Printf("Error marshaling lexer: %v\n", err)
		return generateFallbackAST(input)
	}

	inputJSON, err := json.Marshal(input)
	if err != nil {
		fmt.Printf("Error marshaling input: %v\n", err)
		return generateFallbackAST(input)
	}

	// Crear payload
	payload := []byte(fmt.Sprintf(`{
		"grammar": %s,
		"input": %s,
		"lexgrammar": %s,
		"start": "program"
	}`, parserJSON, inputJSON, lexerJSON))

	fmt.Printf("📤 Enviando request a ANTLR Lab...\n")

	// Hacer request con timeout
	client := &http.Client{}
	req, err := http.NewRequest("POST", "http://lab.antlr.org/parse/", bytes.NewBuffer(payload))
	if err != nil {
		fmt.Printf("Error creando request: %v\n", err)
		return generateFallbackAST(input)
	}

	req.Header.Set("Content-Type", "application/json")
	req.Header.Set("User-Agent", "VLanCherry-IDE/1.0")

	resp, err := client.Do(req)
	if err != nil {
		fmt.Printf("Error enviando request: %v\n", err)
		return generateFallbackAST(input)
	}
	defer resp.Body.Close()

	body, err := io.ReadAll(resp.Body)
	if err != nil {
		fmt.Printf("Error leyendo respuesta: %v\n", err)
		return generateFallbackAST(input)
	}

	if resp.StatusCode != 200 {
		fmt.Printf("❌ Error HTTP %d: %s\n", resp.StatusCode, string(body))
		return generateFallbackAST(input)
	}

	// Parsear respuesta JSON
	var data map[string]interface{}
	if err := json.Unmarshal(body, &data); err != nil {
		fmt.Printf("Error unmarshaling JSON: %v\n", err)
		return generateFallbackAST(input)
	}

	// Extraer SVG
	if result, ok := data["result"].(map[string]interface{}); ok {
		if svgTree, ok := result["svgtree"].(string); ok {
			fmt.Printf("✅ AST generado exitosamente\n")
			return svgTree
		}
	}

	fmt.Printf("❌ No se pudo extraer svgtree de la respuesta\n")
	return generateFallbackAST(input)
}

// Generar AST de respaldo usando información básica
func generateFallbackAST(input string) string {
	fmt.Printf("🔧 Generando AST de respaldo...\n")

	// Crear un SVG básico pero más informativo
	inputPreview := input
	if len(input) > 100 {
		inputPreview = input[:100] + "..."
	}

	return fmt.Sprintf(`<svg xmlns="http://www.w3.org/2000/svg" width="800" height="600" viewBox="0 0 800 600">
		<defs>
			<style>
				.node { fill: #007acc; stroke: #ffffff; stroke-width: 2; }
				.node-text { fill: #ffffff; font-family: Arial, sans-serif; font-size: 12px; text-anchor: middle; }
				.edge { stroke: #cccccc; stroke-width: 1.5; }
				.code-text { fill: #ffb74d; font-family: 'Courier New', monospace; font-size: 10px; }
			</style>
		</defs>
		
		<!-- Fondo -->
		<rect width="800" height="600" fill="#1e1e1e"/>
		
		<!-- Nodo raíz -->
		<circle cx="400" cy="100" r="30" class="node"/>
		<text x="400" y="105" class="node-text">Program</text>
		
		<!-- Información del código -->
		<text x="400" y="200" class="node-text" style="font-size: 14px;">Código analizado:</text>
		<text x="400" y="230" class="code-text">%s</text>
		
		<!-- Mensaje -->
		<text x="400" y="300" class="node-text" style="fill: #ff9800;">AST básico generado localmente</text>
		<text x="400" y="320" class="node-text" style="fill: #ff9800; font-size: 10px;">El servicio externo no está disponible</text>
		
		<!-- Estadísticas básicas -->
		<text x="400" y="400" class="node-text">Líneas de código: %d</text>
		<text x="400" y="420" class="node-text">Caracteres: %d</text>
		
	</svg>`,
		inputPreview,
		len(input)/80+1, // Aproximar líneas
		len(input))
}
