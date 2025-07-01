package ast

import (
	"fmt"
	"strings"

	"github.com/antlr4-go/antlr/v4"
	compiler "main.go/grammar"
)

type ASTNode struct {
	Type     string     `json:"type"`
	Text     string     `json:"text"`
	Line     int        `json:"line"`
	Column   int        `json:"column"`
	Children []*ASTNode `json:"children"`
}

type ASTGenerator struct {
	nodes []*ASTNode
}

func NewASTGenerator() *ASTGenerator {
	return &ASTGenerator{
		nodes: make([]*ASTNode, 0),
	}
}

// Generar AST nativo desde el ParseTree de ANTLR
func GenerateNativeAST(tree antlr.ParseTree) *ASTNode {
	generator := NewASTGenerator()
	return generator.visit(tree)
}

func (g *ASTGenerator) visit(tree antlr.ParseTree) *ASTNode {
	if tree == nil {
		return nil
	}

	node := &ASTNode{
		Children: make([]*ASTNode, 0),
	}

	// Obtener información del nodo
	switch t := tree.(type) {
	case *compiler.ProgramContext:
		node.Type = "Program"
		node.Text = "program"
		node.Line = 1
		node.Column = 1
	case *compiler.StmtContext:
		node.Type = "Statement"
		node.Text = truncateText(t.GetText())
		if t.GetStart() != nil {
			node.Line = t.GetStart().GetLine()
			node.Column = t.GetStart().GetColumn()
		}
	case *compiler.Decl_stmtContext:
		node.Type = "Declaration"
		node.Text = truncateText(t.GetText())
		if t.GetStart() != nil {
			node.Line = t.GetStart().GetLine()
			node.Column = t.GetStart().GetColumn()
		}
	case *compiler.ExpressionContext:
		node.Type = "Expression"
		node.Text = truncateText(t.GetText())
		if t.GetStart() != nil {
			node.Line = t.GetStart().GetLine()
			node.Column = t.GetStart().GetColumn()
		}
	case antlr.TerminalNode:
		node.Type = "Terminal"
		node.Text = truncateText(t.GetText())
		if t.GetSymbol() != nil {
			node.Line = t.GetSymbol().GetLine()
			node.Column = t.GetSymbol().GetColumn()
		}
	default:
		// Para otros tipos de contexto
		node.Type = getSimpleTypeName(fmt.Sprintf("%T", tree))
		node.Text = truncateText(tree.GetText())
		if ctx, ok := tree.(antlr.ParserRuleContext); ok && ctx.GetStart() != nil {
			node.Line = ctx.GetStart().GetLine()
			node.Column = ctx.GetStart().GetColumn()
		}
	}

	// Visitar hijos con conversión de tipo correcta
	for i := 0; i < tree.GetChildCount(); i++ {
		child := tree.GetChild(i)

		// Convertir antlr.Tree a antlr.ParseTree
		if parseTreeChild, ok := child.(antlr.ParseTree); ok {
			childNode := g.visit(parseTreeChild)
			if childNode != nil {
				node.Children = append(node.Children, childNode)
			}
		}
	}

	return node
}

// Función auxiliar para truncar texto
func truncateText(text string) string {
	if len(text) > 30 {
		return text[:30] + "..."
	}
	return text
}

// Función auxiliar para obtener nombre simple del tipo
func getSimpleTypeName(fullTypeName string) string {
	// Extraer solo el nombre de la clase del tipo completo
	parts := strings.Split(fullTypeName, ".")
	if len(parts) > 0 {
		typeName := parts[len(parts)-1]
		// Remover "Context" del final si existe
		if strings.HasSuffix(typeName, "Context") {
			typeName = typeName[:len(typeName)-7]
		}
		// Remover "*" del inicio si existe
		if strings.HasPrefix(typeName, "*") {
			typeName = typeName[1:]
		}
		return typeName
	}
	return "Unknown"
}

// Generar SVG desde el AST nativo
func GenerateASTSVG(astNode *ASTNode) string {
	if astNode == nil {
		return generateEmptySVG()
	}

	generator := &SVGGenerator{
		width:       1200,
		height:      800,
		nodeRadius:  30,
		levelHeight: 100,
		nodeSpacing: 150,
	}

	return generator.Generate(astNode)
}

type SVGGenerator struct {
	width       int
	height      int
	nodeRadius  int
	levelHeight int
	nodeSpacing int
}

func (g *SVGGenerator) Generate(root *ASTNode) string {
	// Calcular dimensiones dinámicas
	maxDepth := g.calculateDepth(root)
	maxWidth := g.calculateMaxWidth(root, 0)

	g.width = maxWidth + 200
	if g.width < 800 {
		g.width = 800
	}

	g.height = maxDepth*g.levelHeight + 200
	if g.height < 600 {
		g.height = 600
	}

	var svg strings.Builder

	// Header SVG con estilos mejorados
	svg.WriteString(fmt.Sprintf(`<svg xmlns="http://www.w3.org/2000/svg" width="%d" height="%d" viewBox="0 0 %d %d">`,
		g.width, g.height, g.width, g.height))

	svg.WriteString(`
		<defs>
			<style>
				.node-program { fill: #4caf50; stroke: #ffffff; stroke-width: 3; }
				.node-statement { fill: #2196f3; stroke: #ffffff; stroke-width: 2; }
				.node-declaration { fill: #ff9800; stroke: #ffffff; stroke-width: 2; }
				.node-expression { fill: #9c27b0; stroke: #ffffff; stroke-width: 2; }
				.node-terminal { fill: #f44336; stroke: #ffffff; stroke-width: 2; }
				.node-function { fill: #00bcd4; stroke: #ffffff; stroke-width: 2; }
				.node-variable { fill: #8bc34a; stroke: #ffffff; stroke-width: 2; }
				.node-if { fill: #ff5722; stroke: #ffffff; stroke-width: 2; }
				.node-while { fill: #795548; stroke: #ffffff; stroke-width: 2; }
				.node-for { fill: #607d8b; stroke: #ffffff; stroke-width: 2; }
				.node-default { fill: #757575; stroke: #ffffff; stroke-width: 2; }
				.node-text { fill: #ffffff; font-family: 'Segoe UI', Arial, sans-serif; font-size: 11px; text-anchor: middle; dominant-baseline: middle; font-weight: 600; }
				.edge { stroke: #64b5f6; stroke-width: 2; opacity: 0.8; }
				.background { fill: #1e1e1e; }
				.title-text { fill: #ffffff; font-family: Arial, sans-serif; font-size: 16px; text-anchor: middle; font-weight: bold; }
			</style>
		</defs>
	`)

	// Fondo
	svg.WriteString(fmt.Sprintf(`<rect width="%d" height="%d" class="background"/>`, g.width, g.height))

	// Título
	svg.WriteString(fmt.Sprintf(`<text x="%d" y="30" class="title-text">VLan Cherry - Árbol de Sintaxis Abstracta</text>`, g.width/2))

	// Generar nodos y conexiones
	positions := make(map[*ASTNode]Position)
	g.calculatePositions(root, g.width/2, 80, 0, positions)

	// Dibujar conexiones primero
	g.drawConnections(&svg, root, positions)

	// Dibujar nodos encima
	g.drawNodes(&svg, root, positions)

	svg.WriteString(`</svg>`)
	return svg.String()
}

type Position struct {
	X, Y int
}

func (g *SVGGenerator) calculatePositions(node *ASTNode, x, y, level int, positions map[*ASTNode]Position) {
	positions[node] = Position{X: x, Y: y}

	if len(node.Children) == 0 {
		return
	}

	// Calcular distribución de hijos
	totalWidth := (len(node.Children) - 1) * g.nodeSpacing
	startX := x - totalWidth/2

	for i, child := range node.Children {
		childX := startX + i*g.nodeSpacing
		childY := y + g.levelHeight
		g.calculatePositions(child, childX, childY, level+1, positions)
	}
}

func (g *SVGGenerator) drawConnections(svg *strings.Builder, node *ASTNode, positions map[*ASTNode]Position) {
	nodePos := positions[node]

	for _, child := range node.Children {
		childPos := positions[child]
		svg.WriteString(fmt.Sprintf(`<line x1="%d" y1="%d" x2="%d" y2="%d" class="edge"/>`,
			nodePos.X, nodePos.Y+g.nodeRadius, childPos.X, childPos.Y-g.nodeRadius))

		g.drawConnections(svg, child, positions)
	}
}

func (g *SVGGenerator) drawNodes(svg *strings.Builder, node *ASTNode, positions map[*ASTNode]Position) {
	pos := positions[node]

	// Determinar clase CSS basada en el tipo
	class := g.getNodeClass(node.Type)

	// Dibujar círculo del nodo
	svg.WriteString(fmt.Sprintf(`<circle cx="%d" cy="%d" r="%d" class="%s"/>`,
		pos.X, pos.Y, g.nodeRadius, class))

	// Preparar texto para mostrar
	displayText := node.Type
	if len(displayText) > 10 {
		displayText = displayText[:10]
	}

	// Dibujar texto del nodo
	svg.WriteString(fmt.Sprintf(`<text x="%d" y="%d" class="node-text">%s</text>`,
		pos.X, pos.Y, displayText))

	// Tooltip con información completa
	tooltipText := fmt.Sprintf("Tipo: %s\nTexto: %s\nLínea: %d, Columna: %d",
		node.Type, node.Text, node.Line, node.Column)
	svg.WriteString(fmt.Sprintf(`<title>%s</title>`, tooltipText))

	// Dibujar hijos recursivamente
	for _, child := range node.Children {
		g.drawNodes(svg, child, positions)
	}
}

func (g *SVGGenerator) getNodeClass(nodeType string) string {
	nodeTypeLower := strings.ToLower(nodeType)

	switch {
	case strings.Contains(nodeTypeLower, "program"):
		return "node-program"
	case strings.Contains(nodeTypeLower, "stmt") || strings.Contains(nodeTypeLower, "statement"):
		return "node-statement"
	case strings.Contains(nodeTypeLower, "decl") || strings.Contains(nodeTypeLower, "declaration"):
		return "node-declaration"
	case strings.Contains(nodeTypeLower, "expr") || strings.Contains(nodeTypeLower, "expression"):
		return "node-expression"
	case strings.Contains(nodeTypeLower, "terminal"):
		return "node-terminal"
	case strings.Contains(nodeTypeLower, "func"):
		return "node-function"
	case strings.Contains(nodeTypeLower, "var") || strings.Contains(nodeTypeLower, "mut"):
		return "node-variable"
	case strings.Contains(nodeTypeLower, "if"):
		return "node-if"
	case strings.Contains(nodeTypeLower, "while"):
		return "node-while"
	case strings.Contains(nodeTypeLower, "for"):
		return "node-for"
	default:
		return "node-default"
	}
}

func (g *SVGGenerator) calculateMaxWidth(node *ASTNode, level int) int {
	if len(node.Children) == 0 {
		return g.nodeSpacing
	}

	return len(node.Children) * g.nodeSpacing
}

func (g *SVGGenerator) calculateDepth(node *ASTNode) int {
	if len(node.Children) == 0 {
		return 1
	}

	maxDepth := 0
	for _, child := range node.Children {
		depth := g.calculateDepth(child)
		if depth > maxDepth {
			maxDepth = depth
		}
	}

	return maxDepth + 1
}

func generateEmptySVG() string {
	return `<svg xmlns="http://www.w3.org/2000/svg" width="600" height="400" viewBox="0 0 600 400">
		<rect width="600" height="400" fill="#1e1e1e"/>
		<text x="300" y="180" text-anchor="middle" fill="#ff9800" font-family="Arial" font-size="18">
			⚠️ No se pudo generar el AST
		</text>
		<text x="300" y="210" text-anchor="middle" fill="#cccccc" font-family="Arial" font-size="14">
			Verifica que el código tenga sintaxis válida
		</text>
	</svg>`
}
