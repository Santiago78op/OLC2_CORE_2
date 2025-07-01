#!/bin/bash

echo "[INFO] Ejecutando ANTLR4 directamente con el .jar..."
JAR="antlr-4.13.2-complete.jar"

# Verifica que el archivo exista
if [ ! -f "$JAR" ]; then
    echo "[ERROR] No se encuentra $JAR en el directorio actual"
    exit 1
fi

# Verifica que existan los archivos de gramática
if [ ! -f "grammar/VLangLexer.g4" ]; then
    echo "[ERROR] No se encuentra grammar/VLangLexer.g4"
    exit 1
fi

if [ ! -f "grammar/VLangGrammar.g4" ]; then
    echo "[ERROR] No se encuentra grammar/VLangGrammar.g4"
    exit 1
fi

echo "[INFO] Generando VLangLexer..."
# Ejecuta ANTLR para el Lexer
java -Xmx500M -cp "$JAR" org.antlr.v4.Tool \
    -Dlanguage=Go \
    -visitor \
    -package compiler \
    -o . \
    grammar/VLangLexer.g4

if [ $? -ne 0 ]; then
    echo "[ERROR] Falló la generación del VLangLexer"
    exit 1
fi

echo "[INFO] Generando VLangGrammar..."
# Ejecuta ANTLR para el Parser
java -Xmx500M -cp "$JAR" org.antlr.v4.Tool \
    -Dlanguage=Go \
    -visitor \
    -package compiler \
    -o . \
    grammar/VLangGrammar.g4

if [ $? -ne 0 ]; then
    echo "[ERROR] Falló la generación del VLangGrammar"
    exit 1
fi

echo "[INFO] Generación completada exitosamente."
echo "[INFO] Archivos generados:"
echo "  - VLangLexer (desde VLangLexer.g4)"
echo "  - VLangGrammar (desde VLangGrammar.g4)"