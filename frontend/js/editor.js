class CodeEditor {
    constructor() {
        this.editor = null;
        this.currentFile = null;
        this.isModified = false;
        this.isEditorReady = false;
        this.init();
    }

    init() {
        this.setupMonacoEditor();
        this.bindEvents();
    }

    setupMonacoEditor() {
        require.config({
            paths: {
                'vs': 'https://cdnjs.cloudflare.com/ajax/libs/monaco-editor/0.44.0/min/vs'
            }
        });

        require(['vs/editor/editor.main'], () => {
            // Registrar lenguaje VLan Cherry
            monaco.languages.register({ id: 'vlancherry' });

            // Configurar tokenizer
            monaco.languages.setMonarchTokensProvider('vlancherry', {
                tokenizer: {
                    root: [
                        [/\b(mut|if|else|while|for|function|return|print|int|string|bool|true|false|and|or|not|nil)\b/, 'keyword'],
                        [/\b\d+(\.\d+)?\b/, 'number'],
                        [/"([^"\\\\]|\\\\.)*"/, 'string'],
                        [/'([^'\\\\]|\\\\.)*'/, 'string'],
                        [/\/\/.*$/, 'comment'],
                        [/\/\*/, 'comment', '@comment'],
                        [/[(){}[\]]/, 'bracket'],
                        [/[+\-*\/=<>!&|%]/, 'operator'],
                        [/[;,.]/, 'delimiter'],
                        [/[a-zA-Z_]\w*/, 'identifier']
                    ],
                    comment: [
                        [/[^/*]+/, 'comment'],
                        [/\*\//, 'comment', '@pop'],
                        [/./, 'comment']
                    ]
                }
            });

            // Configurar autocompletado
            monaco.languages.registerCompletionItemProvider('vlancherry', {
                provideCompletionItems: (model, position) => {
                    const suggestions = [
                        {
                            label: 'var',
                            kind: monaco.languages.CompletionItemKind.Keyword,
                            insertText: 'var ${1:nombre} = ${2:valor};',
                            insertTextRules: monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet
                        },
                        {
                            label: 'if',
                            kind: monaco.languages.CompletionItemKind.Keyword,
                            insertText: 'if (${1:condition}) {\n\t${2:// código}\n}',
                            insertTextRules: monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet
                        },
                        {
                            label: 'print',
                            kind: monaco.languages.CompletionItemKind.Function,
                            insertText: 'print(${1:valor});',
                            insertTextRules: monaco.languages.CompletionItemInsertTextRule.InsertAsSnippet
                        }
                    ];
                    return { suggestions };
                }
            });

            // Definir tema
            monaco.editor.defineTheme('vlancherry-dark', {
                base: 'vs-dark',
                inherit: true,
                rules: [
                    { token: 'keyword', foreground: '569cd6' },
                    { token: 'string', foreground: 'ce9178' },
                    { token: 'comment', foreground: '6a9955' },
                    { token: 'number', foreground: 'b5cea8' },
                    { token: 'operator', foreground: 'd4d4d4' },
                    { token: 'identifier', foreground: '9cdcfe' }
                ],
                colors: {
                    'editor.background': '#1e1e1e',
                    'editor.foreground': '#d4d4d4'
                }
            });

            // Crear editor
            this.editor = monaco.editor.create(document.getElementById('monaco-editor'), {
                value: '',
                language: 'vlancherry',
                theme: 'vlancherry-dark',
                fontSize: 14,
                lineNumbers: 'on',
                minimap: { enabled: true },
                automaticLayout: true,
                scrollBeyondLastLine: false,
                wordWrap: 'on',
                formatOnPaste: true,
                formatOnType: true
            });

            this.isEditorReady = true;
            this.setupEditorEvents();
        });
    }

    setupEditorEvents() {
        if (!this.editor) return;

        // Eventos de cambio de cursor
        this.editor.onDidChangeCursorPosition((e) => {
            this.updateEditorStats(e.position);
        });

        // Eventos de cambio de contenido
        this.editor.onDidChangeModelContent(() => {
            this.setModified(true);
            this.updateFileStats();
        });

        // Atajos de teclado
        this.editor.addCommand(monaco.KeyMod.CtrlCmd | monaco.KeyCode.KEY_S, () => {
            window.ideController?.saveCurrentFile();
        });

        this.editor.addCommand(monaco.KeyCode.F5, () => {
            window.ideController?.executeCode();
        });

        this.editor.addCommand(monaco.KeyMod.CtrlCmd | monaco.KeyCode.KEY_N, () => {
            window.ideController?.createNewFile();
        });
    }

    bindEvents() {
        // Eventos de teclado globales
        document.addEventListener('keydown', (e) => {
            if (e.ctrlKey || e.metaKey) {
                switch (e.key) {
                    case 'n':
                        e.preventDefault();
                        window.ideController?.createNewFile();
                        break;
                    case 'o':
                        e.preventDefault();
                        window.ideController?.openFile();
                        break;
                    case 's':
                        e.preventDefault();
                        if (e.shiftKey) {
                            window.ideController?.saveFileAs();
                        } else {
                            window.ideController?.saveCurrentFile();
                        }
                        break;
                }
            } else if (e.key === 'F5') {
                e.preventDefault();
                window.ideController?.executeCode();
            }
        });
    }

    openFile(fileName, content) {
        if (!this.isEditorReady) {
            setTimeout(() => this.openFile(fileName, content), 100);
            return;
        }

        this.currentFile = fileName;
        this.editor.setValue(content);
        this.setModified(false);

        // Mostrar editor
        this.showEditor();

        // Actualizar UI
        this.updateFileInfo();
        this.updateFileStats();
        this.updateEditorStats({ lineNumber: 1, column: 1 });
    }

    showEditor() {
        document.getElementById('welcomeScreen').style.display = 'none';
        document.getElementById('editorWrapper').style.display = 'flex';
    }

    showWelcome() {
        document.getElementById('welcomeScreen').style.display = 'flex';
        document.getElementById('editorWrapper').style.display = 'none';
        this.currentFile = null;
        this.setModified(false);
    }

    updateFileInfo() {
        const currentFileElement = document.getElementById('currentFile');
        if (this.currentFile) {
            currentFileElement.textContent = this.currentFile;
        } else {
            currentFileElement.textContent = 'Sin archivo';
        }
    }

    updateEditorStats(position) {
        if (!position) return;
        document.getElementById('lineColumn').textContent =
            `Línea ${position.lineNumber}, Columna ${position.column}`;
    }

    updateFileStats() {
        if (!this.editor) return;

        const model = this.editor.getModel();
        if (!model) return;

        const lineCount = model.getLineCount();
        const charCount = model.getValueLength();

        document.getElementById('fileStats').textContent =
            `${lineCount} líneas, ${charCount} caracteres`;
    }

    setModified(modified) {
        this.isModified = modified;

        // Actualizar indicador visual en el archivo
        if (window.ideController?.fileManager) {
            window.ideController.fileManager.setFileModified(this.currentFile, modified);
        }
    }

    getContent() {
        return this.editor ? this.editor.getValue() : '';
    }

    setContent(content) {
        if (this.editor) {
            this.editor.setValue(content);
            this.setModified(false);
        }
    }

    focus() {
        if (this.editor) {
            this.editor.focus();
        }
    }

    goToLine(line, column = 1) {
        if (this.editor) {
            this.editor.setPosition({ lineNumber: line, column: column });
            this.editor.revealLineInCenter(line);
            this.focus();
        }
    }

    markErrors(errors) {
        if (!this.editor) return;

        const markers = errors.map(error => ({
            startLineNumber: error.line,
            startColumn: error.column || 1,
            endLineNumber: error.line,
            endColumn: (error.column || 1) + (error.length || 1),
            message: error.message,
            severity: this.getSeverity(error.type || error.severity)
        }));

        monaco.editor.setModelMarkers(this.editor.getModel(), 'vlancherry', markers);
    }

    clearErrors() {
        if (this.editor) {
            monaco.editor.setModelMarkers(this.editor.getModel(), 'vlancherry', []);
        }
    }

    getSeverity(type) {
        switch (type) {
            case 'error':
                return monaco.MarkerSeverity.Error;
            case 'warning':
                return monaco.MarkerSeverity.Warning;
            case 'info':
                return monaco.MarkerSeverity.Info;
            default:
                return monaco.MarkerSeverity.Error;
        }
    }

    getCurrentFile() {
        return this.currentFile;
    }

    isFileModified() {
        return this.isModified;
    }

    hasUnsavedChanges() {
        return this.isModified && this.currentFile;
    }
}