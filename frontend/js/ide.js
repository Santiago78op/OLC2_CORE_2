class IDEController {
    constructor() {
        this.fileManager = null;
        this.editor = null;
        this.reportsManager = null;
        this.isConnected = false;
        this.messageCounter = 0;
        this.init();
    }
    
    init() {
        // Inicializar componentes
        this.fileManager = new FileManager();
        this.editor = new CodeEditor();
        this.reportsManager = new ReportsManager();

        // Hacer disponible globalmente
        window.ideController = this;
        window.reportsManager = this.reportsManager; // Para las funciones de ordenamiento

        // Configurar interfaz
        this.bindEvents();
        this.checkConnection();

        // Verificar conexión periódicamente
        setInterval(() => this.checkConnection(), 5000);

        this.addConsoleMessage('VLan Cherry IDE iniciado correctamente', 'info');
    }

    bindEvents() {
        // Botón ejecutar
        document.getElementById('executeBtn').addEventListener('click', () => {
            this.executeCode();
        });

        // Limpiar consola
        document.getElementById('clearConsoleBtn').addEventListener('click', () => {
            this.clearConsole();
        });

        // Eventos del menú de Electron
        if (window.electronAPI) {
            window.electronAPI.onMenuExecute(() => this.executeCode());
            window.electronAPI.onMenuShowReports(() => this.reportsManager.showReportsModal());
            window.electronAPI.onMenuClearConsole(() => this.clearConsole());
        }

        // Evento antes de cerrar ventana
        window.addEventListener('beforeunload', (e) => {
            if (this.fileManager.hasUnsavedChanges()) {
                e.preventDefault();
                e.returnValue = '💾 Hay cambios sin guardar. ¿Estás seguro de que quieres salir?';
            }
        });
    }

    switchPanel(panelName) {
        // Actualizar tabs
        document.querySelectorAll('.panel-tab').forEach(tab => {
            tab.classList.toggle('active', tab.dataset.panel === panelName);
        });

        // Mostrar/ocultar paneles
        document.querySelectorAll('.panel-content').forEach(panel => {
            panel.classList.toggle('active', panel.id === panelName + 'Panel');
        });
    }

    async checkConnection() {
        try {
            const response = await window.apiClient.checkStatus();
            this.updateConnectionStatus('🟢 Conectado', true);
            this.isConnected = true;
        } catch (error) {
            this.updateConnectionStatus('🔴 Desconectado', false);
            this.isConnected = false;
        }
    }

    updateConnectionStatus(message, isConnected) {
        const statusElement = document.getElementById('connectionStatus');
        statusElement.textContent = message;
        statusElement.className = `connection-status ${isConnected ? 'connected' : 'disconnected'}`;
    }

    async executeCode() {
        const activeFile = this.fileManager.getActiveFile();
        if (!activeFile) {
            this.addConsoleMessage('No hay archivo activo para ejecutar', 'warning');
            return;
        }

        if (!this.isConnected) {
            this.addConsoleMessage('No hay conexión con el backend', 'error');
            return;
        }

        const code = this.editor.getContent();
        if (!code.trim()) {
            this.addConsoleMessage('El archivo está vacío', 'warning');
            return;
        }

        // UI feedback
        const executeBtn = document.getElementById('executeBtn');
        const originalText = executeBtn.textContent;
        executeBtn.textContent = '⏳ Ejecutando...';
        executeBtn.disabled = true;

        this.addConsoleMessage(`Ejecutando archivo: ${activeFile}`, 'info');
        const startTime = Date.now();

        try {
            const result = await window.apiClient.executeCode(code, activeFile);
            const executionTime = Date.now() - startTime;

            this.displayExecutionResults(result, executionTime);

        } catch (error) {
            console.error('Error executing code:', error);
            this.addConsoleMessage(`Error al ejecutar: ${error.message}`, 'error');
            this.updateStatusMessage('Error de ejecución');
        } finally {
            executeBtn.textContent = originalText;
            executeBtn.disabled = false;
        }
    }

    displayExecutionResults(result, executionTime) {
        // Limpiar errores previos
        this.editor.clearErrors();

        if (result.success) {
            this.addConsoleMessage('✅ Ejecución completada exitosamente', 'success');

            // Mostrar salida del programa usando los nuevos formatos
            this.displayProgramOutput(result);

            this.updateStatusMessage('Ejecución exitosa');
        } else {
            this.addConsoleMessage('❌ Ejecución falló con errores', 'error');

            // Marcar errores en el editor
            if (result.errors && result.errors.length > 0) {
                this.editor.markErrors(result.errors);
                
                // Mostrar resumen de errores
                const errorSummary = this.formatErrorSummary(result.errorSummary);
                this.addConsoleMessage(`Errores encontrados: ${errorSummary}`, 'error');
            }

            this.updateStatusMessage(`${result.errors?.length || 0} errores encontrados`);
        }

        // Mostrar tiempo de ejecución
        document.getElementById('executionTime').textContent = `Tiempo: ${executionTime}ms`;

        // Si hay código ARM64 generado exitosamente, mostrar notificación
        if (result.hasArm64 && result.arm64Code) {
            this.addConsoleMessage('🔧 Código ARM64 generado exitosamente', 'success');
            this.addConsoleMessage('📊 Revisa la pestaña ARM64 en Reportes para ver el código', 'info');
        }

        // Agregar línea divisora
        this.addConsoleDivider();
        
        // Agregar línea divisora
        this.addConsoleDivider();

        // Actualizar reportes
        this.reportsManager.updateReports(result);

        // Cambiar a panel de reportes si hay errores
        if (result.errors && result.errors.length > 0) {
            this.switchPanel('reports');
        }
    }

    updateReports(data) {
        if (!data) return;

        // Mapear los datos del backend a la estructura esperada
        this.currentReports = {
            errors: data.errors || [],
            symbols: data.symbols || data.symbolTable || [],
            ast: data.ast || data.cstSvg || null,
            cstSvg: data.cstSvg || null
        };

        console.log('📊 Reportes actualizados:', {
            errores: this.currentReports.errors.length,
            símbolos: this.currentReports.symbols.length,
            tieneAST: !!this.currentReports.ast,
            tieneCST: !!this.currentReports.cstSvg
        });

        // Si el modal está abierto, actualizar
        if (document.getElementById('reportsModal').style.display !== 'none') {
            this.updateAllReports();
        }
    }

    displayProgramOutput(result) {
        // Priorizar mensajes estructurados si están disponibles
        if (result.consoleMessages && result.consoleMessages.length > 0) {
            console.log('📤 Mostrando mensajes estructurados:', result.consoleMessages);
            
            result.consoleMessages.forEach(msg => {
                this.addConsoleMessage(msg.content, this.mapConsoleMessageType(msg.type), msg.timestamp);
            });
        } else if (result.formattedOutput && result.formattedOutput.length > 0) {
            // Usar output formateado si está disponible
            console.log('📤 Mostrando output formateado');
            this.displayFormattedOutput(result.formattedOutput);
        } else if (result.output && result.output.length > 0) {
            // Fallback al output plano pero procesado
            console.log('📤 Mostrando output plano procesado');
            this.displayPlainOutput(result.output);
        }
    }

    displayFormattedOutput(formattedOutput) {
        // Dividir por líneas y mostrar cada una
        const lines = formattedOutput.split('\n');
        
        lines.forEach(line => {
            if (line.trim()) { // Solo mostrar líneas no vacías
                // Detectar tipo de mensaje por prefijo
                let type = 'output';
                let content = line;
                
                if (line.startsWith('❌')) {
                    type = 'error';
                    content = line.substring(2).trim();
                } else if (line.startsWith('⚠️')) {
                    type = 'warning';
                    content = line.substring(2).trim();
                } else if (line.startsWith('ℹ️')) {
                    type = 'info';
                    content = line.substring(2).trim();
                }
                
                this.addConsoleMessage(content, type);
            }
        });
    }

    displayPlainOutput(plainOutput) {
        // Procesar output plano para preservar saltos de línea
        const lines = plainOutput.split('\n');
        
        lines.forEach((line, index) => {
            // Mostrar líneas vacías solo si no es la última
            if (line.trim() || index < lines.length - 1) {
                this.addConsoleMessage(line || ' ', 'output');
            }
        });
    }

    mapConsoleMessageType(backendType) {
        const typeMap = {
            'output': 'output',
            'error': 'error',
            'info': 'info',
            'warning': 'warning'
        };
        
        return typeMap[backendType] || 'output';
    }

    formatErrorSummary(errorSummary) {
        if (!errorSummary) return '0';
        
        const parts = [];
        if (errorSummary.lexical) parts.push(`${errorSummary.lexical} léxicos`);
        if (errorSummary.syntax) parts.push(`${errorSummary.syntax} sintácticos`);
        if (errorSummary.semantic) parts.push(`${errorSummary.semantic} semánticos`);
        if (errorSummary.runtime) parts.push(`${errorSummary.runtime} de ejecución`);
        
        return parts.length > 0 ? parts.join(', ') : '0';
    }

    addConsoleDivider() {
        const consoleOutput = document.getElementById('consoleOutput');
        const divider = document.createElement('div');
        divider.className = 'console-divider';
        consoleOutput.appendChild(divider);
        consoleOutput.scrollTop = consoleOutput.scrollHeight;
    }

    showActivityIndicator() {
        const consoleOutput = document.getElementById('consoleOutput');
        
        // Remover indicador anterior si existe
        const existingIndicator = consoleOutput.querySelector('.console-activity-indicator');
        if (existingIndicator) {
            existingIndicator.remove();
        }

        // Agregar nuevo indicador
        const indicator = document.createElement('div');
        indicator.className = 'console-activity-indicator';
        consoleOutput.appendChild(indicator);

        // Remover después de 2 segundos
        setTimeout(() => {
            if (indicator.parentNode) {
                indicator.remove();
            }
        }, 2000);
    }

    // Métodos de gestión de archivos
    createNewFile() {
        this.fileManager.createNewFile();
    }

    openFile() {
        this.fileManager.openFileDialog();
    }

    saveCurrentFile() {
        this.fileManager.saveCurrentFile();
    }

    saveFileAs() {
        this.fileManager.saveFileAs();
    }

    // Gestión de consola
    addConsoleMessage(message, type = 'info', timestamp = null) {
        const consoleOutput = document.getElementById('consoleOutput');
        const messageTime = timestamp ? new Date(timestamp) : new Date();
        const timeString = messageTime.toLocaleTimeString('es-ES', { 
            hour12: false,
            hour: '2-digit',
            minute: '2-digit',
            second: '2-digit'
        });

        // Incrementar contador
        this.messageCounter++;

        const messageElement = document.createElement('div');
        messageElement.className = `console-message ${type}`;
        messageElement.setAttribute('data-message-id', this.messageCounter);
        
        // Escapar HTML pero preservar saltos de línea
        const escapedMessage = this.escapeHtml(message);
        
        messageElement.innerHTML = `
            <span class="timestamp">[${timeString}]</span>
            <span class="message">${escapedMessage}</span>
        `;

        // Agregar efecto de entrada
        messageElement.style.opacity = '0';
        messageElement.style.transform = 'translateX(-10px)';
        
        consoleOutput.appendChild(messageElement);

        // Animar entrada
        requestAnimationFrame(() => {
            messageElement.style.transition = 'all 0.3s ease-out';
            messageElement.style.opacity = '1';
            messageElement.style.transform = 'translateX(0)';
        });

        // Auto-scroll al final
        consoleOutput.scrollTop = consoleOutput.scrollHeight;

        // Limitar número de mensajes (mantener últimos 500)
        const messages = consoleOutput.children;
        if (messages.length > 500) {
            // Remover los primeros 50 mensajes
            for (let i = 0; i < 50; i++) {
                if (messages[0]) {
                    consoleOutput.removeChild(messages[0]);
                }
            }
        }

        // Agregar indicador de actividad temporal
        this.showActivityIndicator();
    }

    clearConsole() {
        const consoleOutput = document.getElementById('consoleOutput');
        consoleOutput.innerHTML = '';
        this.messageCounter = 0;
        this.addConsoleMessage('🧹 Consola limpiada', 'system');
    }

    updateStatusMessage(message) {
        document.getElementById('statusMessage').textContent = message;
    }

    // Método mejorado de escape HTML que preserva saltos de línea
    escapeHtml(text) {
        if (typeof text !== 'string') {
            text = String(text);
        }
        
        const div = document.createElement('div');
        div.textContent = text;
        return div.innerHTML;
    }

    exportConsoleLog() {
        const messages = document.querySelectorAll('.console-message');
        const logContent = Array.from(messages).map(msg => {
            const timestamp = msg.querySelector('.timestamp')?.textContent || '';
            const message = msg.querySelector('.message')?.textContent || '';
            const type = msg.className.split(' ').find(cls => cls !== 'console-message') || 'info';
            
            return `${timestamp} [${type.toUpperCase()}] ${message}`;
        }).join('\n');

        const blob = new Blob([logContent], { type: 'text/plain' });
        const url = URL.createObjectURL(blob);
        const a = document.createElement('a');
        a.href = url;
        a.download = `vlancherry-log-${new Date().toISOString().slice(0, 19).replace(/:/g, '-')}.txt`;
        document.body.appendChild(a);
        a.click();
        document.body.removeChild(a);
        URL.revokeObjectURL(url);

        this.addConsoleMessage('📄 Log de consola exportado', 'info');
    }

    getConsoleStats() {
        const messages = document.querySelectorAll('.console-message');
        const stats = {
            total: messages.length,
            info: document.querySelectorAll('.console-message.info').length,
            success: document.querySelectorAll('.console-message.success').length,
            warning: document.querySelectorAll('.console-message.warning').length,
            error: document.querySelectorAll('.console-message.error').length,
            output: document.querySelectorAll('.console-message.output').length,
            system: document.querySelectorAll('.console-message.system').length
        };

        return stats;
    }


    // Método para obtener estado del IDE
    getIDEState() {
        return {
            activeFile: this.fileManager.getActiveFile(),
            openFiles: Array.from(this.fileManager.getOpenFiles().keys()),
            hasUnsavedChanges: this.fileManager.hasUnsavedChanges(),
            isConnected: this.isConnected,
            hasErrors: this.reportsManager.hasErrors()
        };
    }
}

// Inicializar IDE cuando el DOM esté listo
document.addEventListener('DOMContentLoaded', () => {
    new IDEController();
});