class FileManager {
    constructor() {
        this.openFiles = new Map(); // fileName -> { content, modified, path }
        this.activeFile = null;
        this.init();
    }

    init() {
        this.bindEvents();
        this.updateFilesList();
    }

    bindEvents() {
        // Botones del header
        document.getElementById('newFileBtn').addEventListener('click', () => this.createNewFile());
        document.getElementById('openFileBtn').addEventListener('click', () => this.openFileDialog());
        document.getElementById('saveFileBtn').addEventListener('click', () => this.saveCurrentFile());

        // Botones de bienvenida
        document.getElementById('createFirstFile').addEventListener('click', () => this.createNewFile());
        document.getElementById('welcomeNewFile').addEventListener('click', () => this.createNewFile());
        document.getElementById('welcomeOpenFile').addEventListener('click', () => this.openFileDialog());

        // Eventos del menú de Electron
        if (window.electronAPI) {
            window.electronAPI.onMenuNewFile(() => this.createNewFile());
            window.electronAPI.onMenuOpenFile(() => this.openFileDialog());
            window.electronAPI.onMenuSaveFile(() => this.saveCurrentFile());
            window.electronAPI.onMenuSaveFileAs(() => this.saveFileAs());
        }
    }

    async createNewFile() {
        if (!window.electronAPI) {
            // Fallback para navegador - crear en memoria
            await this.createNewFileInMemory();
            return;
        }

        try {
            // Mostrar diálogo para guardar archivo
            const result = await window.electronAPI.showSaveDialog('nuevo_archivo.vch');

            if (result.canceled || !result.filePath) {
                return; // Usuario canceló
            }

            const filePath = result.filePath;
            const fileName = filePath.split(/[/\\]/).pop();

            // Validar extensión .vch
            if (!fileName.toLowerCase().endsWith('.vch')) {
                window.ideController?.addConsoleMessage(`El archivo debe tener extensión .vch`, 'error');
                return;
            }

            // Verificar si ya está abierto
            if (this.openFiles.has(fileName)) {
                window.ideController?.addConsoleMessage(`El archivo '${fileName}' ya está abierto`, 'warning');
                this.setActiveFile(fileName);
                return;
            }

            // Crear contenido inicial del archivo
            const defaultContent = this.generateDefaultContent(fileName);

            // Guardar archivo físicamente
            const writeResult = await window.electronAPI.writeFile(filePath, defaultContent);

            if (!writeResult.success) {
                throw new Error(writeResult.error);
            }

            // Agregar a archivos abiertos
            this.openFiles.set(fileName, {
                content: defaultContent,
                modified: false,
                path: filePath
            });

            this.setActiveFile(fileName);
            window.ideController?.editor?.openFile(fileName, defaultContent);
            this.updateFilesList();

            window.ideController?.addConsoleMessage(`Archivo '${fileName}' creado en: ${filePath}`, 'success');

        } catch (error) {
            console.error('Error creating file:', error);
            window.ideController?.addConsoleMessage(`Error al crear archivo: ${error.message}`, 'error');
        }
    }

    async createNewFileInMemory() {
        // Función de respaldo para entornos sin Electron
        const fileName = await this.promptFileName();
        if (!fileName) return;

        const fullFileName = fileName.endsWith('.vch') ? fileName : fileName + '.vch';

        if (this.openFiles.has(fullFileName)) {
            window.ideController?.addConsoleMessage(`El archivo '${fullFileName}' ya está abierto`, 'warning');
            this.setActiveFile(fullFileName);
            return;
        }

        const defaultContent = this.generateDefaultContent(fullFileName);

        this.openFiles.set(fullFileName, {
            content: defaultContent,
            modified: true,
            path: null
        });

        this.setActiveFile(fullFileName);
        window.ideController?.editor?.openFile(fullFileName, defaultContent);
        this.updateFilesList();

        window.ideController?.addConsoleMessage(`Archivo '${fullFileName}' creado (en memoria)`, 'success');
    }

    generateDefaultContent(fileName) {
        const now = new Date();
        const dateStr = now.toLocaleDateString('es-ES');
        const timeStr = now.toLocaleTimeString('es-ES');

        return `/*
 * ================================================
 * Archivo: ${fileName}
 * Lenguaje: VLan Cherry
 * Autor: 
 * Fecha: ${dateStr} ${timeStr}
 * Descripción: 
 * ================================================
 */

// Programa principal
function main() {
    // Declaración de variables
    var mensaje = "Hola VLan Cherry";
    var numero = 42;
    var activo = true;
    
    // Salida por consola
    print(mensaje);
    print("El número es: " + numero);
    
    // Estructura de control
    if (activo) {
        print("El programa está funcionando correctamente");
    }
}

// Punto de entrada del programa
main();
`;
    }

    async openFileDialog() {
        if (!window.electronAPI) {
            // Fallback para navegador
            this.showOpenFilesDialog();
            return;
        }

        try {
            const result = await window.electronAPI.showOpenDialog();
            if (result.canceled || !result.filePaths.length) return;

            const filePath = result.filePaths[0];
            await this.openFileFromPath(filePath);
        } catch (error) {
            console.error('Error opening file dialog:', error);
            window.ideController?.addConsoleMessage('Error al abrir archivo', 'error');
        }
    }

    async openFileFromPath(filePath) {
        try {
            const fileName = filePath.split(/[/\\]/).pop();

            if (this.openFiles.has(fileName)) {
                window.ideController?.addConsoleMessage(`El archivo '${fileName}' ya está abierto`, 'warning');
                this.setActiveFile(fileName);
                return;
            }

            const result = await window.electronAPI.readFile(filePath);
            if (!result.success) {
                throw new Error(result.error);
            }

            this.openFiles.set(fileName, {
                content: result.content,
                modified: false,
                path: filePath
            });

            this.setActiveFile(fileName);
            window.ideController?.editor?.openFile(fileName, result.content);
            this.updateFilesList();

            window.ideController?.addConsoleMessage(`📂 Archivo '${fileName}' abierto desde: ${filePath}`, 'success');
        } catch (error) {
            console.error('Error opening file:', error);
            window.ideController?.addConsoleMessage(`Error al abrir archivo: ${error.message}`, 'error');
        }
    }

    showOpenFilesDialog() {
        // Para entornos sin Electron, mostrar lista de archivos disponibles
        if (this.openFiles.size === 0) {
            const createNew = confirm('No hay archivos disponibles.\n\n¿Deseas crear un nuevo archivo?');
            if (createNew) {
                this.createNewFileInMemory();
            }
            return;
        }

        const fileNames = Array.from(this.openFiles.keys());
        const fileName = prompt('Archivos disponibles:\n' + fileNames.join('\n') + '\n\nEscribe el nombre del archivo:');

        if (fileName && this.openFiles.has(fileName)) {
            this.setActiveFile(fileName);
        } else if (fileName) {
            window.ideController?.addConsoleMessage('Archivo no encontrado', 'error');
        }
    }

    async saveCurrentFile() {
        if (!this.activeFile) {
            window.ideController?.addConsoleMessage('No hay archivo activo para guardar', 'warning');
            return;
        }

        const fileData = this.openFiles.get(this.activeFile);
        if (!fileData) return;

        // Obtener contenido actual del editor
        const currentContent = window.ideController?.editor?.getContent() || '';
        fileData.content = currentContent;

        try {
            if (fileData.path) {
                // Guardar en la ruta existente
                await this.saveToPath(fileData.path, currentContent);

                fileData.modified = false;
                this.updateFilesList();
                window.ideController?.editor?.setModified?.(false);

                window.ideController?.addConsoleMessage(`💾 Archivo '${this.activeFile}' guardado en: ${fileData.path}`, 'success');
            } else {
                // Archivo nuevo, necesita ruta
                await this.saveFileAs();
                return;
            }
        } catch (error) {
            console.error('Error saving file:', error);
            window.ideController?.addConsoleMessage(`Error al guardar: ${error.message}`, 'error');
        }
    }

    async saveFileAs() {
        if (!this.activeFile) {
            window.ideController?.addConsoleMessage('No hay archivo activo para guardar', 'warning');
            return;
        }

        if (!window.electronAPI) {
            window.ideController?.addConsoleMessage('Función "Guardar Como" no disponible en este entorno', 'warning');
            return;
        }

        try {
            const result = await window.electronAPI.showSaveDialog(this.activeFile);
            if (result.canceled || !result.filePath) return;

            const currentContent = window.ideController?.editor?.getContent() || '';
            await this.saveToPath(result.filePath, currentContent);

            // Actualizar datos del archivo
            const fileData = this.openFiles.get(this.activeFile);
            if (fileData) {
                fileData.path = result.filePath;
                fileData.modified = false;
            }

            this.updateFilesList();
            window.ideController?.addConsoleMessage(`💾 Archivo guardado como: ${result.filePath}`, 'success');
        } catch (error) {
            console.error('Error saving file as:', error);
            window.ideController?.addConsoleMessage(`Error al guardar: ${error.message}`, 'error');
        }
    }

    async saveToPath(filePath, content) {
        if (window.electronAPI) {
            const result = await window.electronAPI.writeFile(filePath, content);
            if (!result.success) {
                throw new Error(result.error);
            }
        } else {
            // Fallback: solo actualizar en memoria
            console.log('Saving to memory (no file system access):', filePath);
        }
    }

    async closeFile(fileName) {
        const fileData = this.openFiles.get(fileName);
        if (!fileData) return;

        if (fileData.modified) {
            const save = confirm(`💾 El archivo '${fileName}' tiene cambios sin guardar.\n\n¿Deseas guardarlo antes de cerrar?`);
            if (save) {
                await this.saveCurrentFile();
                return;
            }
        }

        this.openFiles.delete(fileName);

        if (this.activeFile === fileName) {
            // Activar otro archivo o mostrar bienvenida
            const remainingFiles = Array.from(this.openFiles.keys());
            if (remainingFiles.length > 0) {
                this.setActiveFile(remainingFiles[0]);
            } else {
                this.activeFile = null;
                window.ideController?.editor?.showWelcome();
            }
        }

        this.updateFilesList();
        window.ideController?.addConsoleMessage(`🗂️ Archivo '${fileName}' cerrado`, 'info');
    }

    setActiveFile(fileName) {
        if (!this.openFiles.has(fileName)) return;

        this.activeFile = fileName;
        const fileData = this.openFiles.get(fileName);

        window.ideController?.editor?.openFile(fileName, fileData.content);
        this.updateFilesList();
    }

    setFileModified(fileName, modified) {
        if (!fileName || !this.openFiles.has(fileName)) return;

        const fileData = this.openFiles.get(fileName);
        fileData.modified = modified;
        this.updateFilesList();
    }

    updateFilesList() {
        const filesList = document.getElementById('filesList');

        if (this.openFiles.size === 0) {
            filesList.innerHTML = `
                <div class="empty-files">
                    <p>No hay archivos abiertos</p>
                    <button id="createFirstFileEmpty" class="create-btn">📄 Crear nuevo archivo</button>
                </div>
            `;

            document.getElementById('createFirstFileEmpty').addEventListener('click', () => this.createNewFile());

            // Actualizar tabs
            this.updateTabs();
            return;
        }

        filesList.innerHTML = '';

        this.openFiles.forEach((fileData, fileName) => {
            const fileItem = document.createElement('div');
            fileItem.className = `file-item ${fileName === this.activeFile ? 'active' : ''} ${fileData.modified ? 'modified' : ''}`;

            const fileIcon = fileName.endsWith('.vch') ? '🍒' : '📄';

            fileItem.innerHTML = `
                <span class="file-icon">${fileIcon}</span>
                <span class="file-name">${fileName}</span>
                <button class="file-close" title="Cerrar archivo">×</button>
            `;

            fileItem.addEventListener('click', (e) => {
                if (!e.target.classList.contains('file-close')) {
                    this.setActiveFile(fileName);
                }
            });

            fileItem.querySelector('.file-close').addEventListener('click', (e) => {
                e.stopPropagation();
                this.closeFile(fileName);
            });

            filesList.appendChild(fileItem);
        });

        this.updateTabs();
    }

    updateTabs() {
        const tabsContainer = document.getElementById('tabsContainer');

        if (!this.activeFile) {
            tabsContainer.innerHTML = `
                <div class="welcome-tab active">
                    <span>👋 Bienvenido</span>
                </div>
            `;
            return;
        }

        const fileIcon = this.activeFile.endsWith('.vch') ? '🍒' : '📄';
        const fileData = this.openFiles.get(this.activeFile);
        const modifiedIndicator = fileData?.modified ? ' •' : '';

        tabsContainer.innerHTML = `
            <div class="tab active">
                <span class="tab-icon">${fileIcon}</span>
                <span class="tab-name">${this.activeFile}${modifiedIndicator}</span>
                <button class="tab-close" title="Cerrar archivo">×</button>
            </div>
        `;

        tabsContainer.querySelector('.tab-close').addEventListener('click', () => {
            this.closeFile(this.activeFile);
        });
    }

    async promptFileName() {
        const fileName = prompt('Nombre del archivo VLan Cherry:', 'mi_programa.vch');
        return fileName ? fileName.trim() : null;
    }

    getActiveFile() {
        return this.activeFile;
    }

    getOpenFiles() {
        return this.openFiles;
    }

    hasUnsavedChanges() {
        for (const [fileName, fileData] of this.openFiles) {
            if (fileData.modified) return true;
        }
        return false;
    }
}