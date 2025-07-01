const { contextBridge, ipcRenderer } = require('electron');

contextBridge.exposeInMainWorld('electronAPI', {
    // Menú eventos
    onMenuNewFile: (callback) => ipcRenderer.on('menu-new-file', callback),
    onMenuOpenFile: (callback) => ipcRenderer.on('menu-open-file', callback),
    onMenuSaveFile: (callback) => ipcRenderer.on('menu-save-file', callback),
    onMenuSaveFileAs: (callback) => ipcRenderer.on('menu-save-file-as', callback),
    onMenuExecute: (callback) => ipcRenderer.on('menu-execute', callback),
    onMenuShowReports: (callback) => ipcRenderer.on('menu-show-reports', callback),
    onMenuClearConsole: (callback) => ipcRenderer.on('menu-clear-console', callback),

    // Diálogos de archivo
    showOpenDialog: () => ipcRenderer.invoke('show-open-dialog'),
    showSaveDialog: (defaultName) => ipcRenderer.invoke('show-save-dialog', defaultName),
    readFile: (filePath) => ipcRenderer.invoke('read-file', filePath),
    writeFile: (filePath, content) => ipcRenderer.invoke('write-file', filePath, content)
});