// JavaScript adicional para la consola mejorada
document.addEventListener('DOMContentLoaded', () => {
    // Botón de exportar consola
    document.getElementById('exportConsoleBtn')?.addEventListener('click', () => {
        if (window.ideController) {
            window.ideController.exportConsoleLog();
        }
    });

    // Botón de estadísticas
    document.getElementById('showConsoleStatsBtn')?.addEventListener('click', () => {
        showConsoleStats();
    });

    // Cerrar modal de estadísticas
    document.getElementById('closeConsoleStatsModal')?.addEventListener('click', () => {
        document.getElementById('consoleStatsModal').style.display = 'none';
    });

    // Exportar estadísticas
    document.getElementById('exportStatsBtn')?.addEventListener('click', () => {
        exportConsoleStats();
    });

    // Actualizar contador de mensajes periódicamente
    setInterval(updateConsoleStatus, 1000);
});

function showConsoleStats() {
    if (!window.ideController) return;
    
    const stats = window.ideController.getConsoleStats();
    
    // Actualizar valores en el modal
    document.getElementById('totalMessages').textContent = stats.total;
    document.getElementById('outputMessages').textContent = stats.output;
    document.getElementById('infoCount').textContent = stats.info;
    document.getElementById('successCount').textContent = stats.success;
    document.getElementById('warningCount').textContent = stats.warning;
    document.getElementById('errorCount').textContent = stats.error;
    document.getElementById('systemCount').textContent = stats.system;
    
    // Mostrar modal
    document.getElementById('consoleStatsModal').style.display = 'flex';
}

function exportConsoleStats() {
    if (!window.ideController) return;
    
    const stats = window.ideController.getConsoleStats();
    const timestamp = new Date().toISOString();
    
    const csvContent = [
        'Tipo,Cantidad',
        `Total,${stats.total}`,
        `Información,${stats.info}`,
        `Éxito,${stats.success}`,
        `Advertencias,${stats.warning}`,
        `Errores,${stats.error}`,
        `Salidas,${stats.output}`,
        `Sistema,${stats.system}`,
        `Timestamp,${timestamp}`
    ].join('\n');
    
    const blob = new Blob([csvContent], { type: 'text/csv' });
    const url = URL.createObjectURL(blob);
    const a = document.createElement('a');
    a.href = url;
    a.download = `vlancherry-stats-${timestamp.slice(0, 19).replace(/:/g, '-')}.csv`;
    document.body.appendChild(a);
    a.click();
    document.body.removeChild(a);
    URL.revokeObjectURL(url);
    
    // Cerrar modal
    document.getElementById('consoleStatsModal').style.display = 'none';
}

function updateConsoleStatus() {
    const messages = document.querySelectorAll('.console-message');
    const messageCount = messages.length;
    
    document.getElementById('consoleMessageCount').textContent = 
        `${messageCount} ${messageCount === 1 ? 'mensaje' : 'mensajes'}`;
    
    if (messageCount > 0) {
        const lastTimestamp = messages[messages.length - 1]?.querySelector('.timestamp')?.textContent;
        if (lastTimestamp) {
            document.getElementById('consoleLastUpdate').textContent = `Último: ${lastTimestamp}`;
        }
    }
}