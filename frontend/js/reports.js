class ReportsManager {
    constructor() {
        this.currentReports = {
            errors: [],
            symbols: [],
            ast: null
        };
        this.sortOrder = {};
        this.astZoom = 1;
        this.astSvg = null;
        this.astData = null;
        this.arm64Highlighter = null;
        this.rawARM64Code = null; // NUEVA LÍNEA - para código sin HTML
        this.init();
    }

    init() {
        this.bindEvents();
        this.setupModal();
    }

    bindEvents() {
        // Botón para mostrar reportes
        document.getElementById('showReportsBtn').addEventListener('click', () => {
            this.showReportsModal();
        });

        // Cerrar modal
        document.getElementById('closeReportsModal').addEventListener('click', () => {
            this.hideReportsModal();
        });

        // Cerrar modal al hacer click updateARM64Visualizationuera
        document.getElementById('reportsModal').addEventListener('click', (e) => {
            if (e.target.id === 'reportsModal') {
                this.hideReportsModal();
            }
        });

        // Tabs del modal
        document.querySelectorAll('.modal-tab').forEach(tab => {
            tab.addEventListener('click', () => {
                this.switchModalTab(tab.dataset.tab);
            });
        });

        // Botones de descarga - Errores
        document.getElementById('downloadErrorsBtn').addEventListener('click', () => {
            this.downloadErrors('csv');
        });
        document.getElementById('downloadErrorsExcelBtn').addEventListener('click', () => {
            this.downloadErrors('excel');
        });

        // Botones de descarga - Símbolos
        document.getElementById('downloadSymbolsBtn').addEventListener('click', () => {
            this.downloadSymbols('csv');
        });
        document.getElementById('downloadSymbolsExcelBtn').addEventListener('click', () => {
            this.downloadSymbols('excel');
        });

        // Controles de AST
        document.getElementById('zoomInBtn').addEventListener('click', () => {
            this.zoomAST(1.2);
        });
        document.getElementById('zoomOutBtn').addEventListener('click', () => {
            this.zoomAST(0.8);
        });
        document.getElementById('resetZoomBtn').addEventListener('click', () => {
            this.resetASTZoom();
        });
        document.getElementById('expandAllBtn').addEventListener('click', () => {
            this.expandAllAST();
        });
        document.getElementById('collapseAllBtn').addEventListener('click', () => {
            this.collapseAllAST();
        });

        // Botones de descarga - AST
        document.getElementById('downloadASTSvgBtn').addEventListener('click', () => {
            this.downloadAST('svg');
        });
        document.getElementById('downloadASTPngBtn').addEventListener('click', () => {
            this.downloadAST('png');
        });
        document.getElementById('downloadASTJsonBtn').addEventListener('click', () => {
            this.downloadAST('json');
        });

        // Botones de ARM64
        document.getElementById('downloadARM64Btn').addEventListener('click', () => {
            this.downloadARM64Code();
        });

        document.getElementById('executeARM64Btn').addEventListener('click', () => {
            this.executeARM64Code();
        });

        document.getElementById('clearARM64OutputBtn').addEventListener('click', () => {
            this.clearARM64Output();
        });

        // Teclas de escape para cerrar modal
        document.addEventListener('keydown', (e) => {
            if (e.key === 'Escape' && document.getElementById('reportsModal').style.display !== 'none') {
                this.hideReportsModal();
            }
        });

        // Agregar evento para botón de ajuste automático (se creará dinámicamente)
        setTimeout(() => {
            const fitBtn = document.getElementById('fitASTBtn');
            if (fitBtn) {
                fitBtn.addEventListener('click', () => {
                    this.fitASTToContainer();
                });
            }
        }, 100);
    }

    setupModal() {
        // Configuración inicial del modal
        this.hideReportsModal();
        this.createFitButton();
    }

    createFitButton() {
        // Crear botón de ajuste automático
        const astControls = document.querySelector('.ast-controls');
        if (astControls && !document.getElementById('fitASTBtn')) {
            const fitBtn = document.createElement('button');
            fitBtn.className = 'control-btn';
            fitBtn.id = 'fitASTBtn';
            fitBtn.innerHTML = '⤢';
            fitBtn.title = 'Ajustar al contenedor';
            
            // Insertar después del botón de reset zoom
            const resetBtn = document.getElementById('resetZoomBtn');
            if (resetBtn) {
                resetBtn.parentNode.insertBefore(fitBtn, resetBtn.nextSibling);
            } else {
                astControls.appendChild(fitBtn);
            }
            
            fitBtn.addEventListener('click', () => {
                this.fitASTToContainer();
            });
        }
    }

    showReportsModal() {
        document.getElementById('reportsModal').style.display = 'flex';
        document.body.style.overflow = 'hidden'; // Prevenir scroll del fondo

        // Refresh data al mostrar
        this.updateAllReports();
    }

    hideReportsModal() {
        document.getElementById('reportsModal').style.display = 'none';
        document.body.style.overflow = 'auto';
    }

    switchModalTab(tabName) {
        // Actualizar tabs
        document.querySelectorAll('.modal-tab').forEach(tab => {
            tab.classList.toggle('active', tab.dataset.tab === tabName);
        });

        // Mostrar/ocultar contenido
        document.querySelectorAll('.modal-tab-content').forEach(content => {
            content.classList.toggle('active', content.id === tabName + 'Tab');
        });

        // Si es AST y hay datos, renderizar
        if (tabName === 'ast' && this.currentReports.ast) {
            setTimeout(() => this.renderAST(), 100);
        }
        
        // NUEVAS LÍNEAS ARM64:
        if (tabName === 'arm64') {
            setTimeout(() => this.updateARM64Visualization(), 100);
        }
    }

    updateReports(data) {
        if (!data) return;

        // Mapear los datos del backend a la estructura esperada
        this.currentReports = {
            errors: data.errors || [],
            symbols: data.symbols || data.symbolTable || [],
            ast: data.ast || data.cstSvg || null,
            // NUEVAS LÍNEAS ARM64:
            arm64Code: data.arm64Code || null,
            arm64Errors: data.arm64Errors || [],
            hasARM64: data.hasArm64 || false
        };

        console.log('📊 Reportes actualizados:', {
            errores: this.currentReports.errors.length,
            símbolos: this.currentReports.symbols.length,
            tieneAST: !!this.currentReports.ast,
            tieneARM64: !!this.currentReports.arm64Code
        });

        // Si el modal está abierto, actualizar
        if (document.getElementById('reportsModal').style.display !== 'none') {
            this.updateAllReports();
        }
    }
    updateAllReports() {
        this.updateErrorsTable();
        this.updateSymbolsTable();
        this.updateASTVisualization();
        this.updateARM64Visualization(); // NUEVA LÍNEA
    }
    // ==================== ERRORES ====================
    updateErrorsTable() {
        const tbody = document.getElementById('errorsTableBody');
        const count = document.getElementById('errorsCount');
        const errors = this.currentReports.errors;

        count.textContent = `${errors.length} ${errors.length === 1 ? 'error' : 'errores'}`;

        if (errors.length === 0) {
            tbody.innerHTML = '<tr class="empty-row"><td colspan="5">No hay errores que mostrar</td></tr>';
            return;
        }

        tbody.innerHTML = '';
        errors.forEach((error, index) => {
            const row = document.createElement('tr');
            row.className = 'error-row';

            // Mapear tipos de error a clases CSS
            const typeMapping = {
                'lexical': 'error-type-lexical',
                'syntax': 'error-type-syntax', 
                'semantic': 'error-type-semantic',
                'runtime': 'error-type-runtime'
            };

            const typeClass = typeMapping[error.type] || 'error-type-unknown';
            
            // Nombres en español para mostrar
            const typeNames = {
                'lexical': 'LÉXICO',
                'syntax': 'SINTÁCTICO',
                'semantic': 'SEMÁNTICO', 
                'runtime': 'EJECUCIÓN'
            };

            const typeName = typeNames[error.type] || error.type.toUpperCase();

            row.innerHTML = `
                <td>${index + 1}</td>
                <td class="error-message">${this.escapeHtml(error.message || error.msg || '')}</td>
                <td>${error.line || 0}</td>
                <td>${error.column || 0}</td>
                <td><span class="error-type-cell ${typeClass}">${typeName}</span></td>
            `;

            // Click para ir a la línea
            row.addEventListener('click', () => {
                this.goToLocation(error.line, error.column);
                this.hideReportsModal();
            });

            tbody.appendChild(row);
        });
    }

    downloadErrors(format) {
        const errors = this.currentReports.errors;
        if (errors.length === 0) {
            alert('No hay errores para descargar');
            return;
        }

        const data = errors.map((error, index) => ({
            'No.': index + 1,
            'Descripción': error.message || error.description || '',
            'Línea': error.line || 0,
            'Columna': error.column || 0,
            'Tipo': (error.type || 'ERROR').toUpperCase()
        }));

        if (format === 'csv') {
            this.downloadCSV(data, 'errores.csv');
        } else if (format === 'excel') {
            this.downloadExcel(data, 'errores.xlsx', 'Errores');
        }
    }

    // ==================== SÍMBOLOS ====================
    updateSymbolsTable() {
        const tbody = document.getElementById('symbolsTableBody');
        const count = document.getElementById('symbolsCount');
        const symbols = this.currentReports.symbols;

        count.textContent = `${symbols.length} ${symbols.length === 1 ? 'símbolo' : 'símbolos'}`;

        if (symbols.length === 0) {
            tbody.innerHTML = '<tr class="empty-row"><td colspan="6">No hay símbolos que mostrar</td></tr>';
            return;
        }

        tbody.innerHTML = '';
        symbols.forEach((symbol, index) => {
            const row = document.createElement('tr');
            row.className = 'symbol-row';

            // Determinar tipo y clase CSS
            const symbolType = this.getSymbolTypeClass(symbol.type);
            const scopeClass = this.getScopeClass(symbol.scope);

            row.innerHTML = `
                <td><span class="symbol-name">${symbol.name || `SYM_${index + 1}`}</span></td>
                <td><span class="symbol-type-cell ${symbolType}">${this.getSymbolTypeDisplay(symbol.type)}</span></td>
                <td>${symbol.type || 'unknown'}</td>
                <td><span class="symbol-scope ${scopeClass}">${symbol.scope || 'global'}</span></td>
                <td>${symbol.line || 0}</td>
                <td>${symbol.column || 0}</td>
            `;

            // Click para ir a la línea
            if (symbol.line > 0) {
                row.addEventListener('click', () => {
                    this.goToLocation(symbol.line, symbol.column);
                    this.hideReportsModal();
                });
            }

            tbody.appendChild(row);
        });
    }

    // Función para determinar la clase CSS del tipo
    getSymbolTypeClass(type) {
        if (!type) return 'symbol-type-unknown';
        
        if (type.includes('Embebida')) return 'symbol-type-builtin';
        if (type === 'variable' || type === 'int' || type === 'string' || type === 'bool' || type === 'float') return 'symbol-type-variable';
        if (type === 'function' || type.includes('function')) return 'symbol-type-function';
        if (type === 'struct') return 'symbol-type-struct';
        
        return 'symbol-type-variable';
    }

    // Función para determinar la clase CSS del scope
    getScopeClass(scope) {
        if (!scope || scope === 'global') return 'scope-global';
        if (scope.includes('func') || scope.includes('function')) return 'scope-function';
        if (scope.includes('struct')) return 'scope-struct';
        return 'scope-local';
    }

    // Función para mostrar el tipo de símbolo
    getSymbolTypeDisplay(type) {
        if (!type) return 'DESCONOCIDO';
        
        if (type.includes('Embebida')) return 'INCORPORADA';
        if (type === 'variable' || type === 'int' || type === 'string' || type === 'bool' || type === 'float') return 'VARIABLE';
        if (type === 'function' || type.includes('function')) return 'FUNCIÓN';
        if (type === 'struct') return 'ESTRUCTURA';
        
        return type.toUpperCase();
    }

    downloadSymbols(format) {
        const symbols = this.currentReports.symbols;
        if (symbols.length === 0) {
            alert('No hay símbolos para descargar');
            return;
        }

        const data = symbols.map((symbol, index) => ({
            'ID': symbol.id || symbol.name || `SYM_${index + 1}`,
            'Tipo de Símbolo': (symbol.type || 'VARIABLE').toUpperCase(),
            'Tipo de Dato': symbol.dataType || symbol.valueType || 'unknown',
            'Ámbito': symbol.scope || symbol.ambito || 'global',
            'Línea': symbol.line || 0,
            'Columna': symbol.column || 0
        }));

        if (format === 'csv') {
            this.downloadCSV(data, 'tabla_simbolos.csv');
        } else if (format === 'excel') {
            this.downloadExcel(data, 'tabla_simbolos.xlsx', 'Símbolos');
        }
    }

    // ==================== AST ====================
    updateASTVisualization() {
        const container = document.getElementById('astVisualization');
        const ast = this.currentReports.ast || this.currentReports.cstSvg;

        console.log('🌳 Actualizando AST:', {
            tieneAST: !!ast,
            tipoAST: typeof ast,
            longitudAST: ast ? ast.length : 0
        });

        if (!ast) {
            container.innerHTML = `
                <div class="empty-ast">
                    <div class="empty-ast-icon">🌳</div>
                    <p>No hay AST que mostrar</p>
                    <small>Ejecuta código para generar el árbol de sintaxis</small>
                </div>
            `;
            return;
        }

        this.astData = ast;

        // Si la tab de AST está activa, renderizar inmediatamente
        const astTab = document.getElementById('astTab');
        if (astTab && astTab.classList.contains('active')) {
            this.renderAST();
        }
    }

    renderAST() {
        if (!this.astData) {
            console.log('❌ No hay datos de AST para renderizar');
            return;
        }

        const container = document.getElementById('astVisualization');
        
        // Mostrar estado de carga
        this.showASTLoading(container);

        console.log('🎨 Renderizando AST...');

        // Usar setTimeout para permitir que se muestre el loading
        setTimeout(() => {
            try {
                // Verificar si es SVG
                if (typeof this.astData === 'string' && this.astData.includes('<svg')) {
                    this.renderSVGAST(container);
                } else if (typeof this.astData === 'object') {
                    this.renderJSONAST(container);
                } else {
                    this.renderTextAST(container);
                }
            } catch (error) {
                console.error('Error renderizando AST:', error);
                this.showASTError(container, error);
            }
        }, 100);
    }

    // Método para mostrar estado de carga
    showASTLoading(container) {
        container.innerHTML = `
            <div class="ast-loading">
                Generando visualización del AST...
            </div>
        `;
    }

    // Método para mostrar error
    showASTError(container, error) {
        container.innerHTML = `
            <div class="ast-error">
                <div class="ast-error-icon">⚠️</div>
                <p>Error al cargar el AST</p>
                <small>${error.message || 'Error desconocido'}</small>
                <button class="retry-btn" onclick="window.reportsManager.updateASTVisualization()">
                    Reintentar
                </button>
            </div>
        `;
    }

    // Método mejorado para renderizar AST en formato SVG
    renderSVGAST(container) {
        console.log('📊 Renderizando SVG AST mejorado');
        
        // Limpiar contenedor
        container.innerHTML = '';
        
        // Crear contenedor con scroll
        const scrollContainer = document.createElement('div');
        scrollContainer.className = 'ast-scrollable-container';
        
        const wrapper = document.createElement('div');
        wrapper.className = 'ast-svg-wrapper';
        wrapper.style.cssText = `
            width: 100%;
            height: auto;
            min-height: 100%;
            display: flex;
            justify-content: center;
            align-items: flex-start;
            background: #1e1e1e;
            overflow: auto;
            padding: 20px;
            box-sizing: border-box;
            position: relative;
        `;

        // Insertar el SVG
        wrapper.innerHTML = this.astData;

        // Obtener el SVG y ajustarlo
        const svg = wrapper.querySelector('svg');
        if (svg) {
            // Esperar a que el SVG esté en el DOM para calcular el bbox
            setTimeout(() => {
                try {
                    // Calcular el bounding box real del contenido
                    const bbox = svg.getBBox();
                    const margin = 40; // margen extra para que no quede pegado
                    const viewBox = `${bbox.x - margin} ${bbox.y - margin} ${bbox.width + margin * 2} ${bbox.height + margin * 2}`;
                    svg.setAttribute('viewBox', viewBox);
                    svg.setAttribute('width', bbox.width + margin * 2);
                    svg.setAttribute('height', bbox.height + margin * 2);
                    svg.style.width = '100%';
                    svg.style.height = 'auto';
                    svg.style.display = 'block';
                    svg.style.margin = '0 auto';
                    svg.setAttribute('preserveAspectRatio', 'xMidYMid meet');
                } catch (e) {
                    console.warn('No se pudo ajustar el viewBox del SVG:', e);
                }
            }, 0);

            // Agregar indicador de zoom
            this.addZoomIndicator(wrapper);
            
            // Agregar controles de zoom mejorados
            this.addAdvancedZoomControls(wrapper, svg);
            
            // Agregar botón de pantalla completa
            this.addFullscreenButton(wrapper);
        }

        scrollContainer.appendChild(wrapper);
        container.appendChild(scrollContainer);
        
        this.astZoom = 1;
        
        // Forzar re-render después de un frame
        requestAnimationFrame(() => {
            if (svg) {
                window.dispatchEvent(new Event('resize'));
            }
        });
    }

    // Agregar indicador de zoom
    addZoomIndicator(wrapper) {
        const indicator = document.createElement('div');
        indicator.className = 'zoom-indicator';
        indicator.textContent = '100%';
        indicator.id = 'ast-zoom-indicator';
        wrapper.appendChild(indicator);
    }

    // Controles de zoom avanzados
    addAdvancedZoomControls(wrapper, svg) {
        let scale = 1;
        let isDragging = false;
        let startX, startY, scrollLeft, scrollTop;

        // Zoom con rueda del mouse
        wrapper.addEventListener('wheel', (e) => {
            e.preventDefault();
            
            const delta = e.deltaY > 0 ? 0.9 : 1.1;
            scale = Math.max(0.1, Math.min(3, scale * delta));
            
            svg.style.transform = `scale(${scale})`;
            this.astZoom = scale;
            
            // Actualizar indicador
            const indicator = wrapper.querySelector('#ast-zoom-indicator');
            if (indicator) {
                indicator.textContent = `${Math.round(scale * 100)}%`;
            }
        });

        // Pan con mouse
        wrapper.addEventListener('mousedown', (e) => {
            if (e.button === 0) { // Solo botón izquierdo
                isDragging = true;
                wrapper.style.cursor = 'grabbing';
                startX = e.pageX - wrapper.offsetLeft;
                startY = e.pageY - wrapper.offsetTop;
                scrollLeft = wrapper.scrollLeft;
                scrollTop = wrapper.scrollTop;
                e.preventDefault();
            }
        });

        wrapper.addEventListener('mousemove', (e) => {
            if (!isDragging) return;
            e.preventDefault();
            
            const x = e.pageX - wrapper.offsetLeft;
            const y = e.pageY - wrapper.offsetTop;
            const walkX = (x - startX) * 2;
            const walkY = (y - startY) * 2;
            
            wrapper.scrollLeft = scrollLeft - walkX;
            wrapper.scrollTop = scrollTop - walkY;
        });

        wrapper.addEventListener('mouseup', () => {
            isDragging = false;
            wrapper.style.cursor = 'grab';
        });

        wrapper.addEventListener('mouseleave', () => {
            isDragging = false;
            wrapper.style.cursor = 'default';
        });

        // Establecer cursor inicial
        wrapper.style.cursor = 'grab';
    }

    // Botón de pantalla completa
    addFullscreenButton(wrapper) {
        const fullscreenBtn = document.createElement('button');
        fullscreenBtn.innerHTML = '⛶';
        fullscreenBtn.title = 'Ver en pantalla completa';
        fullscreenBtn.style.cssText = `
            position: absolute;
            top: 10px;
            left: 10px;
            background: rgba(0, 122, 204, 0.8);
            color: white;
            border: none;
            padding: 8px 12px;
            border-radius: 4px;
            cursor: pointer;
            font-size: 14px;
            z-index: 10;
            transition: background 0.2s;
        `;
        
        fullscreenBtn.addEventListener('mouseenter', () => {
            fullscreenBtn.style.background = 'rgba(0, 122, 204, 1)';
        });
        
        fullscreenBtn.addEventListener('mouseleave', () => {
            fullscreenBtn.style.background = 'rgba(0, 122, 204, 0.8)';
        });
        
        fullscreenBtn.addEventListener('click', () => {
            this.toggleASTFullscreen(wrapper);
        });
        
        wrapper.appendChild(fullscreenBtn);
    }

    // Pantalla completa para el AST
    toggleASTFullscreen(wrapper) {
        if (wrapper.classList.contains('ast-full-view')) {
            // Salir de pantalla completa
            wrapper.classList.remove('ast-full-view');
            
            // Restaurar al contenedor original
            const astContainer = document.querySelector('#astVisualization');
            if (astContainer) {
                astContainer.appendChild(wrapper.parentElement);
            }
            
            // Actualizar botón
            const fullscreenBtn = wrapper.querySelector('button');
            if (fullscreenBtn) {
                fullscreenBtn.innerHTML = '⛶';
                fullscreenBtn.title = 'Ver en pantalla completa';
            }
        } else {
            // Entrar en pantalla completa
            wrapper.classList.add('ast-full-view');
            document.body.appendChild(wrapper);
            
            // Actualizar botón
            const fullscreenBtn = wrapper.querySelector('button');
            if (fullscreenBtn) {
                fullscreenBtn.innerHTML = '⤓';
                fullscreenBtn.title = 'Salir de pantalla completa';
            }
            
            // Agregar evento para salir con ESC
            const escHandler = (e) => {
                if (e.key === 'Escape') {
                    this.toggleASTFullscreen(wrapper);
                    document.removeEventListener('keydown', escHandler);
                }
            };
            document.addEventListener('keydown', escHandler);
        }
    }

    // Método para renderizar AST en formato JSON/Objeto
    renderJSONAST(container) {
        console.log('🔧 Renderizando JSON AST');
        
        const wrapper = document.createElement('div');
        wrapper.className = 'ast-json-wrapper';
        wrapper.style.cssText = `
            width: 100%;
            height: 100%;
            overflow: auto;
            padding: 20px;
            background: #1e1e1e;
            font-family: 'Consolas', monospace;
        `;

        // Crear visualización en árbol
        const treeHTML = this.createTreeVisualization(this.astData);
        wrapper.innerHTML = treeHTML;

        container.appendChild(wrapper);
    }

    // Método para renderizar AST en formato texto
    renderTextAST(container) {
        console.log('📝 Renderizando texto AST');
        
        const wrapper = document.createElement('div');
        wrapper.className = 'ast-text-wrapper';
        wrapper.style.cssText = `
            width: 100%;
            height: 100%;
            overflow: auto;
            padding: 20px;
            background: #1e1e1e;
            font-family: 'Consolas', monospace;
            color: #d4d4d4;
            white-space: pre-wrap;
        `;

        wrapper.textContent = this.astData;
        container.appendChild(wrapper);
    }

    // Crear visualización en árbol para JSON
    createTreeVisualization(data, level = 0) {
        const indent = '  '.repeat(level);
        let html = '';

        if (typeof data === 'object' && data !== null) {
            if (Array.isArray(data)) {
                html += `<div class="ast-array" style="margin-left: ${level * 20}px;">[\n`;
                data.forEach((item, index) => {
                    html += this.createTreeVisualization(item, level + 1);
                    if (index < data.length - 1) html += ',';
                    html += '\n';
                });
                html += `${indent}]</div>`;
            } else {
                html += `<div class="ast-object" style="margin-left: ${level * 20}px;">{\n`;
                Object.keys(data).forEach((key, index, keys) => {
                    html += `<div class="ast-property" style="margin-left: ${(level + 1) * 20}px;">`;
                    html += `<span class="ast-key" style="color: #9cdcfe;">"${key}"</span>: `;
                    html += this.createTreeVisualization(data[key], level + 1);
                    if (index < keys.length - 1) html += ',';
                    html += '</div>\n';
                });
                html += `${indent}}</div>`;
            }
        } else {
            const color = typeof data === 'string' ? '#ce9178' : 
                        typeof data === 'number' ? '#b5cea8' : 
                        typeof data === 'boolean' ? '#569cd6' : '#d4d4d4';
            
            const value = typeof data === 'string' ? `"${data}"` : String(data);
            html += `<span style="color: ${color};">${value}</span>`;
        }

        return html;
    }

    // Método mejorado para zoom
    zoomAST(factor) {
        const container = document.getElementById('astVisualization');
        const svg = container.querySelector('svg');
        
        if (svg) {
            this.astZoom *= factor;
            this.astZoom = Math.max(0.1, Math.min(3, this.astZoom));
            svg.style.transform = `scale(${this.astZoom})`;
            
            // Actualizar indicador
            const indicator = container.querySelector('#ast-zoom-indicator');
            if (indicator) {
                indicator.textContent = `${Math.round(this.astZoom * 100)}%`;
            }
            
            console.log(`🔍 Zoom AST: ${Math.round(this.astZoom * 100)}%`);
        }
    }

    // Método mejorado para resetear zoom
    resetASTZoom() {
        const container = document.getElementById('astVisualization');
        const svg = container.querySelector('svg');
        
        if (svg) {
            this.astZoom = 1;
            svg.style.transform = 'scale(1)';
            
            // Resetear scroll
            const wrapper = container.querySelector('.ast-svg-wrapper');
            if (wrapper) {
                wrapper.scrollLeft = 0;
                wrapper.scrollTop = 0;
            }
            
            // Actualizar indicador
            const indicator = container.querySelector('#ast-zoom-indicator');
            if (indicator) {
                indicator.textContent = '100%';
            }
            
            console.log('🔄 Zoom AST reseteado');
        }
    }

    // Método para auto-ajustar el AST al contenedor
    fitASTToContainer() {
        const container = document.getElementById('astVisualization');
        const wrapper = container.querySelector('.ast-svg-wrapper');
        const svg = container.querySelector('svg');
        
        if (!svg || !wrapper) return;
        
        try {
            // Obtener dimensiones
            const containerRect = wrapper.getBoundingClientRect();
            const svgRect = svg.getBoundingClientRect();
            
            // Calcular factor de escala para que quepa
            const scaleX = (containerRect.width - 40) / svgRect.width;
            const scaleY = (containerRect.height - 40) / svgRect.height;
            const scale = Math.min(scaleX, scaleY, 1); // No hacer zoom in, solo zoom out
            
            this.astZoom = scale;
            svg.style.transform = `scale(${scale})`;
            
            // Actualizar indicador
            const indicator = container.querySelector('#ast-zoom-indicator');
            if (indicator) {
                indicator.textContent = `${Math.round(scale * 100)}%`;
            }
            
            console.log(`📐 AST ajustado al contenedor: ${Math.round(scale * 100)}%`);
        } catch (error) {
            console.warn('⚠️ Error ajustando AST:', error);
        }
    }

    expandAllAST() {
        // Por simplicidad, solo actualizamos el mensaje
        console.log('📂 Expandir todos los nodos');
    }

    collapseAllAST() {
        // Por simplicidad, solo actualizamos el mensaje
        console.log('📁 Colapsar todos los nodos');
    }

    downloadAST(format) {
        if (!this.astData) {
            alert('No hay AST para descargar');
            return;
        }

        switch (format) {
            case 'svg':
                this.downloadASTSVG();
                break;
            case 'png':
                this.downloadASTPNG();
                break;
            case 'json':
                this.downloadASTJSON();
                break;
        }
    }


    downloadASTSVG() {
        const container = document.getElementById('astVisualization');
        const svg = container.querySelector('svg');
        if (!svg) {
            alert('No hay SVG para descargar');
            return;
        }
        try {
            // Clona el SVG para evitar modificar el original
            const clonedSvg = svg.cloneNode(true);

            // Opcional: inserta estilos CSS relevantes aquí si los necesitas

            // Serializa el SVG correctamente
            const serializer = new XMLSerializer();
            let source = serializer.serializeToString(clonedSvg);

            // Asegura que el SVG tenga el namespace correcto
            if (!source.match(/^<svg[^>]+xmlns="http:\/\/www\.w3\.org\/2000\/svg"/)) {
                source = source.replace(/^<svg/, '<svg xmlns="http://www.w3.org/2000/svg"');
            }
            // Opcional: agrega xmlns:xlink si usas xlink
            if (!source.match(/^<svg[^>]+"http:\/\/www\.w3\.org\/1999\/xlink"/)) {
                source = source.replace(/^<svg/, '<svg xmlns:xlink="http://www.w3.org/1999/xlink"');
            }

            // Descarga el archivo SVG
            const blob = new Blob([source], { type: 'image/svg+xml;charset=utf-8' });
            this.downloadBlob(blob, 'ast-export.svg');
        } catch (error) {
            alert('Error al exportar el SVG');
        }
    }

    downloadASTJSON() {
        if (!this.astData) {
            alert('No hay AST para descargar');
            return;
        }

        let jsonContent;
        if (typeof this.astData === 'string') {
            jsonContent = JSON.stringify({ ast: this.astData }, null, 2);
        } else {
            jsonContent = JSON.stringify(this.astData, null, 2);
        }

        const blob = new Blob([jsonContent], { type: 'application/json' });
        this.downloadBlob(blob, 'ast.json');
    }

    
    downloadASTPNG() {
        const container = document.getElementById('astVisualization');
        const svg = container.querySelector('svg');
        if (!svg) {
            alert('No hay SVG para convertir a PNG');
            return;
        }
        try {
            // Clonar el SVG para no modificar el original
            const clonedSvg = svg.cloneNode(true);

            // Insertar estilos CSS relevantes dentro del SVG
            let cssText = '';
            // Extrae los estilos embebidos
            document.querySelectorAll('style').forEach(styleNode => {
                cssText += styleNode.innerHTML;
            });
            // Si tienes estilos externos, puedes agregarlos manualmente aquí:
            // cssText += `.node { fill: #fff; stroke: #000; }` // Ejemplo

            if (cssText) {
                const styleElement = document.createElementNS('http://www.w3.org/2000/svg', 'style');
                styleElement.innerHTML = cssText;
                clonedSvg.insertBefore(styleElement, clonedSvg.firstChild);
            }

            // Serializar el SVG clonado
            const serializer = new XMLSerializer();
            let svgString = serializer.serializeToString(clonedSvg);

            // Asegura que el SVG tenga el namespace correcto
            if (!svgString.match(/^<svg[^>]+xmlns="http:\/\/www\.w3\.org\/2000\/svg"/)) {
                svgString = svgString.replace(/^<svg/, '<svg xmlns="http://www.w3.org/2000/svg"');
            }
            if (!svgString.match(/^<svg[^>]+"http:\/\/www\.w3\.org\/1999\/xlink"/)) {
                svgString = svgString.replace(/^<svg/, '<svg xmlns:xlink="http://www.w3.org/1999/xlink"');
            }

            // Crear imagen a partir del SVG serializado
            const img = new window.Image();
            const svgBase64 = 'data:image/svg+xml;base64,' + btoa(unescape(encodeURIComponent(svgString)));

            // Obtener dimensiones del SVG
            const bbox = svg.getBBox();
            const width = Math.ceil(bbox.width);
            const height = Math.ceil(bbox.height);
            const scale = 2; // Para alta resolución

            const canvas = document.createElement('canvas');
            canvas.width = width * scale;
            canvas.height = height * scale;
            const ctx = canvas.getContext('2d');

            img.onload = () => {
                // Fondo blanco
                ctx.fillStyle = '#fff';
                ctx.fillRect(0, 0, canvas.width, canvas.height);

                // Dibuja la imagen SVG escalada
                ctx.setTransform(scale, 0, 0, scale, 0, 0);
                ctx.drawImage(img, -bbox.x, -bbox.y);

                // Descargar como PNG
                canvas.toBlob(blob => {
                    this.downloadBlob(blob, 'ast-export.png');
                });
            };
            img.onerror = () => {
                alert('Error al cargar la imagen SVG para exportar.');
            };
            img.src = svgBase64;
        } catch (error) {
            alert('Error al exportar el AST como PNG');
        }
    }


    // ==================== UTILIDADES ====================
    downloadCSV(data, filename) {
        if (!data.length) return;

        const headers = Object.keys(data[0]);
        const csvContent = [
            headers.join(','),
            ...data.map(row =>
                headers.map(header => {
                    const value = row[header];
                    const stringValue = String(value || '');
                    // Escapar comillas y envolver en comillas si contiene comas
                    return stringValue.includes(',') || stringValue.includes('"')
                        ? `"${stringValue.replace(/"/g, '""')}"`
                        : stringValue;
                }).join(',')
            )
        ].join('\n');

        const blob = new Blob(['\ufeff' + csvContent], { type: 'text/csv;charset=utf-8;' });
        this.downloadBlob(blob, filename);
    }

    downloadExcel(data, filename, sheetName) {
        if (!data.length) return;

        const wb = XLSX.utils.book_new();
        const ws = XLSX.utils.json_to_sheet(data);

        // Ajustar ancho de columnas
        const cols = Object.keys(data[0]).map(key => ({ wch: 20 }));
        ws['!cols'] = cols;

        XLSX.utils.book_append_sheet(wb, ws, sheetName);
        XLSX.writeFile(wb, filename);
    }

    downloadBlob(blob, filename) {
        const url = URL.createObjectURL(blob);
        const a = document.createElement('a');
        a.style.display = 'none';
        a.href = url;
        a.download = filename;
        document.body.appendChild(a);
        a.click();
        document.body.removeChild(a);
        URL.revokeObjectURL(url);
    }

    goToLocation(line, column = 1) {
        if (window.ideController?.editor) {
            window.ideController.editor.goToLine(line, column);
        }
    }

    escapeHtml(text) {
        if (typeof text !== 'string') {
            text = String(text);
        }
        const div = document.createElement('div');
        div.textContent = text;
        return div.innerHTML;
    }

    clearReports() {
        this.currentReports = {
            errors: [],
            symbols: [],
            ast: null
        };

        if (document.getElementById('reportsModal').style.display !== 'none') {
            this.updateAllReports();
        }
    }

    hasErrors() {
        return this.currentReports.errors.length > 0;
    }

    getErrorCount() {
        return this.currentReports.errors.length;
    }

    // Metodos de arm64:
    updateARM64Visualization() {
        const container = document.getElementById('arm64Visualization');
        const executionPanel = document.getElementById('arm64ExecutionPanel');
        
        if (!this.currentReports.arm64Code || this.currentReports.arm64Errors?.length > 0) {
            container.innerHTML = `
                <div class="empty-arm64">
                    <div class="empty-arm64-icon">🔧</div>
                    <p>No hay código ARM64 disponible</p>
                    <small>${this.currentReports.arm64Errors?.length > 0 ? 
                        'Errores en la generación: ' + this.currentReports.arm64Errors.join(', ') : 
                        'Ejecuta código VLan Cherry sin errores para generar ARM64'}</small>
                </div>
            `;
            executionPanel.style.display = 'none';
            return;
        }

        // LIMPIAR TODO EL HTML CORRUPTO DEL BACKEND
        let cleanCode = this.currentReports.arm64Code
            .replace(/"asm-[^"]*">/g, '')           // Remover cualquier "asm-X">
            .replace(/<span[^>]*>/g, '')            // Remover <span>
            .replace(/<\/span>/g, '')               // Remover </span>
            .replace(/&lt;/g, '<')                  // Decodificar HTML entities
            .replace(/&gt;/g, '>')
            .replace(/&amp;/g, '&')
            .replace(/&quot;/g, '"');

        // GUARDAR EL CÓDIGO COMPLETAMENTE LIMPIO
        this.rawARM64Code = cleanCode;
        
        console.log('🔧 Código ARM64 LIMPIO guardado:', this.rawARM64Code);

        // Initialize highlighter
        if (!this.arm64Highlighter) {
            this.arm64Highlighter = new ARM64SyntaxHighlighter();
        }

        // APLICAR HIGHLIGHTING AL CÓDIGO LIMPIO
        const highlightedCode = this.arm64Highlighter.highlight(cleanCode);
        
        // Split into lines for numbering
        const lines = cleanCode.split('\n');
        
        // Generate line numbers
        const lineNumbers = lines.map((_, index) => 
            `<div>${(index + 1).toString().padStart(3, ' ')}</div>`
        ).join('');
        
        // Generate highlighted lines
        const highlightedLines = highlightedCode.split('\n').map(line => 
            `<div>${line || '&nbsp;'}</div>`
        ).join('');
        
        container.innerHTML = `
            <div class="arm64-code-with-lines">
                <div class="arm64-line-numbers">${lineNumbers}</div>
                <div class="arm64-code-content">
                    <div class="arm64-code">${highlightedLines}</div>
                </div>
            </div>
        `;
        
        executionPanel.style.display = 'flex';
        
        console.log('✅ ARM64 con highlighting aplicado correctamente');
    }

    // Agregar este método nuevo también:
    addInstructionTooltips(container) {
        const instructions = container.querySelectorAll('.asm-instruction, .asm-branch');
        
        instructions.forEach(instruction => {
            instruction.addEventListener('mouseenter', (e) => {
                const info = this.arm64Highlighter.getInstructionInfo(e.target.textContent);
                instruction.title = info;
            });
        });
    }

    executeARM64Code() {
        // Use RAW code without HTML tags
        if (!this.rawARM64Code) {
            alert('No hay código ARM64 para ejecutar');
            return;
        }

        const executeBtn = document.getElementById('executeARM64Btn');
        const outputContainer = document.getElementById('arm64ExecutionOutput');
        
        executeBtn.disabled = true;
        executeBtn.textContent = '⏳ Ejecutando...';
        
        console.log('🔧 Ejecutando código ARM64 (RAW):', this.rawARM64Code);
        
        // Llamar al backend para ejecutar el código ARM64 RAW
        this.executeARM64OnBackend(this.rawARM64Code)
            .then(result => {
                this.displayARM64Output(result);
            })
            .catch(error => {
                this.displayARM64Output({
                    success: false,
                    error: error.message || 'Error ejecutando código ARM64'
                });
            })
            .finally(() => {
                executeBtn.disabled = false;
                executeBtn.textContent = '▶️ Ejecutar ARM64';
            });
    }

    // Nuevo método para ejecutar en el backend:
    async executeARM64OnBackend(arm64Code) {
        try {
            const response = await fetch('http://localhost:8080/api/execute-arm64', {
                method: 'POST',
                headers: {
                    'Content-Type': 'application/json'
                },
                body: JSON.stringify({
                    arm64Code: arm64Code
                })
            });

            if (!response.ok) {
                throw new Error(`HTTP ${response.status}: ${response.statusText}`);
            }

            return await response.json();
        } catch (error) {
            console.error('Error ejecutando ARM64 en backend:', error);
            throw error;
        }
    }

    displayARM64Output(result) {
        const outputContainer = document.getElementById('arm64ExecutionOutput');
        
        if (result.success) {
            outputContainer.innerHTML = `
                <div class="arm64-success">
                    <strong>✅ Ejecución exitosa</strong><br>
                    <pre>${this.escapeHtml(result.output)}</pre>
                </div>
            `;
        } else {
            outputContainer.innerHTML = `
                <div class="arm64-error">
                    <strong>❌ Error en ejecución</strong><br>
                    <pre>${this.escapeHtml(result.error)}</pre>
                </div>
            `;
        }
    }

    downloadARM64Code() {
        if (!this.rawARM64Code) {
            alert('No hay código ARM64 para descargar');
            return;
        }

        const blob = new Blob([this.rawARM64Code], { type: 'text/plain' });
        const url = URL.createObjectURL(blob);
        const a = document.createElement('a');
        a.href = url;
        a.download = 'programa.s';
        a.click();
        URL.revokeObjectURL(url);
    }

    clearARM64Output() {
        document.getElementById('arm64ExecutionOutput').innerHTML = '';
    }

}

// ========================================
// FUNCIONES GLOBALES DE UTILIDAD
// ========================================

// Función global para ordenar tablas
function sortTable(tableId, columnIndex) {
    const table = document.getElementById(tableId);
    const tbody = table.querySelector('tbody');
    const rows = Array.from(tbody.querySelectorAll('tr:not(.empty-row)'));

    if (rows.length === 0) return;

    const sortKey = `${tableId}_${columnIndex}`;
    const currentOrder = window.reportsManager?.sortOrder?.[sortKey] || 'asc';
    const newOrder = currentOrder === 'asc' ? 'desc' : 'asc';

    if (window.reportsManager) {
        window.reportsManager.sortOrder[sortKey] = newOrder;
    }

    rows.sort((a, b) => {
        const aValue = a.children[columnIndex]?.textContent?.trim() || '';
        const bValue = b.children[columnIndex]?.textContent?.trim() || '';

        // Intentar convertir a número si es posible
        const aNum = parseFloat(aValue);
        const bNum = parseFloat(bValue);

        let comparison = 0;
        if (!isNaN(aNum) && !isNaN(bNum)) {
            comparison = aNum - bNum;
        } else {
            comparison = aValue.localeCompare(bValue);
        }

        return newOrder === 'asc' ? comparison : -comparison;
    });

    // Actualizar iconos de ordenamiento
    const headers = table.querySelectorAll('th');
    headers.forEach((th, index) => {
        const icon = th.querySelector('.sort-icon');
        if (icon) {
            if (index === columnIndex) {
                icon.textContent = newOrder === 'asc' ? '↑' : '↓';
            } else {
                icon.textContent = '↕️';
            }
        }
    });

    // Reordenar filas
    rows.forEach(row => tbody.appendChild(row));
}

// Función para centrar el AST
function centerAST() {
    const container = document.getElementById('astVisualization');
    const wrapper = container.querySelector('.ast-svg-wrapper');
    
    if (wrapper) {
        wrapper.scrollLeft = (wrapper.scrollWidth - wrapper.clientWidth) / 2;
        wrapper.scrollTop = (wrapper.scrollHeight - wrapper.clientHeight) / 2;
    }
}

// Función para obtener estadísticas del AST
function getASTStats() {
    const container = document.getElementById('astVisualization');
    const svg = container.querySelector('svg');
    
    if (!svg) return null;
    
    const nodes = svg.querySelectorAll('.node');
    const links = svg.querySelectorAll('.link');
    
    return {
        nodes: nodes.length,
        links: links.length,
        depth: calculateASTDepth(svg),
        zoom: window.reportsManager?.astZoom || 1
    };
}

// Función para calcular profundidad del AST
function calculateASTDepth(svg) {
    try {
        const nodes = svg.querySelectorAll('.node');
        let maxDepth = 0;
        
        nodes.forEach(node => {
            const transform = node.getAttribute('transform');
            if (transform) {
                const yMatch = transform.match(/translate\([^,]+,\s*([^)]+)\)/);
                if (yMatch) {
                    const y = parseFloat(yMatch[1]);
                    maxDepth = Math.max(maxDepth, y);
                }
            }
        });
        
        return Math.ceil(maxDepth / 50); // Asumiendo 50px por nivel
    } catch (error) {
        return 0;
    }
}

// Función para exportar AST como imagen de alta resolución
function exportASTAsHighResImage() {
    const container = document.getElementById('astVisualization');
    const svg = container.querySelector('svg');
    
    if (!svg) {
        alert('No hay AST para exportar');
        return;
    }
    
    try {
        // Crear canvas con mayor resolución
        const canvas = document.createElement('canvas');
        const ctx = canvas.getContext('2d');
        const scale = 3; // Factor de escala para alta resolución
        
        // Obtener dimensiones del SVG
        const svgRect = svg.getBoundingClientRect();
        canvas.width = svgRect.width * scale;
        canvas.height = svgRect.height * scale;
        
        // Serializar SVG
        const data = new XMLSerializer().serializeToString(svg);
        const img = new Image();
        
        img.onload = () => {
            // Fondo oscuro
            ctx.fillStyle = '#1e1e1e';
            ctx.fillRect(0, 0, canvas.width, canvas.height);
            
            // Escalar contexto
            ctx.scale(scale, scale);
            ctx.drawImage(img, 0, 0);
            
            // Descargar
            canvas.toBlob((blob) => {
                const url = URL.createObjectURL(blob);
                const a = document.createElement('a');
                a.href = url;
                a.download = 'ast-ultra-high-res.png';
                a.click();
                URL.revokeObjectURL(url);
            });
        };
        
        img.src = 'data:image/svg+xml;base64,' + btoa(unescape(encodeURIComponent(data)));
    } catch (error) {
        console.error('Error exportando AST:', error);
        alert('Error al exportar el AST');
    }
}

// Hacer las funciones disponibles globalmente
window.centerAST = centerAST;
window.getASTStats = getASTStats;
window.exportASTAsHighResImage = exportASTAsHighResImage;